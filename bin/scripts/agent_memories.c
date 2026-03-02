#!/usr/bin/env crispy
#define CRISPY_PARAMS "-std=gnu89 -O2 $(pkg-config --cflags --libs libdex-1 json-glib-1.0 libsoup-3.0 mcp-glib-1.0) -lpq -Wno-unused-function"

/*
 * agent_memories.c — Persistent AI agent memory store
 * Copyright (C) 2026  Zach Podbielniak — AGPLv3
 *
 * C rewrite of agent_memories (Python). Uses crispy for script-style execution.
 * Highly async via libdex fibers, libsoup for HTTP, libpq for PostgreSQL.
 * MCP server via mcp-glib (system library).
 */

#include <libdex.h>
#include <json-glib/json-glib.h>
#include <libsoup/soup.h>
#include <libpq-fe.h>
#include <mcp.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <gio/gio.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <spawn.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>

extern char **environ;

/* ═══════════════════════════════════════════════════════════════════════════
 * Constants
 * ═══════════════════════════════════════════════════════════════════════════ */

static const gchar *CATEGORIES[] = {
    "general", "decision", "preference", "fact", "project",
    "learning", "insight", "todo", "relationship", "technical",
    "workflow", "debug", "research", "config", "personal", NULL
};

static const gchar *IMPORTANCE_LEVELS[] = {
    "low", "normal", "high", "critical", NULL
};

static const gchar *DEFAULT_COLUMNS[] = {
    "id", "category", "importance", "summary", "tags", "created_at", NULL
};

static const gchar *ALL_COLUMNS[] = {
    "id", "content", "summary", "category", "subcategory", "importance",
    "source", "source_context", "conversation_id", "tags", "related_to",
    "is_archived", "is_pinned", "last_accessed_at", "access_count",
    "confidence", "expires_at", "metadata", "created_at", "updated_at", NULL
};

/* ═══════════════════════════════════════════════════════════════════════════
 * Types
 * ═══════════════════════════════════════════════════════════════════════════ */

typedef struct {
    gchar   *id;
    gchar   *content;
    gchar   *summary;
    gchar   *category;
    gchar   *subcategory;
    gchar   *importance;
    gchar   *source;
    gchar   *source_context;
    gchar   *conversation_id;
    gchar  **tags;
    gint     n_tags;
    gchar   *related_to;
    gboolean is_archived;
    gboolean is_pinned;
    gdouble  confidence;
    gchar   *expires_at;
    gchar   *created_at;
    gchar   *updated_at;
    gchar   *last_accessed_at;
    gint     access_count;
    gchar   *metadata;
    gboolean has_embedding;
    gdouble  similarity;
    gdouble  relevance;
} Memory;

typedef struct {
    PGconn      *conn;
    SoupSession *soup;
    gchar       *ollama_url;
    gchar       *embed_model;
    gboolean     no_color;
} AppState;

/* ═══════════════════════════════════════════════════════════════════════════
 * Color helpers
 * ═══════════════════════════════════════════════════════════════════════════ */

static gboolean g_no_color = FALSE;

static gchar *
_c (const gchar *code, const gchar *text)
{
    if (g_no_color)
        return g_strdup (text);
    return g_strdup_printf ("\033[%sm%s\033[0m", code, text);
}

#define _green(t)  _c("32", t)
#define _red(t)    _c("31", t)
#define _yellow(t) _c("33", t)
#define _cyan(t)   _c("36", t)
#define _bold(t)   _c("1", t)
#define _dim(t)    _c("2", t)

/* ═══════════════════════════════════════════════════════════════════════════
 * Memory struct helpers
 * ═══════════════════════════════════════════════════════════════════════════ */

static void
memory_free (Memory *mem)
{
    if (!mem) return;
    g_free (mem->id);
    g_free (mem->content);
    g_free (mem->summary);
    g_free (mem->category);
    g_free (mem->subcategory);
    g_free (mem->importance);
    g_free (mem->source);
    g_free (mem->source_context);
    g_free (mem->conversation_id);
    g_strfreev (mem->tags);
    g_free (mem->related_to);
    g_free (mem->expires_at);
    g_free (mem->created_at);
    g_free (mem->updated_at);
    g_free (mem->last_accessed_at);
    g_free (mem->metadata);
    g_free (mem);
}

G_DEFINE_AUTOPTR_CLEANUP_FUNC (Memory, memory_free)

static gchar *
_pg_val (PGresult *res, gint row, gint col)
{
    if (PQgetisnull (res, row, col))
        return NULL;
    return g_strdup (PQgetvalue (res, row, col));
}

static gchar **
_pg_parse_array (const gchar *pg_array, gint *out_n)
{
    *out_n = 0;
    if (!pg_array || pg_array[0] != '{')
        return NULL;

    /* Parse PostgreSQL array literal {tag1,tag2,"tag with spaces"} */
    GPtrArray *arr = g_ptr_array_new ();
    const gchar *p = pg_array + 1; /* skip { */
    GString *cur = g_string_new (NULL);
    gboolean in_quote = FALSE;

    while (*p && *p != '}') {
        if (*p == '"') {
            in_quote = !in_quote;
            p++;
        } else if (*p == ',' && !in_quote) {
            g_ptr_array_add (arr, g_string_free (cur, FALSE));
            cur = g_string_new (NULL);
            p++;
        } else if (*p == '\\' && in_quote && *(p+1)) {
            p++;
            g_string_append_c (cur, *p++);
        } else {
            g_string_append_c (cur, *p++);
        }
    }
    if (cur->len > 0 || arr->len > 0)
        g_ptr_array_add (arr, g_string_free (cur, FALSE));
    else
        g_string_free (cur, TRUE);

    g_ptr_array_add (arr, NULL);
    *out_n = arr->len - 1;
    return (gchar **)g_ptr_array_free (arr, FALSE);
}

static Memory *
_memory_from_row (PGresult *res, gint row)
{
    Memory *m = g_new0 (Memory, 1);
    gint ncols = PQnfields (res);

    gint c;

    for (c = 0; c < ncols; c++) {
        const gchar *name = PQfname (res, c);
        if (g_strcmp0 (name, "id") == 0)
            m->id = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "content") == 0)
            m->content = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "summary") == 0)
            m->summary = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "category") == 0)
            m->category = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "subcategory") == 0)
            m->subcategory = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "importance") == 0)
            m->importance = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "source") == 0)
            m->source = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "source_context") == 0)
            m->source_context = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "conversation_id") == 0)
            m->conversation_id = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "tags") == 0)
            m->tags = _pg_parse_array (PQgetvalue (res, row, c), &m->n_tags);
        else if (g_strcmp0 (name, "related_to") == 0)
            m->related_to = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "is_archived") == 0)
            m->is_archived = g_strcmp0 (PQgetvalue (res, row, c), "t") == 0;
        else if (g_strcmp0 (name, "is_pinned") == 0)
            m->is_pinned = g_strcmp0 (PQgetvalue (res, row, c), "t") == 0;
        else if (g_strcmp0 (name, "confidence") == 0)
            m->confidence = PQgetisnull (res, row, c) ? 1.0 : g_ascii_strtod (PQgetvalue (res, row, c), NULL);
        else if (g_strcmp0 (name, "expires_at") == 0)
            m->expires_at = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "created_at") == 0)
            m->created_at = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "updated_at") == 0)
            m->updated_at = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "last_accessed_at") == 0)
            m->last_accessed_at = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "access_count") == 0)
            m->access_count = PQgetisnull (res, row, c) ? 0 : atoi (PQgetvalue (res, row, c));
        else if (g_strcmp0 (name, "metadata") == 0)
            m->metadata = _pg_val (res, row, c);
        else if (g_strcmp0 (name, "embedding") == 0)
            m->has_embedding = !PQgetisnull (res, row, c);
        else if (g_strcmp0 (name, "similarity") == 0)
            m->similarity = PQgetisnull (res, row, c) ? 0.0 : g_ascii_strtod (PQgetvalue (res, row, c), NULL);
        else if (g_strcmp0 (name, "relevance") == 0)
            m->relevance = PQgetisnull (res, row, c) ? 0.0 : g_ascii_strtod (PQgetvalue (res, row, c), NULL);
    }

    return m;
}

static GPtrArray *
_memories_from_result (PGresult *res)
{
    GPtrArray *arr = g_ptr_array_new_with_free_func ((GDestroyNotify)memory_free);
    gint nrows = PQntuples (res);
    gint i;
    for (i = 0; i < nrows; i++)
        g_ptr_array_add (arr, _memory_from_row (res, i));
    return arr;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Database connection
 * ═══════════════════════════════════════════════════════════════════════════ */

static PGconn *
db_connect (void)
{
    const gchar *host = g_getenv ("AGENT_MEMORIES_DB_HOST");
    const gchar *port = g_getenv ("AGENT_MEMORIES_DB_PORT");
    const gchar *dbname = g_getenv ("AGENT_MEMORIES_DB_NAME");
    const gchar *user = g_getenv ("AGENT_MEMORIES_DB_USER");
    const gchar *pass = g_getenv ("AGENT_MEMORIES_DB_PASSWORD");

    g_autofree gchar *conninfo = g_strdup_printf (
        "host=%s port=%s dbname=%s user=%s password=%s",
        host   ? host   : "127.0.0.1",
        port   ? port   : "5432",
        dbname ? dbname : "agent_memories",
        user   ? user   : "postgres",
        pass   ? pass   : ""
    );

    PGconn *conn = PQconnectdb (conninfo);
    if (PQstatus (conn) != CONNECTION_OK) {
        g_autofree gchar *msg = _red ("Database connection failed");
        g_printerr ("%s: %s\n", msg, PQerrorMessage (conn));
        g_printerr ("Run 'local_postgres init-agent-memories' to initialize the database.\n");
        PQfinish (conn);
        return NULL;
    }
    return conn;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * UUID resolution
 * ═══════════════════════════════════════════════════════════════════════════ */

static gchar *
db_resolve_uuid (PGconn *conn, const gchar *prefix)
{
    g_autofree gchar *pattern = g_strdup_printf ("%s%%", prefix);
    const gchar *params[] = { pattern };
    PGresult *res = PQexecParams (conn,
        "SELECT id FROM memories WHERE id::text LIKE $1 LIMIT 3",
        1, NULL, params, NULL, NULL, 0);

    gchar *result = NULL;
    if (PQresultStatus (res) == PGRES_TUPLES_OK) {
        gint n = PQntuples (res);
        if (n == 1) {
            result = g_strdup (PQgetvalue (res, 0, 0));
        } else if (n > 1) {
            g_autofree gchar *msg = _yellow ("Ambiguous ID prefix");
            g_printerr ("%s '%s' — matches %d memories\n", msg, prefix, n);
        }
    }
    PQclear (res);
    return result;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Expiry parsing
 * ═══════════════════════════════════════════════════════════════════════════ */

static gchar *
parse_expiry (const gchar *value)
{
    if (!value) return NULL;

    gint amount;
    gchar unit;
    if (sscanf (value, "%d%c", &amount, &unit) == 2) {
        GDateTime *now = g_date_time_new_now_utc ();
        GDateTime *exp = NULL;
        switch (unit) {
            case 'd': exp = g_date_time_add_days (now, amount); break;
            case 'w': exp = g_date_time_add_weeks (now, amount); break;
            case 'm': exp = g_date_time_add_months (now, amount); break;
            case 'y': exp = g_date_time_add_years (now, amount); break;
            default: break;
        }
        g_date_time_unref (now);
        if (exp) {
            gchar *iso = g_date_time_format_iso8601 (exp);
            g_date_time_unref (exp);
            return iso;
        }
    }

    /* Try ISO date: validate by parsing */
    GDateTime *dt = g_date_time_new_from_iso8601 (value, NULL);
    if (dt) {
        gchar *iso = g_date_time_format_iso8601 (dt);
        g_date_time_unref (dt);
        return iso;
    }

    /* Try plain date YYYY-MM-DD */
    gint y, m, d;
    if (sscanf (value, "%d-%d-%d", &y, &m, &d) == 3) {
        dt = g_date_time_new_utc (y, m, d, 0, 0, 0);
        if (dt) {
            gchar *iso = g_date_time_format_iso8601 (dt);
            g_date_time_unref (dt);
            return iso;
        }
    }

    g_autofree gchar *msg = _red ("Invalid expiry format");
    g_printerr ("%s: '%s'\n", msg, value);
    g_printerr ("Use relative (7d, 2w, 3m, 1y) or ISO date (2026-03-01)\n");
    return NULL;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Ollama embedding generation (async via libsoup)
 * ═══════════════════════════════════════════════════════════════════════════ */

static gchar *
generate_embedding_sync (SoupSession *soup, const gchar *ollama_url,
                         const gchar *model, const gchar *text)
{
    g_autofree gchar *url = g_strdup_printf ("%s/api/embed", ollama_url);
    JsonBuilder *builder = json_builder_new ();
    json_builder_begin_object (builder);
    json_builder_set_member_name (builder, "model");
    json_builder_add_string_value (builder, model);
    json_builder_set_member_name (builder, "input");
    json_builder_add_string_value (builder, text);
    json_builder_end_object (builder);

    JsonGenerator *gen = json_generator_new ();
    json_generator_set_root (gen, json_builder_get_root (builder));
    g_autofree gchar *body = json_generator_to_data (gen, NULL);
    g_object_unref (gen);
    g_object_unref (builder);

    GBytes *body_bytes = g_bytes_new (body, strlen (body));
    SoupMessage *msg = soup_message_new ("POST", url);
    if (!msg) {
        g_bytes_unref (body_bytes);
        return NULL;
    }
    soup_message_set_request_body_from_bytes (msg, "application/json", body_bytes);
    g_bytes_unref (body_bytes);

    GError *error = NULL;
    GBytes *resp_bytes = soup_session_send_and_read (soup, msg, NULL, &error);
    g_object_unref (msg);

    if (error) {
        g_error_free (error);
        return NULL;
    }

    gsize resp_len;
    const gchar *resp_data = g_bytes_get_data (resp_bytes, &resp_len);

    JsonParser *parser = json_parser_new ();
    gchar *result = NULL;
    if (json_parser_load_from_data (parser, resp_data, resp_len, NULL)) {
        JsonNode *root = json_parser_get_root (parser);
        JsonObject *obj = json_node_get_object (root);
        if (json_object_has_member (obj, "embeddings")) {
            JsonArray *embeddings = json_object_get_array_member (obj, "embeddings");
            if (json_array_get_length (embeddings) > 0) {
                JsonArray *vec = json_array_get_array_element (embeddings, 0);
                guint vlen = json_array_get_length (vec);
                GString *s = g_string_new ("[");
                guint i;
                for (i = 0; i < vlen; i++) {
                    if (i > 0) g_string_append_c (s, ',');
                    g_string_append_printf (s, "%.8g",
                        json_array_get_double_element (vec, i));
                }
                g_string_append_c (s, ']');
                result = g_string_free (s, FALSE);
            }
        }
    }
    g_object_unref (parser);
    g_bytes_unref (resp_bytes);
    return result;
}

typedef struct {
    SoupSession *soup;
    gchar       *ollama_url;
    gchar       *model;
    gchar       *text;
    gchar       *result;
} EmbedData;

static DexFuture *
_embed_fiber (gpointer data)
{
    EmbedData *ed = data;
    ed->result = generate_embedding_sync (ed->soup, ed->ollama_url,
                                          ed->model, ed->text);
    return dex_future_new_for_boolean (TRUE);
}

static gchar *
generate_embedding_async (DexScheduler *thread_pool, SoupSession *soup,
                          const gchar *ollama_url, const gchar *model,
                          const gchar *text)
{
    EmbedData ed = {
        .soup = soup,
        .ollama_url = (gchar *)ollama_url,
        .model = (gchar *)model,
        .text = (gchar *)text,
        .result = NULL,
    };

    DexFuture *f = dex_scheduler_spawn (thread_pool, 0, _embed_fiber, &ed, NULL);
    GError *error = NULL;
    dex_await (dex_ref (f), &error);
    dex_unref (f);
    g_clear_error (&error);
    return ed.result;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Core database operations
 * ═══════════════════════════════════════════════════════════════════════════ */

static gchar *
db_add_memory (AppState *app, const gchar *content, const gchar *summary,
               const gchar *category, const gchar *subcategory,
               const gchar *importance, const gchar *source,
               const gchar *source_context, const gchar *conversation_id,
               gchar **tags, gint n_tags, const gchar *related_to,
               gboolean pinned, const gchar *metadata_json,
               gdouble confidence, const gchar *expires_at)
{
    g_autofree gchar *auto_summary = NULL;
    if (!summary || !*summary) {
        auto_summary = g_strndup (content, 200);
        /* Truncate at first newline */
        gchar *nl = strchr (auto_summary, '\n');
        if (nl) *nl = '\0';
        summary = auto_summary;
    }

    g_autofree gchar *resolved_related = NULL;
    if (related_to && *related_to) {
        resolved_related = db_resolve_uuid (app->conn, related_to);
        if (!resolved_related) {
            g_autofree gchar *msg = _yellow ("Warning: related memory not found, ignoring");
            g_printerr ("%s\n", msg);
        }
    }

    /* Build tags array literal */
    GString *tags_lit = g_string_new ("{");
    gint i;
    for (i = 0; i < n_tags; i++) {
        if (i > 0) g_string_append_c (tags_lit, ',');
        g_string_append_printf (tags_lit, "\"%s\"", tags[i]);
    }
    g_string_append_c (tags_lit, '}');
    g_autofree gchar *tags_str = g_string_free (tags_lit, FALSE);

    g_autofree gchar *conf_str = g_strdup_printf ("%.2f", confidence);
    const gchar *meta = metadata_json ? metadata_json : "{}";
    const gchar *pin_str = pinned ? "true" : "false";

    const gchar *params[14];
    params[0]  = content;
    params[1]  = summary;
    params[2]  = category   ? category   : "general";
    params[3]  = subcategory;
    params[4]  = importance ? importance : "normal";
    params[5]  = source;
    params[6]  = source_context;
    params[7]  = conversation_id;
    params[8]  = tags_str;
    params[9]  = resolved_related;
    params[10] = pin_str;
    params[11] = meta;
    params[12] = conf_str;
    params[13] = expires_at;

    PGresult *res = PQexecParams (app->conn,
        "INSERT INTO memories ("
        "  content, summary, category, subcategory, importance,"
        "  source, source_context, conversation_id,"
        "  tags, related_to, is_pinned, metadata,"
        "  confidence, expires_at"
        ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9,"
        "  $10::uuid, $11::boolean, $12::jsonb, $13::float, $14::timestamptz)"
        " RETURNING id",
        14, NULL, params, NULL, NULL, 0);

    gchar *memory_id = NULL;
    if (PQresultStatus (res) == PGRES_TUPLES_OK && PQntuples (res) > 0)
        memory_id = g_strdup (PQgetvalue (res, 0, 0));
    else
        g_printerr ("INSERT failed: %s\n", PQerrorMessage (app->conn));
    PQclear (res);

    /* Generate embedding (non-fatal) */
    if (memory_id) {
        g_autofree gchar *embed_text = g_strdup_printf ("%s %s", summary, content);
        g_autofree gchar *embedding = generate_embedding_sync (
            app->soup, app->ollama_url, app->embed_model, embed_text);
        if (embedding) {
            const gchar *up_params[] = { embedding, memory_id };
            PGresult *up = PQexecParams (app->conn,
                "UPDATE memories SET embedding = $1 WHERE id = $2::uuid",
                2, NULL, up_params, NULL, NULL, 0);
            PQclear (up);
        }
    }

    return memory_id;
}

static Memory *
db_get_memory (PGconn *conn, const gchar *id_prefix)
{
    g_autofree gchar *full_id = db_resolve_uuid (conn, id_prefix);
    if (!full_id) return NULL;

    /* Touch access tracking */
    const gchar *touch_params[] = { full_id };
    PGresult *touch = PQexecParams (conn,
        "UPDATE memories SET access_count = access_count + 1, "
        "last_accessed_at = CURRENT_TIMESTAMP WHERE id = $1::uuid",
        1, NULL, touch_params, NULL, NULL, 0);
    PQclear (touch);

    PGresult *res = PQexecParams (conn,
        "SELECT * FROM memories WHERE id = $1::uuid",
        1, NULL, touch_params, NULL, NULL, 0);

    Memory *mem = NULL;
    if (PQresultStatus (res) == PGRES_TUPLES_OK && PQntuples (res) > 0)
        mem = _memory_from_row (res, 0);
    PQclear (res);
    return mem;
}

static gboolean
db_update_memory (AppState *app, const gchar *id_prefix,
                  GHashTable *fields)
{
    g_autofree gchar *full_id = db_resolve_uuid (app->conn, id_prefix);
    if (!full_id) return FALSE;

    GHashTableIter iter;
    gpointer key, val;
    GString *sets = g_string_new (NULL);
    GPtrArray *params = g_ptr_array_new ();
    g_ptr_array_add (params, full_id);
    gint idx = 2;

    g_hash_table_iter_init (&iter, fields);
    while (g_hash_table_iter_next (&iter, &key, &val)) {
        const gchar *k = key;
        const gchar *v = val;

        if (sets->len > 0) g_string_append (sets, ", ");

        if (g_strcmp0 (k, "tags") == 0)
            g_string_append_printf (sets, "tags = $%d::text[]", idx);
        else if (g_strcmp0 (k, "is_pinned") == 0 || g_strcmp0 (k, "is_archived") == 0)
            g_string_append_printf (sets, "%s = $%d::boolean", k, idx);
        else if (g_strcmp0 (k, "confidence") == 0)
            g_string_append_printf (sets, "confidence = $%d::float", idx);
        else if (g_strcmp0 (k, "metadata") == 0)
            g_string_append_printf (sets, "metadata = $%d::jsonb", idx);
        else if (g_strcmp0 (k, "expires_at") == 0)
            g_string_append_printf (sets, "expires_at = $%d::timestamptz", idx);
        else
            g_string_append_printf (sets, "%s = $%d", k, idx);

        g_ptr_array_add (params, (gpointer)v);
        idx++;
    }

    if (sets->len == 0) {
        g_string_free (sets, TRUE);
        g_ptr_array_free (params, TRUE);
        return FALSE;
    }

    g_autofree gchar *sql = g_strdup_printf (
        "UPDATE memories SET %s, updated_at = CURRENT_TIMESTAMP WHERE id = $1::uuid",
        sets->str);
    g_string_free (sets, TRUE);

    const gchar **p = g_new0 (const gchar *, params->len);
    guint i;
    for (i = 0; i < params->len; i++)
        p[i] = g_ptr_array_index (params, i);

    PGresult *res = PQexecParams (app->conn, sql,
        params->len, NULL, p, NULL, NULL, 0);
    gboolean ok = (PQresultStatus (res) == PGRES_COMMAND_OK);
    PQclear (res);
    g_free (p);
    g_ptr_array_free (params, TRUE);

    /* Regenerate embedding if content or summary changed */
    if (ok && (g_hash_table_contains (fields, "content") ||
               g_hash_table_contains (fields, "summary"))) {
        const gchar *q_params[] = { full_id };
        PGresult *qres = PQexecParams (app->conn,
            "SELECT content, summary FROM memories WHERE id = $1::uuid",
            1, NULL, q_params, NULL, NULL, 0);
        if (PQresultStatus (qres) == PGRES_TUPLES_OK && PQntuples (qres) > 0) {
            const gchar *c = PQgetvalue (qres, 0, 0);
            const gchar *s = PQgetvalue (qres, 0, 1);
            g_autofree gchar *embed_text = g_strdup_printf ("%s %s", s ? s : "", c);
            g_autofree gchar *embedding = generate_embedding_sync (
                app->soup, app->ollama_url, app->embed_model, embed_text);
            if (embedding) {
                const gchar *up[] = { embedding, full_id };
                PGresult *u = PQexecParams (app->conn,
                    "UPDATE memories SET embedding = $1 WHERE id = $2::uuid",
                    2, NULL, up, NULL, NULL, 0);
                PQclear (u);
            }
        }
        PQclear (qres);
    }

    return ok;
}

static gboolean
db_delete_memory (PGconn *conn, const gchar *id_prefix)
{
    g_autofree gchar *full_id = db_resolve_uuid (conn, id_prefix);
    if (!full_id) return FALSE;

    const gchar *params[] = { full_id };
    PGresult *res = PQexecParams (conn,
        "UPDATE memories SET related_to = NULL WHERE related_to = $1::uuid",
        1, NULL, params, NULL, NULL, 0);
    PQclear (res);

    res = PQexecParams (conn,
        "DELETE FROM memories WHERE id = $1::uuid",
        1, NULL, params, NULL, NULL, 0);
    gboolean ok = g_strcmp0 (PQcmdTuples (res), "1") == 0;
    PQclear (res);
    return ok;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * List and search
 * ═══════════════════════════════════════════════════════════════════════════ */

static GPtrArray *
db_list_memories (PGconn *conn, const gchar *category, const gchar *subcategory,
                  const gchar *importance, gchar **filter_tags, gint n_filter_tags,
                  const gchar *source, gint pinned_filter,
                  gboolean include_archived, gboolean only_archived,
                  gint limit, gint offset, const gchar *order_by,
                  gboolean descending)
{
    GString *sql = g_string_new ("SELECT * FROM memories WHERE ");
    GPtrArray *plist = g_ptr_array_new ();
    gint idx = 1;

    if (only_archived)
        g_string_append (sql, "is_archived = TRUE");
    else if (!include_archived)
        g_string_append (sql, "is_archived = FALSE");
    else
        g_string_append (sql, "TRUE");

    if (category) {
        g_string_append_printf (sql, " AND category = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)category);
    }
    if (subcategory) {
        g_string_append_printf (sql, " AND subcategory = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)subcategory);
    }
    if (importance) {
        g_string_append_printf (sql, " AND importance = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)importance);
    }
    if (filter_tags && n_filter_tags > 0) {
        /* Build array literal for @> operator */
        GString *tag_lit = g_string_new ("{");
        gint i;
        for (i = 0; i < n_filter_tags; i++) {
            if (i > 0) g_string_append_c (tag_lit, ',');
            g_string_append_printf (tag_lit, "\"%s\"", filter_tags[i]);
        }
        g_string_append_c (tag_lit, '}');
        g_string_append_printf (sql, " AND tags @> $%d::text[]", idx++);
        g_ptr_array_add (plist, g_string_free (tag_lit, FALSE));
    }
    if (source) {
        g_string_append_printf (sql, " AND source = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)source);
    }
    if (pinned_filter == 1) {
        g_string_append_printf (sql, " AND is_pinned = $%d::boolean", idx++);
        g_ptr_array_add (plist, (gpointer)"true");
    }

    g_string_append (sql, " AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)");

    /* Validate order_by */
    const gchar *safe_order = "created_at";
    gint i;
    for (i = 0; ALL_COLUMNS[i]; i++) {
        if (g_strcmp0 (order_by, ALL_COLUMNS[i]) == 0) {
            safe_order = order_by;
            break;
        }
    }

    gchar *offset_str = g_strdup_printf ("%d", offset);
    gchar *limit_str = NULL;
    if (limit < 0) {
        g_string_append_printf (sql, " ORDER BY %s %s LIMIT ALL OFFSET $%d",
            safe_order, descending ? "DESC" : "ASC", idx);
    } else {
        limit_str = g_strdup_printf ("%d", limit);
        g_string_append_printf (sql, " ORDER BY %s %s LIMIT $%d OFFSET $%d",
            safe_order, descending ? "DESC" : "ASC", idx, idx + 1);
        g_ptr_array_add (plist, limit_str);
    }
    g_ptr_array_add (plist, offset_str);

    const gchar **p = g_new0 (const gchar *, plist->len);
    guint j;
    for (j = 0; j < plist->len; j++)
        p[j] = g_ptr_array_index (plist, j);

    PGresult *res = PQexecParams (conn, sql->str,
        plist->len, NULL, p, NULL, NULL, 0);

    GPtrArray *result = NULL;
    if (PQresultStatus (res) == PGRES_TUPLES_OK)
        result = _memories_from_result (res);
    else
        result = g_ptr_array_new_with_free_func ((GDestroyNotify)memory_free);

    PQclear (res);
    g_free (p);
    g_free (limit_str);
    g_free (offset_str);
    g_string_free (sql, TRUE);
    g_ptr_array_free (plist, TRUE);
    return result;
}

static GPtrArray *
db_search_fuzzy (PGconn *conn, const gchar *query, const gchar *category,
                 gboolean include_archived, gint limit)
{
    GPtrArray *plist = g_ptr_array_new ();
    gint idx = 1;

    g_autofree gchar *like_pattern = g_strdup_printf ("%%%s%%", query);
    GString *sql = g_string_new (
        "SELECT *, GREATEST("
        "  COALESCE(similarity(summary, $2), 0) * 2.0,"
        "  COALESCE(similarity(content, $2), 0),"
        "  COALESCE(similarity(array_to_string(tags, ' '), $2), 0) * 1.5"
        ") as relevance FROM memories WHERE "
        "(content ILIKE $1 OR summary ILIKE $1 "
        "OR array_to_string(tags, ' ') ILIKE $1)"
    );
    g_ptr_array_add (plist, (gpointer)like_pattern); idx++;
    g_ptr_array_add (plist, (gpointer)query); idx++;

    if (!include_archived)
        g_string_append (sql, " AND is_archived = FALSE");

    if (category) {
        g_string_append_printf (sql, " AND category = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)category);
    }

    g_string_append (sql, " AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)");

    g_autofree gchar *limit_str = g_strdup_printf ("%d", limit);
    g_string_append_printf (sql,
        " ORDER BY is_pinned DESC, "
        "(GREATEST(COALESCE(similarity(summary, $2), 0) * 2.0,"
        "  COALESCE(similarity(content, $2), 0),"
        "  COALESCE(similarity(array_to_string(tags, ' '), $2), 0) * 1.5"
        ") * COALESCE(confidence, 1.0)) DESC, "
        "created_at DESC LIMIT $%d", idx);
    g_ptr_array_add (plist, limit_str);

    const gchar **p = g_new0 (const gchar *, plist->len);
    guint i;
    for (i = 0; i < plist->len; i++)
        p[i] = g_ptr_array_index (plist, i);

    PGresult *res = PQexecParams (conn, sql->str,
        plist->len, NULL, p, NULL, NULL, 0);

    GPtrArray *result = NULL;
    if (PQresultStatus (res) == PGRES_TUPLES_OK)
        result = _memories_from_result (res);
    else
        result = g_ptr_array_new_with_free_func ((GDestroyNotify)memory_free);

    PQclear (res);
    g_free (p);
    g_string_free (sql, TRUE);
    g_ptr_array_free (plist, TRUE);
    return result;
}

static GPtrArray *
db_search_exact (PGconn *conn, const gchar *query, const gchar *category,
                 gboolean include_archived, gint limit)
{
    GPtrArray *plist = g_ptr_array_new ();
    gint idx = 1;

    GString *sql = g_string_new (
        "SELECT *, ts_rank_cd(search_vector, websearch_to_tsquery('english', $1))"
        " as relevance FROM memories"
        " WHERE search_vector @@ websearch_to_tsquery('english', $1)"
    );
    g_ptr_array_add (plist, (gpointer)query); idx++;

    if (!include_archived)
        g_string_append (sql, " AND is_archived = FALSE");

    if (category) {
        g_string_append_printf (sql, " AND category = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)category);
    }

    g_string_append (sql, " AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)");

    g_autofree gchar *limit_str = g_strdup_printf ("%d", limit);
    g_string_append_printf (sql,
        " ORDER BY is_pinned DESC, "
        "(ts_rank_cd(search_vector, websearch_to_tsquery('english', $1))"
        " * COALESCE(confidence, 1.0)) DESC, created_at DESC LIMIT $%d", idx);
    g_ptr_array_add (plist, limit_str);

    const gchar **p = g_new0 (const gchar *, plist->len);
    guint i;
    for (i = 0; i < plist->len; i++)
        p[i] = g_ptr_array_index (plist, i);

    PGresult *res = PQexecParams (conn, sql->str,
        plist->len, NULL, p, NULL, NULL, 0);

    GPtrArray *result = NULL;
    if (PQresultStatus (res) == PGRES_TUPLES_OK)
        result = _memories_from_result (res);
    else
        result = g_ptr_array_new_with_free_func ((GDestroyNotify)memory_free);

    PQclear (res);
    g_free (p);
    g_string_free (sql, TRUE);
    g_ptr_array_free (plist, TRUE);
    return result;
}

static GPtrArray *
db_search_semantic (AppState *app, const gchar *query, const gchar *category,
                    gboolean include_archived, gint limit)
{
    g_autofree gchar *embedding = generate_embedding_sync (
        app->soup, app->ollama_url, app->embed_model, query);

    if (!embedding) {
        g_autofree gchar *msg = _yellow ("Warning: ollama unavailable, falling back to fuzzy search");
        g_printerr ("%s\n", msg);
        return db_search_fuzzy (app->conn, query, category, include_archived, limit);
    }

    GPtrArray *plist = g_ptr_array_new ();
    gint idx = 1;

    GString *sql = g_string_new (
        "SELECT *, (1 - (embedding <=> $1::vector)) * COALESCE(confidence, 1.0)"
        " AS similarity FROM memories WHERE embedding IS NOT NULL"
    );
    g_ptr_array_add (plist, (gpointer)embedding); idx++;

    if (!include_archived)
        g_string_append (sql, " AND is_archived = FALSE");

    if (category) {
        g_string_append_printf (sql, " AND category = $%d", idx++);
        g_ptr_array_add (plist, (gpointer)category);
    }

    g_string_append (sql, " AND (expires_at IS NULL OR expires_at > CURRENT_TIMESTAMP)");

    g_autofree gchar *limit_str = g_strdup_printf ("%d", limit);
    g_string_append_printf (sql, " ORDER BY similarity DESC LIMIT $%d", idx);
    g_ptr_array_add (plist, limit_str);

    const gchar **p = g_new0 (const gchar *, plist->len);
    guint i;
    for (i = 0; i < plist->len; i++)
        p[i] = g_ptr_array_index (plist, i);

    PGresult *res = PQexecParams (app->conn, sql->str,
        plist->len, NULL, p, NULL, NULL, 0);

    GPtrArray *result = g_ptr_array_new_with_free_func ((GDestroyNotify)memory_free);
    if (PQresultStatus (res) == PGRES_TUPLES_OK) {
        gint n = PQntuples (res);
        gint i;
        for (i = 0; i < n; i++) {
            Memory *m = _memory_from_row (res, i);
            if (m->similarity >= 0.5)
                g_ptr_array_add (result, m);
            else
                memory_free (m);
        }
    }

    PQclear (res);
    g_free (p);
    g_string_free (sql, TRUE);
    g_ptr_array_free (plist, TRUE);
    return result;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Stats
 * ═══════════════════════════════════════════════════════════════════════════ */

static JsonNode *
db_get_stats (PGconn *conn)
{
    JsonBuilder *b = json_builder_new ();
    json_builder_begin_object (b);

    PGresult *res;

    res = PQexec (conn, "SELECT COUNT(*) FROM memories");
    gint64 total = (PQresultStatus (res) == PGRES_TUPLES_OK) ? g_ascii_strtoll (PQgetvalue (res, 0, 0), NULL, 10) : 0;
    PQclear (res);

    res = PQexec (conn, "SELECT COUNT(*) FROM memories WHERE is_archived = TRUE");
    gint64 archived = (PQresultStatus (res) == PGRES_TUPLES_OK) ? g_ascii_strtoll (PQgetvalue (res, 0, 0), NULL, 10) : 0;
    PQclear (res);

    res = PQexec (conn, "SELECT COUNT(*) FROM memories WHERE is_pinned = TRUE");
    gint64 pinned = (PQresultStatus (res) == PGRES_TUPLES_OK) ? g_ascii_strtoll (PQgetvalue (res, 0, 0), NULL, 10) : 0;
    PQclear (res);

    json_builder_set_member_name (b, "total");
    json_builder_add_int_value (b, total);
    json_builder_set_member_name (b, "active");
    json_builder_add_int_value (b, total - archived);
    json_builder_set_member_name (b, "archived");
    json_builder_add_int_value (b, archived);
    json_builder_set_member_name (b, "pinned");
    json_builder_add_int_value (b, pinned);

    /* by_category */
    json_builder_set_member_name (b, "by_category");
    json_builder_begin_array (b);
    res = PQexec (conn,
        "SELECT category, COUNT(*) as count FROM memories "
        "WHERE is_archived = FALSE GROUP BY category ORDER BY count DESC");
    if (PQresultStatus (res) == PGRES_TUPLES_OK) {
        gint i;
        for (i = 0; i < PQntuples (res); i++) {
            json_builder_begin_object (b);
            json_builder_set_member_name (b, "category");
            json_builder_add_string_value (b, PQgetvalue (res, i, 0));
            json_builder_set_member_name (b, "count");
            json_builder_add_int_value (b, g_ascii_strtoll (PQgetvalue (res, i, 1), NULL, 10));
            json_builder_end_object (b);
        }
    }
    PQclear (res);
    json_builder_end_array (b);

    /* by_importance */
    json_builder_set_member_name (b, "by_importance");
    json_builder_begin_array (b);
    res = PQexec (conn,
        "SELECT importance, COUNT(*) as count FROM memories "
        "WHERE is_archived = FALSE GROUP BY importance ORDER BY count DESC");
    if (PQresultStatus (res) == PGRES_TUPLES_OK) {
        gint i;
        for (i = 0; i < PQntuples (res); i++) {
            json_builder_begin_object (b);
            json_builder_set_member_name (b, "importance");
            json_builder_add_string_value (b, PQgetvalue (res, i, 0));
            json_builder_set_member_name (b, "count");
            json_builder_add_int_value (b, g_ascii_strtoll (PQgetvalue (res, i, 1), NULL, 10));
            json_builder_end_object (b);
        }
    }
    PQclear (res);
    json_builder_end_array (b);

    /* top_tags */
    json_builder_set_member_name (b, "top_tags");
    json_builder_begin_array (b);
    res = PQexec (conn,
        "SELECT unnest(tags) as tag, COUNT(*) as count FROM memories "
        "WHERE is_archived = FALSE GROUP BY tag ORDER BY count DESC LIMIT 20");
    if (PQresultStatus (res) == PGRES_TUPLES_OK) {
        gint i;
        for (i = 0; i < PQntuples (res); i++) {
            json_builder_begin_object (b);
            json_builder_set_member_name (b, "tag");
            json_builder_add_string_value (b, PQgetvalue (res, i, 0));
            json_builder_set_member_name (b, "count");
            json_builder_add_int_value (b, g_ascii_strtoll (PQgetvalue (res, i, 1), NULL, 10));
            json_builder_end_object (b);
        }
    }
    PQclear (res);
    json_builder_end_array (b);

    /* most_accessed */
    json_builder_set_member_name (b, "most_accessed");
    json_builder_begin_array (b);
    res = PQexec (conn,
        "SELECT id, summary, access_count FROM memories "
        "WHERE access_count > 0 ORDER BY access_count DESC LIMIT 10");
    if (PQresultStatus (res) == PGRES_TUPLES_OK) {
        gint i;
        for (i = 0; i < PQntuples (res); i++) {
            json_builder_begin_object (b);
            json_builder_set_member_name (b, "id");
            json_builder_add_string_value (b, PQgetvalue (res, i, 0));
            json_builder_set_member_name (b, "summary");
            json_builder_add_string_value (b, PQgetvalue (res, i, 1));
            json_builder_set_member_name (b, "access_count");
            json_builder_add_int_value (b, g_ascii_strtoll (PQgetvalue (res, i, 2), NULL, 10));
            json_builder_end_object (b);
        }
    }
    PQclear (res);
    json_builder_end_array (b);

    json_builder_end_object (b);
    JsonNode *node = json_builder_get_root (b);
    g_object_unref (b);
    return node;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * JSON serialization helpers
 * ═══════════════════════════════════════════════════════════════════════════ */

static JsonNode *
memory_to_json_node (Memory *m)
{
    JsonBuilder *b = json_builder_new ();
    json_builder_begin_object (b);

    json_builder_set_member_name (b, "id");
    json_builder_add_string_value (b, m->id ? m->id : "");
    json_builder_set_member_name (b, "content");
    json_builder_add_string_value (b, m->content ? m->content : "");
    json_builder_set_member_name (b, "summary");
    json_builder_add_string_value (b, m->summary ? m->summary : "");
    json_builder_set_member_name (b, "category");
    json_builder_add_string_value (b, m->category ? m->category : "general");
    json_builder_set_member_name (b, "subcategory");
    if (m->subcategory) json_builder_add_string_value (b, m->subcategory);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "importance");
    json_builder_add_string_value (b, m->importance ? m->importance : "normal");
    json_builder_set_member_name (b, "source");
    if (m->source) json_builder_add_string_value (b, m->source);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "source_context");
    if (m->source_context) json_builder_add_string_value (b, m->source_context);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "conversation_id");
    if (m->conversation_id) json_builder_add_string_value (b, m->conversation_id);
    else json_builder_add_null_value (b);

    json_builder_set_member_name (b, "tags");
    json_builder_begin_array (b);
    if (m->tags) {
        gint i;
        for (i = 0; i < m->n_tags; i++)
            json_builder_add_string_value (b, m->tags[i]);
    }
    json_builder_end_array (b);

    json_builder_set_member_name (b, "related_to");
    if (m->related_to) json_builder_add_string_value (b, m->related_to);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "is_archived");
    json_builder_add_boolean_value (b, m->is_archived);
    json_builder_set_member_name (b, "is_pinned");
    json_builder_add_boolean_value (b, m->is_pinned);
    json_builder_set_member_name (b, "confidence");
    json_builder_add_double_value (b, m->confidence);
    json_builder_set_member_name (b, "expires_at");
    if (m->expires_at) json_builder_add_string_value (b, m->expires_at);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "created_at");
    if (m->created_at) json_builder_add_string_value (b, m->created_at);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "updated_at");
    if (m->updated_at) json_builder_add_string_value (b, m->updated_at);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "last_accessed_at");
    if (m->last_accessed_at) json_builder_add_string_value (b, m->last_accessed_at);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "access_count");
    json_builder_add_int_value (b, m->access_count);
    json_builder_set_member_name (b, "metadata");
    if (m->metadata) json_builder_add_string_value (b, m->metadata);
    else json_builder_add_null_value (b);
    json_builder_set_member_name (b, "has_embedding");
    json_builder_add_boolean_value (b, m->has_embedding);

    if (m->similarity > 0.0) {
        json_builder_set_member_name (b, "similarity");
        json_builder_add_double_value (b, m->similarity);
    }
    if (m->relevance > 0.0) {
        json_builder_set_member_name (b, "relevance");
        json_builder_add_double_value (b, m->relevance);
    }

    json_builder_end_object (b);
    JsonNode *node = json_builder_get_root (b);
    g_object_unref (b);
    return node;
}

static gchar *
memory_array_to_json (GPtrArray *memories)
{
    JsonBuilder *b = json_builder_new ();
    json_builder_begin_array (b);
    guint i;
    for (i = 0; i < memories->len; i++) {
        Memory *m = g_ptr_array_index (memories, i);
        JsonNode *n = memory_to_json_node (m);
        json_builder_add_value (b, n);
    }
    json_builder_end_array (b);
    JsonGenerator *gen = json_generator_new ();
    json_generator_set_pretty (gen, TRUE);
    json_generator_set_indent (gen, 2);
    json_generator_set_root (gen, json_builder_get_root (b));
    gchar *out = json_generator_to_data (gen, NULL);
    g_object_unref (gen);
    g_object_unref (b);
    return out;
}

static gchar *
json_node_to_string_pretty (JsonNode *node)
{
    JsonGenerator *gen = json_generator_new ();
    json_generator_set_pretty (gen, TRUE);
    json_generator_set_indent (gen, 2);
    json_generator_set_root (gen, node);
    gchar *out = json_generator_to_data (gen, NULL);
    g_object_unref (gen);
    return out;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Terminal formatters
 * ═══════════════════════════════════════════════════════════════════════════ */

static void
print_memory_detail (Memory *m)
{
    g_autofree gchar *hdr = _bold (g_strdup_printf ("━━━ Memory: %.8s ━━━", m->id ? m->id : "?"));
    g_print ("%s\n", hdr);

    g_autofree gchar *c1 = _cyan ("ID:");
    g_print ("  %s          %s\n", c1, m->id ? m->id : "-");
    g_autofree gchar *c2 = _cyan ("Category:");
    g_print ("  %s    %s/%s\n", c2, m->category ? m->category : "-",
             m->subcategory ? m->subcategory : "-");
    g_autofree gchar *c3 = _cyan ("Importance:");
    g_print ("  %s  %s\n", c3, m->importance ? m->importance : "normal");
    g_autofree gchar *c4 = _cyan ("Source:");
    g_print ("  %s      %s\n", c4, m->source ? m->source : "-");
    if (m->source_context) {
        g_autofree gchar *c = _cyan ("Context:");
        g_print ("  %s     %s\n", c, m->source_context);
    }
    if (m->conversation_id) {
        g_autofree gchar *c = _cyan ("Conv ID:");
        g_print ("  %s     %s\n", c, m->conversation_id);
    }
    g_autofree gchar *c5 = _cyan ("Pinned:");
    g_print ("  %s      %s\n", c5, m->is_pinned ? "yes" : "no");
    g_autofree gchar *c6 = _cyan ("Archived:");
    g_print ("  %s    %s\n", c6, m->is_archived ? "yes" : "no");
    g_autofree gchar *c7 = _cyan ("Confidence:");
    g_print ("  %s  %.2f\n", c7, m->confidence);
    g_autofree gchar *c8 = _cyan ("Expires:");
    g_print ("  %s     %s\n", c8, m->expires_at ? m->expires_at : "-");
    g_autofree gchar *c9 = _cyan ("Embedding:");
    g_print ("  %s   %s\n", c9, m->has_embedding ? "yes" : "no");
    g_autofree gchar *c10 = _cyan ("Accessed:");
    g_print ("  %s    %d times\n", c10, m->access_count);

    g_autofree gchar *c11 = _cyan ("Tags:");
    if (m->tags && m->n_tags > 0) {
        g_autofree gchar *joined = g_strjoinv (", ", m->tags);
        g_print ("  %s        %s\n", c11, joined);
    } else {
        g_print ("  %s        -\n", c11);
    }

    if (m->related_to) {
        g_autofree gchar *c = _cyan ("Related to:");
        g_print ("  %s  %.8s\n", c, m->related_to);
    }

    g_autofree gchar *c12 = _cyan ("Created:");
    g_print ("  %s     %s\n", c12, m->created_at ? m->created_at : "-");
    g_autofree gchar *c13 = _cyan ("Updated:");
    g_print ("  %s     %s\n", c13, m->updated_at ? m->updated_at : "-");
    g_autofree gchar *c14 = _cyan ("Last access:");
    g_print ("  %s %s\n", c14, m->last_accessed_at ? m->last_accessed_at : "never");

    g_autofree gchar *sh = _bold ("━━━ Summary ━━━");
    g_print ("\n%s\n  %s\n", sh, m->summary ? m->summary : "-");

    g_autofree gchar *ch = _bold ("━━━ Content ━━━");
    g_print ("\n%s\n", ch);
    if (m->content) {
        gchar **lines = g_strsplit (m->content, "\n", -1);
        gint i;
        for (i = 0; lines[i]; i++)
            g_print ("  %s\n", lines[i]);
        g_strfreev (lines);
    } else {
        g_print ("  -\n");
    }

    if (m->metadata && g_strcmp0 (m->metadata, "{}") != 0) {
        g_autofree gchar *mh = _bold ("━━━ Metadata ━━━");
        g_print ("\n%s\n  %s\n", mh, m->metadata);
    }
}

static void
print_memory_table (GPtrArray *memories, const gchar **columns)
{
    if (!memories || memories->len == 0) {
        g_autofree gchar *msg = _yellow ("No memories found.");
        g_print ("%s\n", msg);
        return;
    }

    const gchar **cols = columns ? columns : DEFAULT_COLUMNS;

    /* Print header */
    GString *hdr = g_string_new (NULL);
    gint total_width = 0;
    gint i;
    for (i = 0; cols[i]; i++) {
        if (i > 0) g_string_append (hdr, " | ");
        gint w = 20;
        if (g_strcmp0 (cols[i], "id") == 0) w = 8;
        else if (g_strcmp0 (cols[i], "similarity") == 0) w = 6;
        else if (g_strcmp0 (cols[i], "category") == 0) w = 12;
        else if (g_strcmp0 (cols[i], "importance") == 0) w = 8;
        else if (g_strcmp0 (cols[i], "summary") == 0) w = 50;
        else if (g_strcmp0 (cols[i], "tags") == 0) w = 25;
        else if (g_strcmp0 (cols[i], "created_at") == 0) w = 10;
        else if (g_strcmp0 (cols[i], "is_pinned") == 0) w = 3;

        g_autofree gchar *upper = g_ascii_strup (cols[i], -1);
        g_string_append_printf (hdr, "%-*s", w, upper);
        total_width += w + 3;
    }
    g_autofree gchar *bold_hdr = _bold (hdr->str);
    g_print ("%s\n", bold_hdr);
    for (i = 0; i < total_width; i++) g_print ("-");
    g_print ("\n");
    g_string_free (hdr, TRUE);

    /* Print rows */
    guint r;
    for (r = 0; r < memories->len; r++) {
        Memory *m = g_ptr_array_index (memories, r);
        gint i;
        for (i = 0; cols[i]; i++) {
            if (i > 0) g_print (" | ");
            const gchar *col = cols[i];
            gint w = 20;

            if (g_strcmp0 (col, "id") == 0) { w = 8; g_print ("%-*.*s", w, w, m->id ? m->id : ""); }
            else if (g_strcmp0 (col, "similarity") == 0) { w = 6; g_print ("%-*.2f", w, m->similarity); }
            else if (g_strcmp0 (col, "category") == 0) { w = 12; g_print ("%-*.*s", w, w, m->category ? m->category : ""); }
            else if (g_strcmp0 (col, "importance") == 0) { w = 8; g_print ("%-*.*s", w, w, m->importance ? m->importance : "normal"); }
            else if (g_strcmp0 (col, "summary") == 0) { w = 50; g_print ("%-*.*s", w, w, m->summary ? m->summary : ""); }
            else if (g_strcmp0 (col, "tags") == 0) {
                w = 25;
                if (m->tags && m->n_tags > 0) {
                    GString *ts = g_string_new (NULL);
                    gint t;
                    for (t = 0; t < MIN (m->n_tags, 3); t++) {
                        if (t > 0) g_string_append_c (ts, ',');
                        g_string_append (ts, m->tags[t]);
                    }
                    g_print ("%-*.*s", w, w, ts->str);
                    g_string_free (ts, TRUE);
                } else {
                    g_print ("%-*s", w, "");
                }
            }
            else if (g_strcmp0 (col, "created_at") == 0) {
                w = 10;
                if (m->created_at) g_print ("%-*.*s", w, w, m->created_at);
                else g_print ("%-*s", w, "");
            }
            else if (g_strcmp0 (col, "is_pinned") == 0) { w = 3; g_print ("%-*s", w, m->is_pinned ? "*" : " "); }
            else { g_print ("%-*s", w, ""); }
        }
        g_print ("\n");
    }
}

/* ═══════════════════════════════════════════════════════════════════════════
 * FZF integration
 * ═══════════════════════════════════════════════════════════════════════════ */

/* Helper: run fzf with argv, stdin from file descriptor, capture stdout selection.
 * Uses posix_spawn for zero shell overhead. fzf opens /dev/tty for UI. */
static gchar *
_run_fzf (const gchar *fzf_path, gchar **argv, gint stdin_fd)
{
    gint stdout_pipe[2];
    if (pipe (stdout_pipe) < 0)
        return NULL;

    posix_spawn_file_actions_t actions;
    posix_spawn_file_actions_init (&actions);
    posix_spawn_file_actions_adddup2 (&actions, stdin_fd, STDIN_FILENO);
    posix_spawn_file_actions_adddup2 (&actions, stdout_pipe[1], STDOUT_FILENO);
    posix_spawn_file_actions_addclose (&actions, stdout_pipe[0]);
    posix_spawn_file_actions_addclose (&actions, stdout_pipe[1]);
    posix_spawn_file_actions_addclose (&actions, stdin_fd);

    pid_t pid;
    gint err = posix_spawnp (&pid, fzf_path, &actions, NULL, argv, environ);
    posix_spawn_file_actions_destroy (&actions);
    close (stdout_pipe[1]);
    close (stdin_fd);

    if (err != 0) {
        close (stdout_pipe[0]);
        return NULL;
    }

    /* Read selection from fzf stdout */
    char buf[512] = {0};
    ssize_t n = read (stdout_pipe[0], buf, sizeof (buf) - 1);
    close (stdout_pipe[0]);

    gint status;
    waitpid (pid, &status, 0);

    if (n <= 0)
        return NULL;
    buf[n] = '\0';
    g_strstrip (buf);
    return buf[0] ? g_strdup (buf) : NULL;
}

static gchar *
fzf_select_memory (PGconn *conn, const gchar *header, gboolean include_archived)
{
    g_autofree gchar *fzf_path = g_find_program_in_path ("fzf");
    if (!fzf_path) {
        g_autofree gchar *msg = _red ("fzf not found. Please install fzf.");
        g_printerr ("%s\n", msg);
        return NULL;
    }

    /* Fetch everything in one query — no psql spawning for preview */
    const gchar *where = include_archived ? "TRUE" : "NOT is_archived";
    g_autofree gchar *sql = g_strdup_printf (
        "SELECT id, summary, category, importance, subcategory, source, "
        "tags, is_pinned, created_at, access_count, content "
        "FROM memories WHERE %s ORDER BY is_pinned DESC, created_at DESC", where);

    PGresult *res = PQexec (conn, sql);
    if (PQresultStatus (res) != PGRES_TUPLES_OK || PQntuples (res) == 0) {
        g_autofree gchar *msg = _yellow ("No memories found.");
        g_printerr ("%s\n", msg);
        PQclear (res);
        return NULL;
    }

    /* Create temp dir for preview files */
    g_autofree gchar *tmpdir = g_strdup ("/tmp/agent_memories_fzf_XXXXXX");
    if (!g_mkdtemp (tmpdir)) {
        PQclear (res);
        return NULL;
    }

    /* Build fzf input + write per-memory preview files (plain text, instant cat) */
    GString *input = g_string_new (NULL);
    gint nrows = PQntuples (res);
    gint i;
    for (i = 0; i < nrows; i++) {
        const gchar *id       = PQgetvalue (res, i, 0);
        const gchar *summary  = PQgetvalue (res, i, 1);
        const gchar *cat      = PQgetvalue (res, i, 2);
        const gchar *imp      = PQgetvalue (res, i, 3);
        const gchar *subcat   = PQgetvalue (res, i, 4);
        const gchar *source   = PQgetvalue (res, i, 5);
        const gchar *tags_raw = PQgetvalue (res, i, 6);
        gboolean pin = g_strcmp0 (PQgetvalue (res, i, 7), "t") == 0;
        const gchar *created  = PQgetvalue (res, i, 8);
        const gchar *acc_cnt  = PQgetvalue (res, i, 9);
        const gchar *content  = PQgetvalue (res, i, 10);

        /* fzf list line */
        g_string_append_printf (input, "%.8s %c | %-12.12s | %-8.8s | %-50.50s | %.10s\n",
            id, pin ? '*' : ' ', cat, imp, summary, created);

        /* Write preview file: tmpdir/<8-char-id>.txt */
        g_autofree gchar *preview_file = g_strdup_printf ("%s/%.8s.txt", tmpdir, id);
        /* Parse {tag1,tag2} → "tag1, tag2" */
        g_autofree gchar *tags_pretty = NULL;
        if (tags_raw && tags_raw[0] == '{') {
            g_autofree gchar *inner = g_strndup (tags_raw + 1, strlen (tags_raw) - 2);
            gchar **parts = g_strsplit (inner, ",", -1);
            tags_pretty = g_strjoinv (", ", parts);
            g_strfreev (parts);
        }

        GString *pv = g_string_new (NULL);
        g_string_append_printf (pv,
            "━━━ MEMORY ━━━\n"
            "ID: %s\n"
            "Category: %s/%s\n"
            "Importance: %s\n"
            "Source: %s\n"
            "Pinned: %s\n"
            "Tags: %s\n"
            "Created: %.16s\n"
            "Accessed: %s times\n"
            "\n━━━ SUMMARY ━━━\n%s\n"
            "\n━━━ CONTENT ━━━\n%s\n",
            id,
            cat[0] ? cat : "-", subcat[0] ? subcat : "-",
            imp[0] ? imp : "normal",
            source[0] ? source : "-",
            pin ? "yes" : "no",
            tags_pretty ? tags_pretty : (tags_raw[0] ? tags_raw : "-"),
            created,
            acc_cnt,
            summary[0] ? summary : "-",
            content[0] ? content : "-");
        g_file_set_contents (preview_file, pv->str, pv->len, NULL);
        g_string_free (pv, TRUE);
    }
    PQclear (res);

    /* Write fzf input to temp file */
    g_autofree gchar *input_path = g_strdup_printf ("%s/_input.txt", tmpdir);
    g_file_set_contents (input_path, input->str, input->len, NULL);
    g_string_free (input, TRUE);

    /* Preview command: just cat the pre-built file — instant, zero subprocess overhead */
    /* fzf's {1} = first whitespace-delimited field = the 8-char UUID */
    g_autofree gchar *preview_cmd = g_strdup_printf ("cat %s/{1}.txt", tmpdir);
    g_autofree gchar *header_arg = g_strdup_printf ("%s (Ctrl-C to cancel)", header);

    /* Run fzf via posix_spawn — no shell overhead */
    gchar *stdout_data = NULL;
    gint input_fd = open (input_path, O_RDONLY);
    if (input_fd < 0) goto cleanup;

    {
        gchar *fzf_argv[] = {
            fzf_path,
            "--preview", preview_cmd,
            "--preview-window", "down:60%:wrap",
            "--header", header_arg,
            "--bind", "ctrl-/:toggle-preview",
            "--ansi",
            NULL
        };
        stdout_data = _run_fzf (fzf_path, fzf_argv, input_fd);
    }

cleanup:
    /* Remove temp dir and all files */
    {
        g_autoptr (GDir) dir = g_dir_open (tmpdir, 0, NULL);
        if (dir) {
            const gchar *name;
            while ((name = g_dir_read_name (dir)) != NULL) {
                g_autofree gchar *path = g_build_filename (tmpdir, name, NULL);
                g_unlink (path);
            }
        }
        g_rmdir (tmpdir);
    }

    if (!stdout_data || !*stdout_data) {
        g_free (stdout_data);
        return NULL;
    }

    /* Extract short UUID from selection */
    gchar *space = strchr (stdout_data, ' ');
    if (space) *space = '\0';

    gchar *full_id = db_resolve_uuid (conn, stdout_data);
    g_free (stdout_data);
    return full_id;
}

static gchar *
fzf_search_memory (PGconn *conn, const gchar *initial_query)
{
    g_autofree gchar *fzf_path = g_find_program_in_path ("fzf");
    if (!fzf_path) {
        g_autofree gchar *msg = _red ("fzf not found.");
        g_printerr ("%s\n", msg);
        return NULL;
    }

    PGresult *res = PQexec (conn,
        "SELECT id, summary, category, content FROM memories "
        "WHERE NOT is_archived ORDER BY is_pinned DESC, created_at DESC");
    if (PQresultStatus (res) != PGRES_TUPLES_OK || PQntuples (res) == 0) {
        g_autofree gchar *msg = _yellow ("No memories found.");
        g_printerr ("%s\n", msg);
        PQclear (res);
        return NULL;
    }

    GString *input = g_string_new (NULL);
    gint i;
    for (i = 0; i < PQntuples (res); i++) {
        const gchar *id = PQgetvalue (res, i, 0);
        const gchar *summary = PQgetvalue (res, i, 1);
        const gchar *cat = PQgetvalue (res, i, 2);
        const gchar *content = PQgetvalue (res, i, 3);
        g_autofree gchar *snippet = g_strndup (content, 100);
        {
            gchar *p;
            for (p = snippet; *p; p++) if (*p == '\n') *p = ' ';
        }
        g_string_append_printf (input, "%.8s | %-12.12s | %-60.60s | %s\n",
            id, cat, summary, snippet);
    }
    PQclear (res);

    /* Write to temp file — avoids embedding content in shell command string */
    g_autofree gchar *input_path = NULL;
    gint tmp_fd = g_file_open_tmp ("agent_memories_search_XXXXXX.txt", &input_path, NULL);
    if (tmp_fd >= 0) {
        write (tmp_fd, input->str, input->len);
        /* lseek back to start so fzf can read it */
        lseek (tmp_fd, 0, SEEK_SET);
    }
    g_string_free (input, TRUE);

    if (tmp_fd < 0)
        return NULL;

    /* Run fzf via posix_spawn — direct argv, no shell parsing */
    gchar *fzf_argv[] = {
        fzf_path,
        "--header", "Search memories (type to filter)",
        "--query", (gchar *)(initial_query ? initial_query : ""),
        NULL
    };

    gchar *stdout_data = _run_fzf (fzf_path, fzf_argv, tmp_fd);
    g_unlink (input_path);

    if (!stdout_data || !*stdout_data) {
        g_free (stdout_data);
        return NULL;
    }

    gchar *space = strchr (stdout_data, ' ');
    if (space) *space = '\0';

    gchar *full_id = db_resolve_uuid (conn, stdout_data);
    g_free (stdout_data);
    return full_id;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Bootstrap
 * ═══════════════════════════════════════════════════════════════════════════ */

static gchar *
bootstrap_text (PGconn *conn, gint recent_limit, gboolean slim)
{
    GHashTable *seen = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    GString *out = g_string_new (NULL);
    gint total = 0;

    typedef struct { const gchar *name; const gchar *cat; const gchar *imp; gboolean is_pinned; gint limit; } Section;

    Section sections_full[] = {
        { "PINNED", NULL, NULL, TRUE, 100 },
        { "DECISIONS", "decision", NULL, FALSE, 100 },
        { "PREFERENCES", "preference", NULL, FALSE, 100 },
        { "CRITICAL", NULL, "critical", FALSE, 100 },
        { "RECENT (project)", "project", NULL, FALSE, 0 },
        { "RECENT (technical)", "technical", NULL, FALSE, 0 },
        { "RECENT (learning)", "learning", NULL, FALSE, 0 },
        { "RECENT (workflow)", "workflow", NULL, FALSE, 0 },
        { NULL, NULL, NULL, FALSE, 0 }
    };
    Section sections_slim[] = {
        { "PINNED", NULL, NULL, TRUE, 100 },
        { "CRITICAL", NULL, "critical", FALSE, 100 },
        { NULL, NULL, NULL, FALSE, 0 }
    };

    Section *sections = slim ? sections_slim : sections_full;

    gint s;

    for (s = 0; sections[s].name; s++) {
        gint lim = sections[s].limit ? sections[s].limit : recent_limit;
        GPtrArray *mems = db_list_memories (conn,
            sections[s].cat, NULL, sections[s].imp, NULL, 0, NULL,
            sections[s].is_pinned ? 1 : -1,
            FALSE, FALSE, lim, 0, "created_at", TRUE);

        GString *sec_out = g_string_new (NULL);
        guint i;
        for (i = 0; i < mems->len; i++) {
            Memory *m = g_ptr_array_index (mems, i);
            if (g_hash_table_contains (seen, m->id)) continue;
            g_hash_table_add (seen, g_strdup (m->id));

            g_string_append_printf (sec_out, "- [%.8s] %s",
                m->id, m->summary ? m->summary : "");
            if (m->tags && m->n_tags > 0) {
                g_autofree gchar *joined = g_strjoinv (",", m->tags);
                g_string_append_printf (sec_out, " {tags: %s}", joined);
            }
            g_string_append_c (sec_out, '\n');
            total++;
        }

        if (sec_out->len > 0) {
            g_string_append_printf (out, "\n## %s\n%s", sections[s].name, sec_out->str);
        }
        g_string_free (sec_out, TRUE);
        g_ptr_array_unref (mems);
    }

    GString *result = g_string_new (NULL);
    g_string_append_printf (result, "=== AGENT MEMORIES BOOTSTRAP%s ===\n", slim ? " (SLIM)" : "");
    if (total == 0) {
        g_string_append (result, "No memories stored yet. Use 'agent_memories add' to create memories.\n");
    } else {
        g_string_append_printf (result, "[%d memories injected]\n%s", total, out->str);
    }
    g_string_append (result, "\n=== END BOOTSTRAP ===\n");

    g_string_free (out, TRUE);
    g_hash_table_unref (seen);
    return g_string_free (result, FALSE);
}

static gchar *
bootstrap_json (PGconn *conn, gint recent_limit, gboolean slim)
{
    GHashTable *seen = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, NULL);
    JsonBuilder *b = json_builder_new ();
    json_builder_begin_object (b);
    json_builder_set_member_name (b, "sections");
    json_builder_begin_object (b);
    gint total = 0;

    typedef struct { const gchar *name; const gchar *cat; const gchar *imp; gboolean is_pinned; gint limit; } Section;
    Section sections_full[] = {
        { "PINNED", NULL, NULL, TRUE, 100 },
        { "DECISIONS", "decision", NULL, FALSE, 100 },
        { "PREFERENCES", "preference", NULL, FALSE, 100 },
        { "CRITICAL", NULL, "critical", FALSE, 100 },
        { "RECENT (project)", "project", NULL, FALSE, 0 },
        { "RECENT (technical)", "technical", NULL, FALSE, 0 },
        { "RECENT (learning)", "learning", NULL, FALSE, 0 },
        { "RECENT (workflow)", "workflow", NULL, FALSE, 0 },
        { NULL, NULL, NULL, FALSE, 0 }
    };
    Section sections_slim[] = {
        { "PINNED", NULL, NULL, TRUE, 100 },
        { "CRITICAL", NULL, "critical", FALSE, 100 },
        { NULL, NULL, NULL, FALSE, 0 }
    };
    Section *sections = slim ? sections_slim : sections_full;

    gint s;

    for (s = 0; sections[s].name; s++) {
        gint lim = sections[s].limit ? sections[s].limit : recent_limit;
        GPtrArray *mems = db_list_memories (conn,
            sections[s].cat, NULL, sections[s].imp, NULL, 0, NULL,
            sections[s].is_pinned ? 1 : -1,
            FALSE, FALSE, lim, 0, "created_at", TRUE);

        JsonBuilder *sec = json_builder_new ();
        json_builder_begin_array (sec);
        gint sec_count = 0;

        guint i;

        for (i = 0; i < mems->len; i++) {
            Memory *m = g_ptr_array_index (mems, i);
            if (g_hash_table_contains (seen, m->id)) continue;
            g_hash_table_add (seen, g_strdup (m->id));

            json_builder_begin_object (sec);
            json_builder_set_member_name (sec, "id");
            json_builder_add_string_value (sec, m->id);
            json_builder_set_member_name (sec, "summary");
            json_builder_add_string_value (sec, m->summary ? m->summary : "");
            json_builder_set_member_name (sec, "category");
            json_builder_add_string_value (sec, m->category ? m->category : "general");
            json_builder_set_member_name (sec, "tags");
            json_builder_begin_array (sec);
            if (m->tags) {
                gint t;
                for (t = 0; t < m->n_tags; t++)
                    json_builder_add_string_value (sec, m->tags[t]);
            }
            json_builder_end_array (sec);
            json_builder_end_object (sec);
            sec_count++;
            total++;
        }
        json_builder_end_array (sec);

        if (sec_count > 0) {
            json_builder_set_member_name (b, sections[s].name);
            json_builder_add_value (b, json_builder_get_root (sec));
        }
        g_object_unref (sec);
        g_ptr_array_unref (mems);
    }

    json_builder_end_object (b);
    json_builder_set_member_name (b, "total");
    json_builder_add_int_value (b, total);
    json_builder_end_object (b);

    gchar *result = json_node_to_string_pretty (json_builder_get_root (b));
    g_object_unref (b);
    g_hash_table_unref (seen);
    return result;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * MCP tool definitions
 * ═══════════════════════════════════════════════════════════════════════════ */

static const gchar *MCP_TOOLS_JSON =
"["
"  {\"name\":\"memory_add\",\"description\":\"Store a new memory in the persistent agent memory database.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"content\":{\"type\":\"string\",\"description\":\"The full memory content text\"},"
"     \"summary\":{\"type\":\"string\",\"description\":\"Short one-line summary\"},"
"     \"category\":{\"type\":\"string\",\"enum\":[\"general\",\"decision\",\"preference\",\"fact\",\"project\",\"learning\",\"insight\",\"todo\",\"relationship\",\"technical\",\"workflow\",\"debug\",\"research\",\"config\",\"personal\"]},"
"     \"subcategory\":{\"type\":\"string\"},"
"     \"importance\":{\"type\":\"string\",\"enum\":[\"low\",\"normal\",\"high\",\"critical\"]},"
"     \"source\":{\"type\":\"string\"},\"source_context\":{\"type\":\"string\"},"
"     \"conversation_id\":{\"type\":\"string\"},"
"     \"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},"
"     \"related_to\":{\"type\":\"string\"},\"pin\":{\"type\":\"boolean\"},"
"     \"metadata\":{\"type\":\"object\"},\"confidence\":{\"type\":\"number\"},"
"     \"expires\":{\"type\":\"string\"}},"
"   \"required\":[\"content\"]}},"
"  {\"name\":\"memory_show\",\"description\":\"Fetch and display a single memory by ID.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}},"
"  {\"name\":\"memory_list\",\"description\":\"List memories with optional filtering, sorting, and pagination.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"category\":{\"type\":\"string\"},\"subcategory\":{\"type\":\"string\"},"
"     \"importance\":{\"type\":\"string\"},\"source\":{\"type\":\"string\"},"
"     \"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},"
"     \"pinned\":{\"type\":\"boolean\"},\"include_archived\":{\"type\":\"boolean\"},"
"     \"only_archived\":{\"type\":\"boolean\"},\"limit\":{\"type\":\"integer\"},"
"     \"offset\":{\"type\":\"integer\"},\"sort\":{\"type\":\"string\"},"
"     \"ascending\":{\"type\":\"boolean\"}}}},"
"  {\"name\":\"memory_search\",\"description\":\"Search memories using fuzzy (default), exact (tsvector), or semantic (pgvector+ollama) modes.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"query\":{\"type\":\"string\"},\"mode\":{\"type\":\"string\",\"enum\":[\"fuzzy\",\"exact\",\"semantic\"]},"
"     \"category\":{\"type\":\"string\"},\"include_archived\":{\"type\":\"boolean\"},"
"     \"limit\":{\"type\":\"integer\"}},\"required\":[\"query\"]}},"
"  {\"name\":\"memory_update\",\"description\":\"Update fields on an existing memory.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"id\":{\"type\":\"string\"},\"content\":{\"type\":\"string\"},\"summary\":{\"type\":\"string\"},"
"     \"category\":{\"type\":\"string\"},\"subcategory\":{\"type\":\"string\"},"
"     \"importance\":{\"type\":\"string\"},\"source\":{\"type\":\"string\"},"
"     \"source_context\":{\"type\":\"string\"},\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},"
"     \"is_pinned\":{\"type\":\"boolean\"},\"is_archived\":{\"type\":\"boolean\"},"
"     \"metadata\":{\"type\":\"object\"},\"confidence\":{\"type\":\"number\"},"
"     \"expires_at\":{\"type\":\"string\"}},\"required\":[\"id\"]}},"
"  {\"name\":\"memory_archive\",\"description\":\"Archive or unarchive a memory.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"unarchive\":{\"type\":\"boolean\"}},\"required\":[\"id\"]}},"
"  {\"name\":\"memory_pin\",\"description\":\"Pin or unpin a memory.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"unpin\":{\"type\":\"boolean\"}},\"required\":[\"id\"]}},"
"  {\"name\":\"memory_delete\",\"description\":\"Permanently delete a memory.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"}},\"required\":[\"id\"]}},"
"  {\"name\":\"memory_tag\",\"description\":\"Add or remove tags on a memory.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"id\":{\"type\":\"string\"},\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"remove\":{\"type\":\"boolean\"}},\"required\":[\"id\",\"tags\"]}},"
"  {\"name\":\"memory_stats\",\"description\":\"Get aggregate statistics about the memory store.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{}}},"
"  {\"name\":\"memory_export\",\"description\":\"Export all memories as a JSON array.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"include_archived\":{\"type\":\"boolean\"}}}},"
"  {\"name\":\"memory_import\",\"description\":\"Import memories from a JSON array.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"memories\":{\"type\":\"array\",\"items\":{\"type\":\"object\",\"properties\":{\"content\":{\"type\":\"string\"},\"summary\":{\"type\":\"string\"},\"category\":{\"type\":\"string\"},\"importance\":{\"type\":\"string\"},\"source\":{\"type\":\"string\"},\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}},\"metadata\":{\"type\":\"object\"}},\"required\":[\"content\"]}}},\"required\":[\"memories\"]}},"
"  {\"name\":\"memory_prune\",\"description\":\"Archive or permanently delete expired memories.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"delete\":{\"type\":\"boolean\"},\"dry_run\":{\"type\":\"boolean\"}}}},"
"  {\"name\":\"memory_backfill\",\"description\":\"Generate embeddings for memories missing them.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"include_archived\":{\"type\":\"boolean\"}}}},"
"  {\"name\":\"memory_bootstrap\",\"description\":\"Get structured memory context for AI session bootstrap.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"format\":{\"type\":\"string\",\"enum\":[\"text\",\"json\"]},\"limit\":{\"type\":\"integer\"}}}},"
"  {\"name\":\"memory_bootstrap_slim\",\"description\":\"Get minimal memory context (pinned + critical only).\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{\"format\":{\"type\":\"string\",\"enum\":[\"text\",\"json\"]}}}}"
"]";

/* ═══════════════════════════════════════════════════════════════════════════
 * MCP tool call handler
 * ═══════════════════════════════════════════════════════════════════════════ */

static McpToolResult *
_mcp_text_result (const gchar *text, gboolean is_error)
{
    McpToolResult *r = mcp_tool_result_new (is_error);
    mcp_tool_result_add_text (r, text);
    return r;
}

static const gchar *
_json_get_string (JsonObject *obj, const gchar *name, const gchar *def)
{
    if (!json_object_has_member (obj, name)) return def;
    JsonNode *n = json_object_get_member (obj, name);
    if (json_node_is_null (n)) return def;
    return json_node_get_string (n);
}

static gboolean
_json_get_bool (JsonObject *obj, const gchar *name, gboolean def)
{
    if (!json_object_has_member (obj, name)) return def;
    return json_object_get_boolean_member (obj, name);
}

static gint64
_json_get_int (JsonObject *obj, const gchar *name, gint64 def)
{
    if (!json_object_has_member (obj, name)) return def;
    return json_object_get_int_member (obj, name);
}

static McpToolResult *
mcp_handle_tool_call (McpServer *server, const gchar *name, JsonObject *args, gpointer user_data)
{
    AppState *app = (AppState *)user_data;
    /* mcp-glib may pass NULL for tools with no arguments in the call */
    g_autoptr (JsonObject) _empty_args = NULL;
    if (!args) {
        _empty_args = json_object_new ();
        args = _empty_args;
    }
    if (g_strcmp0 (name, "memory_add") == 0) {
        const gchar *content = _json_get_string (args, "content", "");
        const gchar *summary = _json_get_string (args, "summary", NULL);
        const gchar *category = _json_get_string (args, "category", "general");
        const gchar *subcategory = _json_get_string (args, "subcategory", NULL);
        const gchar *importance = _json_get_string (args, "importance", "normal");
        const gchar *source = _json_get_string (args, "source", NULL);
        const gchar *source_ctx = _json_get_string (args, "source_context", NULL);
        const gchar *conv_id = _json_get_string (args, "conversation_id", NULL);
        const gchar *related = _json_get_string (args, "related_to", NULL);
        gboolean pin = _json_get_bool (args, "pin", FALSE);
        gdouble confidence = json_object_has_member (args, "confidence")
            ? json_object_get_double_member (args, "confidence") : 1.0;
        const gchar *expires = _json_get_string (args, "expires", NULL);

        /* Extract tags */
        gchar **tags = NULL;
        gint n_tags = 0;
        if (json_object_has_member (args, "tags")) {
            JsonArray *ta = json_object_get_array_member (args, "tags");
            n_tags = json_array_get_length (ta);
            tags = g_new0 (gchar *, n_tags + 1);
            gint i;
            for (i = 0; i < n_tags; i++)
                tags[i] = g_strdup (json_array_get_string_element (ta, i));
        }

        /* Extract metadata */
        gchar *meta_json = NULL;
        if (json_object_has_member (args, "metadata")) {
            JsonGenerator *gen = json_generator_new ();
            json_generator_set_root (gen, json_object_get_member (args, "metadata"));
            meta_json = json_generator_to_data (gen, NULL);
            g_object_unref (gen);
        }

        g_autofree gchar *exp_str = expires ? parse_expiry (expires) : NULL;
        gchar *mid = db_add_memory (app, content, summary, category, subcategory,
            importance, source, source_ctx, conv_id, tags, n_tags, related,
            pin, meta_json, confidence, exp_str);
        g_strfreev (tags);
        g_free (meta_json);

        if (mid) {
            g_autofree gchar *json = g_strdup_printf ("{\"status\": \"created\", \"id\": \"%s\"}", mid);
            g_free (mid);
            return _mcp_text_result (json, FALSE);
        }
        return _mcp_text_result ("Failed to create memory", TRUE);
    }

    if (g_strcmp0 (name, "memory_show") == 0) {
        const gchar *id = _json_get_string (args, "id", "");
        Memory *mem = db_get_memory (app->conn, id);
        if (!mem) return _mcp_text_result ("Memory not found", TRUE);
        JsonNode *node = memory_to_json_node (mem);
        g_autofree gchar *json = json_node_to_string_pretty (node);
        json_node_unref (node);
        memory_free (mem);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_list") == 0) {
        const gchar *cat = _json_get_string (args, "category", NULL);
        const gchar *subcat = _json_get_string (args, "subcategory", NULL);
        const gchar *imp = _json_get_string (args, "importance", NULL);
        const gchar *src = _json_get_string (args, "source", NULL);
        gboolean pinned = _json_get_bool (args, "pinned", FALSE);
        gboolean inc_arch = _json_get_bool (args, "include_archived", FALSE);
        gboolean only_arch = _json_get_bool (args, "only_archived", FALSE);
        gint limit = (gint)_json_get_int (args, "limit", 50);
        gint offset = (gint)_json_get_int (args, "offset", 0);
        const gchar *sort = _json_get_string (args, "sort", "created_at");
        gboolean ascending = _json_get_bool (args, "ascending", FALSE);

        /* Extract tags filter */
        gchar **filter_tags = NULL;
        gint n_filter = 0;
        if (json_object_has_member (args, "tags")) {
            JsonArray *ta = json_object_get_array_member (args, "tags");
            n_filter = json_array_get_length (ta);
            filter_tags = g_new0 (gchar *, n_filter + 1);
            gint i;
            for (i = 0; i < n_filter; i++)
                filter_tags[i] = g_strdup (json_array_get_string_element (ta, i));
        }

        GPtrArray *mems = db_list_memories (app->conn, cat, subcat, imp,
            filter_tags, n_filter, src, pinned ? 1 : -1,
            inc_arch, only_arch, limit, offset, sort, !ascending);
        g_autofree gchar *json = memory_array_to_json (mems);
        g_ptr_array_unref (mems);
        g_strfreev (filter_tags);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_search") == 0) {
        const gchar *query = _json_get_string (args, "query", "");
        const gchar *mode = _json_get_string (args, "mode", "fuzzy");
        const gchar *cat = _json_get_string (args, "category", NULL);
        gboolean inc_arch = _json_get_bool (args, "include_archived", FALSE);
        gint limit = (gint)_json_get_int (args, "limit", 25);

        GPtrArray *results;
        if (g_strcmp0 (mode, "semantic") == 0)
            results = db_search_semantic (app, query, cat, inc_arch, limit);
        else if (g_strcmp0 (mode, "exact") == 0)
            results = db_search_exact (app->conn, query, cat, inc_arch, limit);
        else
            results = db_search_fuzzy (app->conn, query, cat, inc_arch, limit);

        g_autofree gchar *json = memory_array_to_json (results);
        g_ptr_array_unref (results);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_update") == 0) {
        const gchar *id = _json_get_string (args, "id", "");
        GHashTable *fields = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, g_free);

        const gchar *update_keys[] = {
            "content", "summary", "category", "subcategory", "importance",
            "source", "source_context", NULL
        };
        gint i;
        for (i = 0; update_keys[i]; i++) {
            const gchar *v = _json_get_string (args, update_keys[i], NULL);
            if (v) g_hash_table_insert (fields, (gpointer)update_keys[i], g_strdup (v));
        }
        if (json_object_has_member (args, "is_pinned"))
            g_hash_table_insert (fields, "is_pinned",
                g_strdup (_json_get_bool (args, "is_pinned", FALSE) ? "true" : "false"));
        if (json_object_has_member (args, "is_archived"))
            g_hash_table_insert (fields, "is_archived",
                g_strdup (_json_get_bool (args, "is_archived", FALSE) ? "true" : "false"));
        if (json_object_has_member (args, "confidence"))
            g_hash_table_insert (fields, "confidence",
                g_strdup_printf ("%.2f", json_object_get_double_member (args, "confidence")));
        if (json_object_has_member (args, "tags")) {
            JsonArray *ta = json_object_get_array_member (args, "tags");
            GString *t = g_string_new ("{");
            guint i;
            for (i = 0; i < json_array_get_length (ta); i++) {
                if (i > 0) g_string_append_c (t, ',');
                g_string_append_printf (t, "\"%s\"", json_array_get_string_element (ta, i));
            }
            g_string_append_c (t, '}');
            g_hash_table_insert (fields, "tags", g_string_free (t, FALSE));
        }
        if (json_object_has_member (args, "metadata")) {
            JsonGenerator *gen = json_generator_new ();
            json_generator_set_root (gen, json_object_get_member (args, "metadata"));
            g_hash_table_insert (fields, "metadata", json_generator_to_data (gen, NULL));
            g_object_unref (gen);
        }
        if (json_object_has_member (args, "expires_at")) {
            g_autofree gchar *exp = parse_expiry (_json_get_string (args, "expires_at", NULL));
            if (exp) g_hash_table_insert (fields, "expires_at", g_strdup (exp));
        }

        if (g_hash_table_size (fields) == 0) {
            g_hash_table_unref (fields);
            return _mcp_text_result ("No fields to update", TRUE);
        }

        gboolean ok = db_update_memory (app, id, fields);
        g_hash_table_unref (fields);
        if (ok) {
            g_autofree gchar *json = g_strdup_printf ("{\"status\": \"updated\", \"id\": \"%s\"}", id);
            return _mcp_text_result (json, FALSE);
        }
        return _mcp_text_result ("Memory not found", TRUE);
    }

    if (g_strcmp0 (name, "memory_archive") == 0) {
        const gchar *id = _json_get_string (args, "id", "");
        gboolean unarch = _json_get_bool (args, "unarchive", FALSE);
        GHashTable *f = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, g_free);
        g_hash_table_insert (f, "is_archived", g_strdup (unarch ? "false" : "true"));
        gboolean ok = db_update_memory (app, id, f);
        g_hash_table_unref (f);
        if (ok) {
            g_autofree gchar *json = g_strdup_printf ("{\"status\": \"%s\", \"id\": \"%s\"}",
                unarch ? "unarchived" : "archived", id);
            return _mcp_text_result (json, FALSE);
        }
        return _mcp_text_result ("Memory not found", TRUE);
    }

    if (g_strcmp0 (name, "memory_pin") == 0) {
        const gchar *id = _json_get_string (args, "id", "");
        gboolean unpin = _json_get_bool (args, "unpin", FALSE);
        GHashTable *f = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, g_free);
        g_hash_table_insert (f, "is_pinned", g_strdup (unpin ? "false" : "true"));
        gboolean ok = db_update_memory (app, id, f);
        g_hash_table_unref (f);
        if (ok) {
            g_autofree gchar *json = g_strdup_printf ("{\"status\": \"%s\", \"id\": \"%s\"}",
                unpin ? "unpinned" : "pinned", id);
            return _mcp_text_result (json, FALSE);
        }
        return _mcp_text_result ("Memory not found", TRUE);
    }

    if (g_strcmp0 (name, "memory_delete") == 0) {
        const gchar *id = _json_get_string (args, "id", "");
        if (db_delete_memory (app->conn, id)) {
            g_autofree gchar *json = g_strdup_printf ("{\"status\": \"deleted\", \"id\": \"%s\"}", id);
            return _mcp_text_result (json, FALSE);
        }
        return _mcp_text_result ("Memory not found", TRUE);
    }

    if (g_strcmp0 (name, "memory_tag") == 0) {
        const gchar *id = _json_get_string (args, "id", "");
        gboolean remove = _json_get_bool (args, "remove", FALSE);
        g_autofree gchar *full_id = db_resolve_uuid (app->conn, id);
        if (!full_id) return _mcp_text_result ("Memory not found", TRUE);

        JsonArray *ta = json_object_get_array_member (args, "tags");
        guint i;
        for (i = 0; i < json_array_get_length (ta); i++) {
            const gchar *tag = json_array_get_string_element (ta, i);
            const gchar *params[] = { tag, full_id };
            PGresult *res;
            if (remove)
                res = PQexecParams (app->conn,
                    "UPDATE memories SET tags = array_remove(tags, $1) WHERE id = $2::uuid",
                    2, NULL, params, NULL, NULL, 0);
            else
                res = PQexecParams (app->conn,
                    "UPDATE memories SET tags = CASE WHEN NOT ($1 = ANY(tags)) "
                    "THEN array_append(tags, $1) ELSE tags END WHERE id = $2::uuid",
                    2, NULL, params, NULL, NULL, 0);
            PQclear (res);
        }
        g_autofree gchar *json = g_strdup_printf ("{\"status\": \"%s\", \"id\": \"%s\"}",
            remove ? "tags_removed" : "tags_added", full_id);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_stats") == 0) {
        JsonNode *stats = db_get_stats (app->conn);
        g_autofree gchar *json = json_node_to_string_pretty (stats);
        json_node_unref (stats);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_export") == 0) {
        gboolean inc_arch = _json_get_bool (args, "include_archived", TRUE);
        GPtrArray *mems = db_list_memories (app->conn, NULL, NULL, NULL, NULL, 0, NULL,
            -1, inc_arch, FALSE, 100000, 0, "created_at", TRUE);
        g_autofree gchar *json = memory_array_to_json (mems);
        g_ptr_array_unref (mems);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_import") == 0) {
        JsonArray *mem_arr = json_object_get_array_member (args, "memories");
        gint count = 0;
        guint i;
        for (i = 0; i < json_array_get_length (mem_arr); i++) {
            JsonObject *mo = json_array_get_object_element (mem_arr, i);
            const gchar *content = _json_get_string (mo, "content", "");
            const gchar *summary = _json_get_string (mo, "summary", NULL);
            const gchar *cat = _json_get_string (mo, "category", "general");
            const gchar *subcat = _json_get_string (mo, "subcategory", NULL);
            const gchar *imp = _json_get_string (mo, "importance", "normal");
            const gchar *src = _json_get_string (mo, "source", NULL);

            gchar **tags = NULL;
            gint n_tags = 0;
            if (json_object_has_member (mo, "tags")) {
                JsonArray *ta = json_object_get_array_member (mo, "tags");
                n_tags = json_array_get_length (ta);
                tags = g_new0 (gchar *, n_tags + 1);
                gint t;
                for (t = 0; t < n_tags; t++)
                    tags[t] = g_strdup (json_array_get_string_element (ta, t));
            }

            gchar *mid = db_add_memory (app, content, summary, cat, subcat, imp,
                src, NULL, NULL, tags, n_tags, NULL, FALSE, NULL, 1.0, NULL);
            g_free (mid);
            g_strfreev (tags);
            count++;
        }
        g_autofree gchar *json = g_strdup_printf ("{\"status\": \"imported\", \"count\": %d}", count);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_prune") == 0) {
        gboolean do_delete = _json_get_bool (args, "delete", FALSE);
        gboolean dry_run = _json_get_bool (args, "dry_run", FALSE);

        PGresult *cnt = PQexec (app->conn,
            "SELECT COUNT(*) FROM memories WHERE expires_at IS NOT NULL "
            "AND expires_at < CURRENT_TIMESTAMP AND is_archived = FALSE");
        gint expired = (PQresultStatus (cnt) == PGRES_TUPLES_OK) ? atoi (PQgetvalue (cnt, 0, 0)) : 0;
        PQclear (cnt);

        if (expired == 0)
            return _mcp_text_result ("{\"status\": \"nothing_to_prune\", \"count\": 0}", FALSE);

        if (dry_run) {
            PGresult *res = PQexec (app->conn,
                "SELECT id, summary, expires_at FROM memories WHERE expires_at IS NOT NULL "
                "AND expires_at < CURRENT_TIMESTAMP AND is_archived = FALSE ORDER BY expires_at");
            JsonBuilder *b = json_builder_new ();
            json_builder_begin_object (b);
            json_builder_set_member_name (b, "status"); json_builder_add_string_value (b, "dry_run");
            json_builder_set_member_name (b, "count"); json_builder_add_int_value (b, PQntuples (res));
            json_builder_set_member_name (b, "memories");
            json_builder_begin_array (b);
            gint i;
            for (i = 0; i < PQntuples (res); i++) {
                json_builder_begin_object (b);
                json_builder_set_member_name (b, "id"); json_builder_add_string_value (b, PQgetvalue (res, i, 0));
                json_builder_set_member_name (b, "summary"); json_builder_add_string_value (b, PQgetvalue (res, i, 1));
                json_builder_set_member_name (b, "expires_at"); json_builder_add_string_value (b, PQgetvalue (res, i, 2));
                json_builder_end_object (b);
            }
            json_builder_end_array (b);
            json_builder_end_object (b);
            g_autofree gchar *json = json_node_to_string_pretty (json_builder_get_root (b));
            PQclear (res);
            g_object_unref (b);
            return _mcp_text_result (json, FALSE);
        }

        PGresult *res;
        if (do_delete)
            res = PQexec (app->conn,
                "DELETE FROM memories WHERE expires_at IS NOT NULL "
                "AND expires_at < CURRENT_TIMESTAMP");
        else
            res = PQexec (app->conn,
                "UPDATE memories SET is_archived = TRUE WHERE expires_at IS NOT NULL "
                "AND expires_at < CURRENT_TIMESTAMP AND is_archived = FALSE");
        gint affected = atoi (PQcmdTuples (res));
        PQclear (res);
        g_autofree gchar *json = g_strdup_printf ("{\"status\": \"%s\", \"count\": %d}",
            do_delete ? "deleted" : "archived", affected);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_backfill") == 0) {
        gboolean inc_arch = _json_get_bool (args, "include_archived", FALSE);
        const gchar *where = inc_arch ? "embedding IS NULL" : "embedding IS NULL AND is_archived = FALSE";
        g_autofree gchar *sql = g_strdup_printf (
            "SELECT id, content, summary FROM memories WHERE %s ORDER BY created_at ASC", where);

        PGresult *res = PQexec (app->conn, sql);
        gint total = PQntuples (res);
        if (total == 0) {
            PQclear (res);
            return _mcp_text_result ("{\"status\": \"complete\", \"message\": \"All memories already have embeddings\"}", FALSE);
        }

        gint success = 0, failed = 0;
        gint i;
        for (i = 0; i < total; i++) {
            const gchar *id = PQgetvalue (res, i, 0);
            const gchar *content = PQgetvalue (res, i, 1);
            const gchar *summary = PQgetvalue (res, i, 2);
            g_autofree gchar *embed_text = g_strdup_printf ("%s %s", summary ? summary : "", content);
            g_autofree gchar *embedding = generate_embedding_sync (
                app->soup, app->ollama_url, app->embed_model, embed_text);
            if (embedding) {
                const gchar *up[] = { embedding, id };
                PGresult *u = PQexecParams (app->conn,
                    "UPDATE memories SET embedding = $1 WHERE id = $2::uuid",
                    2, NULL, up, NULL, NULL, 0);
                PQclear (u);
                success++;
            } else {
                failed++;
            }
        }
        PQclear (res);
        g_autofree gchar *json = g_strdup_printf (
            "{\"status\": \"complete\", \"total\": %d, \"embedded\": %d, \"failed\": %d}",
            total, success, failed);
        return _mcp_text_result (json, FALSE);
    }

    if (g_strcmp0 (name, "memory_bootstrap") == 0 ||
        g_strcmp0 (name, "memory_bootstrap_slim") == 0) {
        gboolean slim = g_str_has_suffix (name, "_slim");
        const gchar *fmt = _json_get_string (args, "format", "text");
        gint limit = (gint)_json_get_int (args, "limit", 5);

        if (g_strcmp0 (fmt, "json") == 0) {
            g_autofree gchar *json = bootstrap_json (app->conn, limit, slim);
            return _mcp_text_result (json, FALSE);
        } else {
            g_autofree gchar *text = bootstrap_text (app->conn, limit, slim);
            return _mcp_text_result (text, FALSE);
        }
    }

    g_autofree gchar *msg = g_strdup_printf ("Unknown tool: %s", name);
    return _mcp_text_result (msg, TRUE);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * MCP stdio server (via mcp-glib)
 * ═══════════════════════════════════════════════════════════════════════════ */

static void
on_client_disconnected (McpServer *server, gpointer user_data)
{
    GMainLoop *loop = (GMainLoop *)user_data;
    g_main_loop_quit (loop);
}

static gint
cmd_mcp (AppState *app)
{
    GMainLoop *loop = g_main_loop_new (NULL, FALSE);

    g_autoptr (McpServer) server = mcp_server_new ("agent-memories", "1.0.0");
    mcp_server_set_instructions (server,
        "Persistent searchable memory database for AI agents. "
        "Use memory_bootstrap or memory_bootstrap_slim at every session start "
        "to load context before engaging with tasks.");

    g_signal_connect (server, "client-disconnected",
                      G_CALLBACK (on_client_disconnected), loop);

    /* Register all 16 tools from the JSON schema definitions */
    g_autoptr (JsonParser) tools_parser = json_parser_new ();
    json_parser_load_from_data (tools_parser, MCP_TOOLS_JSON, -1, NULL);
    JsonArray *tools_arr = json_node_get_array (json_parser_get_root (tools_parser));

    guint i;

    for (i = 0; i < json_array_get_length (tools_arr); i++) {
        JsonNode *tool_node = json_array_get_element (tools_arr, i);
        GError *err = NULL;
        McpTool *tool = mcp_tool_new_from_json (tool_node, &err);
        if (tool) {
            mcp_server_add_tool (server, tool, mcp_handle_tool_call, app, NULL);
            g_object_unref (tool);
        } else {
            g_warning ("Failed to register tool from JSON schema: %s",
                       err ? err->message : "unknown error");
            g_clear_error (&err);
        }
    }

    /* Set stdio transport and start */
    g_autoptr (McpStdioTransport) transport = mcp_stdio_transport_new ();
    mcp_server_set_transport (server, MCP_TRANSPORT (transport));
    mcp_server_start_async (server, NULL, NULL, NULL);

    g_main_loop_run (loop);
    g_main_loop_unref (loop);
    return 0;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * CLI command handlers
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_add (AppState *app, gint argc, gchar **argv)
{
    g_autofree gchar *summary = NULL, *category = NULL, *subcategory = NULL;
    g_autofree gchar *importance = NULL, *source = NULL, *source_context = NULL;
    g_autofree gchar *conversation_id = NULL, *related_to = NULL;
    g_autofree gchar *metadata_str = NULL, *expires = NULL;
    gchar **tags = NULL;
    gboolean pin = FALSE;
    gdouble confidence = 1.0;

    GOptionEntry entries[] = {
        { "summary",         's', 0, G_OPTION_ARG_STRING,       &summary, "Short summary", "TEXT" },
        { "category",        'c', 0, G_OPTION_ARG_STRING,       &category, "Category", "CAT" },
        { "subcategory",       0, 0, G_OPTION_ARG_STRING,       &subcategory, "Subcategory", "SUB" },
        { "importance",      'i', 0, G_OPTION_ARG_STRING,       &importance, "Importance", "LVL" },
        { "source",            0, 0, G_OPTION_ARG_STRING,       &source, "Source", "SRC" },
        { "source-context",    0, 0, G_OPTION_ARG_STRING,       &source_context, "Source context", "CTX" },
        { "conversation-id",   0, 0, G_OPTION_ARG_STRING,       &conversation_id, "Conversation ID", "ID" },
        { "tag",             't', 0, G_OPTION_ARG_STRING_ARRAY, &tags, "Tag (repeatable)", "TAG" },
        { "related-to",        0, 0, G_OPTION_ARG_STRING,       &related_to, "Related memory UUID", "UUID" },
        { "pin",               0, 0, G_OPTION_ARG_NONE,         &pin, "Pin this memory", NULL },
        { "metadata",          0, 0, G_OPTION_ARG_STRING,       &metadata_str, "JSON metadata", "JSON" },
        { "confidence",        0, 0, G_OPTION_ARG_DOUBLE,       &confidence, "Confidence 0.0-1.0", "FLOAT" },
        { "expires",           0, 0, G_OPTION_ARG_STRING,       &expires, "Expiry (7d/2w/3m/1y or ISO)", "EXP" },
        { NULL }
    };

    g_autoptr (GOptionContext) ctx = g_option_context_new ("CONTENT");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message);
        g_error_free (error);
        return 1;
    }

    if (argc < 2) {
        g_printerr ("Error: content argument required (use '-' for stdin)\n");
        return 1;
    }

    const gchar *content = argv[1];
    g_autofree gchar *stdin_content = NULL;
    if (g_strcmp0 (content, "-") == 0) {
        GIOChannel *ch = g_io_channel_unix_new (STDIN_FILENO);
        g_io_channel_read_to_end (ch, &stdin_content, NULL, NULL);
        g_io_channel_unref (ch);
        content = g_strstrip (stdin_content);
        if (!content || !*content) {
            g_autofree gchar *msg = _red ("No content provided via stdin");
            g_printerr ("%s\n", msg);
            return 1;
        }
    }

    gint n_tags = tags ? (gint)g_strv_length (tags) : 0;
    g_autofree gchar *exp_str = expires ? parse_expiry (expires) : NULL;

    gchar *mid = db_add_memory (app, content, summary, category, subcategory,
        importance, source, source_context, conversation_id,
        tags, n_tags, related_to, pin, metadata_str, confidence, exp_str);
    g_strfreev (tags);

    if (mid) {
        g_autofree gchar *msg = _green (g_strdup_printf ("Memory created: %.8s", mid));
        g_print ("%s\n", msg);
        g_free (mid);
        return 0;
    }
    return 1;
}

static gint
cmd_show (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE;
    g_autofree gchar *format = NULL;

    GOptionEntry entries[] = {
        { "fzf",    0, 0, G_OPTION_ARG_NONE,   &use_fzf, "Select with fzf", NULL },
        { "format", 0, 0, G_OPTION_ARG_STRING,  &format, "Output format", "FMT" },
        { NULL }
    };

    g_autoptr (GOptionContext) ctx = g_option_context_new ("[ID]");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    const gchar *id = (argc >= 2) ? argv[1] : NULL;
    g_autofree gchar *selected_id = NULL;

    if (use_fzf || (!id && isatty (STDIN_FILENO))) {
        selected_id = fzf_select_memory (app->conn, "Select memory to view", FALSE);
        if (!selected_id) return 0;
        id = selected_id;
    }
    if (!id) {
        g_printerr ("Memory ID required (use --fzf for interactive selection)\n");
        return 1;
    }

    Memory *mem = db_get_memory (app->conn, id);
    if (!mem) {
        g_autofree gchar *msg = _red ("Memory not found");
        g_printerr ("%s: %s\n", msg, id);
        return 1;
    }

    if (format && g_strcmp0 (format, "json") == 0) {
        JsonNode *n = memory_to_json_node (mem);
        g_autofree gchar *json = json_node_to_string_pretty (n);
        g_print ("%s\n", json);
        json_node_unref (n);
    } else {
        print_memory_detail (mem);
    }
    memory_free (mem);
    return 0;
}

static gint
cmd_list (AppState *app, gint argc, gchar **argv)
{
    g_autofree gchar *category = NULL, *subcategory = NULL, *importance = NULL;
    g_autofree gchar *source = NULL, *cols = NULL, *format = NULL, *sort = NULL;
    gchar **tags = NULL;
    gboolean pinned = FALSE, include_archived = FALSE, only_archived = FALSE, ascending = FALSE, all = FALSE;
    gint limit = 50, offset = 0;

    GOptionEntry entries[] = {
        { "cols",             0, 0, G_OPTION_ARG_STRING,       &cols, "Columns", "COLS" },
        { "format",           0, 0, G_OPTION_ARG_STRING,       &format, "Output format", "FMT" },
        { "category",         0, 0, G_OPTION_ARG_STRING,       &category, "Filter category", "CAT" },
        { "subcategory",      0, 0, G_OPTION_ARG_STRING,       &subcategory, "Filter subcategory", "SUB" },
        { "importance",       0, 0, G_OPTION_ARG_STRING,       &importance, "Filter importance", "IMP" },
        { "source",           0, 0, G_OPTION_ARG_STRING,       &source, "Filter source", "SRC" },
        { "tag",            't', 0, G_OPTION_ARG_STRING_ARRAY, &tags, "Filter tag", "TAG" },
        { "pinned",           0, 0, G_OPTION_ARG_NONE,         &pinned, "Only pinned", NULL },
        { "include-archived", 0, 0, G_OPTION_ARG_NONE,         &include_archived, "Include archived", NULL },
        { "only-archived",    0, 0, G_OPTION_ARG_NONE,         &only_archived, "Only archived", NULL },
        { "all",              0, 0, G_OPTION_ARG_NONE,         &all, "List all (no limit)", NULL },
        { "limit",            0, 0, G_OPTION_ARG_INT,          &limit, "Max results (default 50)", "N" },
        { "offset",           0, 0, G_OPTION_ARG_INT,          &offset, "Offset", "N" },
        { "sort",             0, 0, G_OPTION_ARG_STRING,       &sort, "Sort column", "COL" },
        { "ascending",        0, 0, G_OPTION_ARG_NONE,         &ascending, "Sort ascending", NULL },
        { NULL }
    };

    g_autoptr (GOptionContext) ctx = g_option_context_new (NULL);
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    gint n_tags = tags ? (gint)g_strv_length (tags) : 0;
    GPtrArray *mems = db_list_memories (app->conn, category, subcategory, importance,
        tags, n_tags, source, pinned ? 1 : -1,
        include_archived, only_archived, all ? -1 : limit, offset,
        sort ? sort : "created_at", !ascending);
    g_strfreev (tags);

    if (format && g_strcmp0 (format, "json") == 0) {
        g_autofree gchar *json = memory_array_to_json (mems);
        g_print ("%s\n", json);
    } else if (format && (g_strcmp0 (format, "markdown") == 0 || g_strcmp0 (format, "md") == 0)) {
        guint i;
        for (i = 0; i < mems->len; i++) {
            Memory *m = g_ptr_array_index (mems, i);
            g_print ("- **%.8s** [%s] %s\n", m->id, m->category ? m->category : "",
                     m->summary ? m->summary : "");
        }
    } else {
        /* Parse custom columns if provided */
        const gchar **custom_cols = NULL;
        if (cols) {
            gchar **split = g_strsplit (cols, ",", -1);
            gint n = g_strv_length (split);
            custom_cols = g_new0 (const gchar *, n + 1);
            gint i;
            for (i = 0; i < n; i++) {
                g_strstrip (split[i]);
                custom_cols[i] = split[i];
            }
            print_memory_table (mems, custom_cols);
            g_free (custom_cols);
            g_strfreev (split);
        } else {
            print_memory_table (mems, NULL);
        }
    }
    g_ptr_array_unref (mems);
    return 0;
}

static gint
cmd_search (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE, exact = FALSE, semantic = FALSE;
    gboolean include_archived = FALSE;
    g_autofree gchar *category = NULL, *format = NULL;
    gint limit = 25;

    GOptionEntry entries[] = {
        { "fzf",              0, 0, G_OPTION_ARG_NONE,   &use_fzf, "Interactive fzf search", NULL },
        { "exact",            0, 0, G_OPTION_ARG_NONE,   &exact, "Exact tsvector search", NULL },
        { "semantic",         0, 0, G_OPTION_ARG_NONE,   &semantic, "Semantic similarity search", NULL },
        { "category",         0, 0, G_OPTION_ARG_STRING, &category, "Filter category", "CAT" },
        { "include-archived", 0, 0, G_OPTION_ARG_NONE,   &include_archived, "Include archived", NULL },
        { "limit",            0, 0, G_OPTION_ARG_INT,    &limit, "Max results", "N" },
        { "format",           0, 0, G_OPTION_ARG_STRING, &format, "Output format", "FMT" },
        { NULL }
    };

    g_autoptr (GOptionContext) ctx = g_option_context_new ("[QUERY]");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    const gchar *query = (argc >= 2) ? argv[1] : NULL;

    if (use_fzf) {
        g_autofree gchar *selected = fzf_search_memory (app->conn, query ? query : "");
        if (selected) {
            Memory *mem = db_get_memory (app->conn, selected);
            if (mem) { print_memory_detail (mem); memory_free (mem); }
        }
        return 0;
    }

    if (!query) {
        g_autofree gchar *msg = _red ("Search query required");
        g_printerr ("%s\n", msg);
        return 1;
    }

    GPtrArray *results;
    if (semantic)
        results = db_search_semantic (app, query, category, include_archived, limit);
    else if (exact)
        results = db_search_exact (app->conn, query, category, include_archived, limit);
    else
        results = db_search_fuzzy (app->conn, query, category, include_archived, limit);

    if (format && g_strcmp0 (format, "json") == 0) {
        g_autofree gchar *json = memory_array_to_json (results);
        g_print ("%s\n", json);
    } else {
        const gchar *sem_cols[] = { "id", "similarity", "category", "importance", "summary", "tags", NULL };
        print_memory_table (results, semantic ? sem_cols : NULL);
    }
    g_ptr_array_unref (results);
    return 0;
}

/* ── Edit helper: parse simple YAML-like format from editor ───────────── */

static GHashTable *
_parse_edited_text (const gchar *text)
{
    GHashTable *fields = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
    g_auto (GStrv) lines = g_strsplit (text, "\n", -1);
    GString *multiline_val = NULL;
    gchar *multiline_key = NULL;

    gint i;

    for (i = 0; lines[i]; i++) {
        const gchar *line = lines[i];

        /* Skip comment lines */
        if (g_str_has_prefix (g_strstrip ((gchar *)line), "#"))
            continue;

        /* Check if this is a continuation of a multiline block (indented) */
        if (multiline_key && (line[0] == ' ' || line[0] == '\t' || line[0] == '\0')) {
            if (multiline_val->len > 0)
                g_string_append_c (multiline_val, '\n');
            /* Trim leading 2 spaces of indent if present */
            if (line[0] == ' ' && line[1] == ' ')
                g_string_append (multiline_val, line + 2);
            else
                g_string_append (multiline_val, line);
            continue;
        }

        /* Flush previous multiline block */
        if (multiline_key) {
            gchar *val = g_strstrip (g_string_free (multiline_val, FALSE));
            if (val[0] != '\0')
                g_hash_table_insert (fields, multiline_key, val);
            else {
                g_free (multiline_key);
                g_free (val);
            }
            multiline_key = NULL;
            multiline_val = NULL;
        }

        /* Parse "key: value" */
        const gchar *colon = strchr (line, ':');
        if (!colon) continue;

        gchar *key = g_strndup (line, colon - line);
        g_strstrip (key);
        const gchar *rest = colon + 1;

        /* Check for YAML block scalar "key: |" */
        g_autofree gchar *rest_stripped = g_strstrip (g_strdup (rest));
        if (g_strcmp0 (rest_stripped, "|") == 0) {
            multiline_key = key;
            multiline_val = g_string_new (NULL);
            continue;
        }

        gchar *value = g_strstrip (g_strdup (rest));
        if (value[0] != '\0')
            g_hash_table_insert (fields, key, value);
        else {
            g_free (key);
            g_free (value);
        }
    }

    /* Flush trailing multiline */
    if (multiline_key) {
        gchar *val = g_strstrip (g_string_free (multiline_val, FALSE));
        if (val[0] != '\0')
            g_hash_table_insert (fields, multiline_key, val);
        else {
            g_free (multiline_key);
            g_free (val);
        }
    }

    return fields;
}

static gchar *
_memory_to_editable_text (Memory *mem)
{
    GString *s = g_string_new (NULL);
    g_string_append_printf (s, "# Editing memory %.*s\n", 8, mem->id);
    g_string_append (s, "# Save and close to apply changes. Delete all content to cancel.\n\n");

    g_string_append_printf (s, "summary: %s\n", mem->summary ? mem->summary : "");
    g_string_append_printf (s, "category: %s\n", mem->category ? mem->category : "general");
    g_string_append_printf (s, "subcategory: %s\n", mem->subcategory ? mem->subcategory : "");
    g_string_append_printf (s, "importance: %s\n", mem->importance ? mem->importance : "normal");
    g_string_append_printf (s, "source: %s\n", mem->source ? mem->source : "");

    /* Tags as comma-separated list */
    if (mem->tags && mem->tags[0]) {
        g_autofree gchar *tags_str = g_strjoinv (", ", mem->tags);
        g_string_append_printf (s, "tags: %s\n", tags_str);
    } else {
        g_string_append (s, "tags:\n");
    }

    g_string_append_printf (s, "is_pinned: %s\n", mem->is_pinned ? "true" : "false");

    /* Content as YAML block scalar */
    g_string_append (s, "content: |\n");
    if (mem->content) {
        g_auto (GStrv) clines = g_strsplit (mem->content, "\n", -1);
        gint i;
        for (i = 0; clines[i]; i++)
            g_string_append_printf (s, "  %s\n", clines[i]);
    }

    return g_string_free (s, FALSE);
}

static gint
cmd_edit (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE;
    GOptionEntry entries[] = {
        { "fzf", 0, 0, G_OPTION_ARG_NONE, &use_fzf, "Use fzf", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new ("[ID]");
    g_option_context_add_main_entries (ctx, entries, NULL);
    g_option_context_parse (ctx, &argc, &argv, NULL);

    const gchar *id = (argc > 1) ? argv[1] : NULL;
    g_autofree gchar *selected = NULL;

    if (use_fzf || (!id && isatty (STDIN_FILENO))) {
        selected = fzf_select_memory (app->conn, "Select memory to edit", FALSE);
        if (!selected) return 0;
        id = selected;
    }
    if (!id) {
        g_autofree gchar *msg = _red ("Memory ID required (use --fzf for interactive selection)");
        g_printerr ("%s\n", msg);
        return 1;
    }

    g_autoptr (Memory) mem = db_get_memory (app->conn, id);
    if (!mem) {
        g_autofree gchar *msg = _red ("Memory not found");
        g_printerr ("%s\n", msg);
        return 1;
    }

    /* Write editable representation to temp file */
    g_autofree gchar *edit_text = _memory_to_editable_text (mem);
    g_autofree gchar *tmpfile = NULL;
    gint fd = g_file_open_tmp ("agent_mem_XXXXXX.yaml", &tmpfile, NULL);
    if (fd < 0) {
        g_printerr ("Failed to create temp file\n");
        return 1;
    }
    write (fd, edit_text, strlen (edit_text));
    close (fd);

    /* Open in editor */
    const gchar *editor = g_getenv ("EDITOR");
    if (!editor) editor = "vi";

    gchar *editor_cmd = g_strdup_printf ("%s %s", editor, tmpfile);
    gint status = system (editor_cmd);
    g_free (editor_cmd);

    if (status != 0) {
        g_printerr ("Editor exited with error\n");
        g_unlink (tmpfile);
        return 1;
    }

    /* Read edited content */
    g_autofree gchar *edited = NULL;
    gsize len;
    if (!g_file_get_contents (tmpfile, &edited, &len, NULL)) {
        g_printerr ("Failed to read edited file\n");
        g_unlink (tmpfile);
        return 1;
    }
    g_unlink (tmpfile);

    /* Strip comment lines and check for empty */
    GString *clean = g_string_new (NULL);
    g_auto (GStrv) elines = g_strsplit (edited, "\n", -1);
    gint i;
    for (i = 0; elines[i]; i++) {
        if (!g_str_has_prefix (g_strstrip (elines[i]), "#"))
            g_string_append_printf (clean, "%s\n", elines[i]);
    }
    g_autofree gchar *clean_text = g_strstrip (g_string_free (clean, FALSE));

    if (clean_text[0] == '\0') {
        g_autofree gchar *msg = _yellow ("Edit cancelled (empty content)");
        g_print ("%s\n", msg);
        return 0;
    }

    /* Parse the edited fields */
    GHashTable *fields = _parse_edited_text (edited);

    /* Handle tags specially — convert "tag1, tag2" to "{tag1,tag2}" */
    gchar *tags_raw = g_hash_table_lookup (fields, "tags");
    if (tags_raw) {
        g_auto (GStrv) tag_parts = g_strsplit (tags_raw, ",", -1);
        GString *pg_arr = g_string_new ("{");
        gint i;
        for (i = 0; tag_parts[i]; i++) {
            gchar *t = g_strstrip (tag_parts[i]);
            if (t[0] == '\0') continue;
            if (pg_arr->len > 1) g_string_append_c (pg_arr, ',');
            g_string_append (pg_arr, t);
        }
        g_string_append_c (pg_arr, '}');
        g_hash_table_insert (fields, g_strdup ("tags"), g_string_free (pg_arr, FALSE));
    }

    /* Handle is_pinned as boolean string */
    gchar *pinned = g_hash_table_lookup (fields, "is_pinned");
    if (pinned) {
        const gchar *val = (g_ascii_strcasecmp (pinned, "true") == 0) ? "true" : "false";
        g_hash_table_insert (fields, g_strdup ("is_pinned"), g_strdup (val));
    }

    if (g_hash_table_size (fields) == 0) {
        g_autofree gchar *msg = _yellow ("No changes detected");
        g_print ("%s\n", msg);
        g_hash_table_unref (fields);
        return 0;
    }

    gboolean ok = db_update_memory (app, mem->id, fields);
    g_hash_table_unref (fields);

    if (ok) {
        g_autofree gchar *msg = _green ("Memory updated");
        g_print ("%s %.*s\n", msg, 8, mem->id);
    } else {
        g_autofree gchar *msg = _red ("Failed to update memory");
        g_printerr ("%s\n", msg);
        return 1;
    }
    return 0;
}

static gint
cmd_archive (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE, unarchive = FALSE;
    GOptionEntry entries[] = {
        { "fzf",       0, 0, G_OPTION_ARG_NONE, &use_fzf, "Select with fzf", NULL },
        { "unarchive", 0, 0, G_OPTION_ARG_NONE, &unarchive, "Unarchive instead", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new ("[ID]");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    const gchar *id = (argc >= 2) ? argv[1] : NULL;
    g_autofree gchar *selected = NULL;
    if (use_fzf || (!id && isatty (STDIN_FILENO))) {
        selected = fzf_select_memory (app->conn, unarchive ? "Unarchive" : "Archive", unarchive);
        if (!selected) return 0;
        id = selected;
    }
    if (!id) { g_printerr ("Memory ID required\n"); return 1; }

    GHashTable *f = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, g_free);
    g_hash_table_insert (f, "is_archived", g_strdup (unarchive ? "false" : "true"));
    gboolean ok = db_update_memory (app, id, f);
    g_hash_table_unref (f);

    if (ok) {
        g_autofree gchar *msg = _green (g_strdup_printf ("Memory %.8s %s", id,
            unarchive ? "unarchived" : "archived"));
        g_print ("%s\n", msg);
    } else {
        g_autofree gchar *msg = _red ("Memory not found");
        g_printerr ("%s: %s\n", msg, id);
        return 1;
    }
    return 0;
}

static gint
cmd_pin (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE, unpin = FALSE;
    GOptionEntry entries[] = {
        { "fzf",   0, 0, G_OPTION_ARG_NONE, &use_fzf, "Select with fzf", NULL },
        { "unpin", 0, 0, G_OPTION_ARG_NONE, &unpin, "Unpin instead", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new ("[ID]");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    const gchar *id = (argc >= 2) ? argv[1] : NULL;
    g_autofree gchar *selected = NULL;
    if (use_fzf || (!id && isatty (STDIN_FILENO))) {
        selected = fzf_select_memory (app->conn, unpin ? "Unpin" : "Pin", FALSE);
        if (!selected) return 0;
        id = selected;
    }
    if (!id) { g_printerr ("Memory ID required\n"); return 1; }

    GHashTable *f = g_hash_table_new_full (g_str_hash, g_str_equal, NULL, g_free);
    g_hash_table_insert (f, "is_pinned", g_strdup (unpin ? "false" : "true"));
    gboolean ok = db_update_memory (app, id, f);
    g_hash_table_unref (f);

    if (ok) {
        g_autofree gchar *msg = _green (g_strdup_printf ("Memory %.8s %s", id,
            unpin ? "unpinned" : "pinned"));
        g_print ("%s\n", msg);
    } else {
        g_autofree gchar *msg = _red ("Memory not found");
        g_printerr ("%s: %s\n", msg, id);
        return 1;
    }
    return 0;
}

static gint
cmd_rm (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE, force = FALSE;
    GOptionEntry entries[] = {
        { "fzf",   0, 0, G_OPTION_ARG_NONE, &use_fzf, "Select with fzf", NULL },
        { "force", 'f', 0, G_OPTION_ARG_NONE, &force, "Skip confirmation", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new ("[ID]");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    const gchar *id = (argc >= 2) ? argv[1] : NULL;
    g_autofree gchar *selected = NULL;
    if (use_fzf || (!id && isatty (STDIN_FILENO))) {
        selected = fzf_select_memory (app->conn, "Select memory to DELETE", FALSE);
        if (!selected) return 0;
        id = selected;
    }
    if (!id) { g_printerr ("Memory ID required\n"); return 1; }

    if (!force) {
        Memory *mem = db_get_memory (app->conn, id);
        if (mem) {
            g_autofree gchar *bold_sum = _bold (mem->summary ? mem->summary : "");
            g_print ("About to delete: %s\n", bold_sum);
            g_print ("Are you sure? [y/N] ");
            gchar buf[16];
            if (!fgets (buf, sizeof (buf), stdin) || (buf[0] != 'y' && buf[0] != 'Y')) {
                g_autofree gchar *msg = _yellow ("Cancelled");
                g_print ("%s\n", msg);
                memory_free (mem);
                return 0;
            }
            memory_free (mem);
        }
    }

    if (db_delete_memory (app->conn, id)) {
        g_autofree gchar *msg = _green (g_strdup_printf ("Memory %.8s deleted", id));
        g_print ("%s\n", msg);
    } else {
        g_autofree gchar *msg = _red ("Memory not found");
        g_printerr ("%s: %s\n", msg, id);
        return 1;
    }
    return 0;
}

static gint
cmd_tag (AppState *app, gint argc, gchar **argv)
{
    gboolean use_fzf = FALSE, remove = FALSE;
    GOptionEntry entries[] = {
        { "fzf",    0, 0, G_OPTION_ARG_NONE, &use_fzf, "Select with fzf", NULL },
        { "remove", 'r', 0, G_OPTION_ARG_NONE, &remove, "Remove tags", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new ("[ID] TAG...");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }

    const gchar *id = (argc >= 2) ? argv[1] : NULL;
    g_autofree gchar *selected = NULL;
    if (use_fzf || (!id && isatty (STDIN_FILENO))) {
        selected = fzf_select_memory (app->conn, "Select memory to tag", FALSE);
        if (!selected) return 0;
        id = selected;
    }
    if (!id) { g_printerr ("Memory ID required\n"); return 1; }

    g_autofree gchar *full_id = db_resolve_uuid (app->conn, id);
    if (!full_id) {
        g_autofree gchar *msg = _red ("Memory not found");
        g_printerr ("%s: %s\n", msg, id);
        return 1;
    }

    /* Remaining args are tags (skip the id which is argv[1]) */
    gint tag_start = (selected) ? 1 : 2; /* If fzf selected, tags start at argv[1] */
    if (selected) tag_start = (argc >= 2) ? 1 : 0;
    else tag_start = (argc >= 3) ? 2 : 0;

    if (tag_start == 0 || tag_start >= argc) {
        g_printerr ("Tag names required\n");
        return 1;
    }

    gint i;

    for (i = tag_start; i < argc; i++) {
        const gchar *params[] = { argv[i], full_id };
        PGresult *res;
        if (remove)
            res = PQexecParams (app->conn,
                "UPDATE memories SET tags = array_remove(tags, $1) WHERE id = $2::uuid",
                2, NULL, params, NULL, NULL, 0);
        else
            res = PQexecParams (app->conn,
                "UPDATE memories SET tags = CASE WHEN NOT ($1 = ANY(tags)) "
                "THEN array_append(tags, $1) ELSE tags END WHERE id = $2::uuid",
                2, NULL, params, NULL, NULL, 0);
        PQclear (res);
    }

    g_autofree gchar *msg = _green (g_strdup_printf ("%s tags %s %.8s",
        remove ? "Removed" : "Added", remove ? "from" : "to", full_id));
    g_print ("%s\n", msg);
    return 0;
}

static gint
cmd_stats (AppState *app, gint argc, gchar **argv)
{
    g_autofree gchar *format = NULL;
    GOptionEntry entries[] = {
        { "format", 0, 0, G_OPTION_ARG_STRING, &format, "Output format", "FMT" },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new (NULL);
    g_option_context_add_main_entries (ctx, entries, NULL);
    g_option_context_parse (ctx, &argc, &argv, NULL);

    JsonNode *stats = db_get_stats (app->conn);

    if (format && g_strcmp0 (format, "json") == 0) {
        g_autofree gchar *json = json_node_to_string_pretty (stats);
        g_print ("%s\n", json);
    } else {
        JsonObject *obj = json_node_get_object (stats);
        g_autofree gchar *hdr = _bold ("━━━ Agent Memory Statistics ━━━");
        g_print ("%s\n", hdr);
        g_print ("  Total memories:    %ld\n", (long)json_object_get_int_member (obj, "total"));
        g_print ("  Active:            %ld\n", (long)json_object_get_int_member (obj, "active"));
        g_print ("  Archived:          %ld\n", (long)json_object_get_int_member (obj, "archived"));
        g_print ("  Pinned:            %ld\n", (long)json_object_get_int_member (obj, "pinned"));
        g_print ("\n");

        JsonArray *by_cat = json_object_get_array_member (obj, "by_category");
        if (json_array_get_length (by_cat) > 0) {
            g_autofree gchar *h = _bold ("  By Category:");
            g_print ("%s\n", h);
            guint i;
            for (i = 0; i < json_array_get_length (by_cat); i++) {
                JsonObject *r = json_array_get_object_element (by_cat, i);
                g_print ("    %-15s %ld\n",
                    json_object_get_string_member (r, "category"),
                    (long)json_object_get_int_member (r, "count"));
            }
            g_print ("\n");
        }

        JsonArray *by_imp = json_object_get_array_member (obj, "by_importance");
        if (json_array_get_length (by_imp) > 0) {
            g_autofree gchar *h = _bold ("  By Importance:");
            g_print ("%s\n", h);
            guint i;
            for (i = 0; i < json_array_get_length (by_imp); i++) {
                JsonObject *r = json_array_get_object_element (by_imp, i);
                g_print ("    %-15s %ld\n",
                    json_object_get_string_member (r, "importance"),
                    (long)json_object_get_int_member (r, "count"));
            }
            g_print ("\n");
        }

        JsonArray *top_tags = json_object_get_array_member (obj, "top_tags");
        if (json_array_get_length (top_tags) > 0) {
            g_autofree gchar *h = _bold ("  Top Tags:");
            g_print ("%s\n", h);
            guint i;
            for (i = 0; i < json_array_get_length (top_tags); i++) {
                JsonObject *r = json_array_get_object_element (top_tags, i);
                g_print ("    %-20s %ld\n",
                    json_object_get_string_member (r, "tag"),
                    (long)json_object_get_int_member (r, "count"));
            }
            g_print ("\n");
        }

        JsonArray *most = json_object_get_array_member (obj, "most_accessed");
        if (json_array_get_length (most) > 0) {
            g_autofree gchar *h = _bold ("  Most Accessed:");
            g_print ("%s\n", h);
            guint i;
            for (i = 0; i < json_array_get_length (most); i++) {
                JsonObject *r = json_array_get_object_element (most, i);
                g_print ("    %.8s (%ldx) %.50s\n",
                    json_object_get_string_member (r, "id"),
                    (long)json_object_get_int_member (r, "access_count"),
                    json_object_get_string_member (r, "summary"));
            }
        }
    }
    json_node_unref (stats);
    return 0;
}

static gint
cmd_export (AppState *app, gint argc, gchar **argv)
{
    g_autofree gchar *output_file = NULL, *format = NULL;
    GOptionEntry entries[] = {
        { "output", 'o', 0, G_OPTION_ARG_STRING, &output_file, "Output file", "FILE" },
        { "format",   0, 0, G_OPTION_ARG_STRING, &format, "Format", "FMT" },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new (NULL);
    g_option_context_add_main_entries (ctx, entries, NULL);
    g_option_context_parse (ctx, &argc, &argv, NULL);

    GPtrArray *mems = db_list_memories (app->conn, NULL, NULL, NULL, NULL, 0, NULL,
        -1, TRUE, FALSE, 100000, 0, "created_at", TRUE);
    g_autofree gchar *json = memory_array_to_json (mems);

    if (output_file) {
        GError *error = NULL;
        g_file_set_contents (output_file, json, -1, &error);
        if (error) {
            g_printerr ("Write failed: %s\n", error->message);
            g_error_free (error);
            g_ptr_array_unref (mems);
            return 1;
        }
        g_autofree gchar *msg = _green (g_strdup_printf ("Exported %u memories to %s",
            mems->len, output_file));
        g_print ("%s\n", msg);
    } else {
        g_print ("%s\n", json);
    }
    g_ptr_array_unref (mems);
    return 0;
}

static gint
cmd_import (AppState *app, gint argc, gchar **argv)
{
    g_autofree gchar *format = NULL;
    GOptionEntry entries[] = {
        { "format", 0, 0, G_OPTION_ARG_STRING, &format, "File format", "FMT" },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new ("FILE");
    g_option_context_add_main_entries (ctx, entries, NULL);
    GError *error = NULL;
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("Error: %s\n", error->message); g_error_free (error); return 1;
    }
    if (argc < 2) { g_printerr ("Input file required\n"); return 1; }

    g_autofree gchar *contents = NULL;
    if (!g_file_get_contents (argv[1], &contents, NULL, &error)) {
        g_printerr ("Read failed: %s\n", error->message);
        g_error_free (error);
        return 1;
    }

    JsonParser *parser = json_parser_new ();
    if (!json_parser_load_from_data (parser, contents, -1, &error)) {
        g_printerr ("JSON parse failed: %s\n", error->message);
        g_error_free (error);
        g_object_unref (parser);
        return 1;
    }

    JsonArray *arr = json_node_get_array (json_parser_get_root (parser));
    gint count = 0;
    guint i;
    for (i = 0; i < json_array_get_length (arr); i++) {
        JsonObject *mo = json_array_get_object_element (arr, i);
        const gchar *content = _json_get_string (mo, "content", "");
        const gchar *summary = _json_get_string (mo, "summary", NULL);
        const gchar *cat = _json_get_string (mo, "category", "general");
        const gchar *imp = _json_get_string (mo, "importance", "normal");
        const gchar *src = _json_get_string (mo, "source", NULL);

        gchar *mid = db_add_memory (app, content, summary, cat, NULL, imp,
            src, NULL, NULL, NULL, 0, NULL, FALSE, NULL, 1.0, NULL);
        g_free (mid);
        count++;
    }
    g_object_unref (parser);
    g_autofree gchar *msg = _green (g_strdup_printf ("Imported %d memories", count));
    g_print ("%s\n", msg);
    return 0;
}

static gint
cmd_prune (AppState *app, gint argc, gchar **argv)
{
    gboolean do_delete = FALSE, dry_run = FALSE;
    GOptionEntry entries[] = {
        { "delete",  0, 0, G_OPTION_ARG_NONE, &do_delete, "Delete instead of archive", NULL },
        { "dry-run", 0, 0, G_OPTION_ARG_NONE, &dry_run, "Preview only", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new (NULL);
    g_option_context_add_main_entries (ctx, entries, NULL);
    g_option_context_parse (ctx, &argc, &argv, NULL);

    PGresult *cnt = PQexec (app->conn,
        "SELECT COUNT(*) FROM memories WHERE expires_at IS NOT NULL "
        "AND expires_at < CURRENT_TIMESTAMP AND is_archived = FALSE");
    gint expired = (PQresultStatus (cnt) == PGRES_TUPLES_OK) ? atoi (PQgetvalue (cnt, 0, 0)) : 0;
    PQclear (cnt);

    if (expired == 0) { g_print ("No expired memories to prune.\n"); return 0; }

    if (dry_run) {
        g_print ("Would prune %d expired memories:\n", expired);
        PGresult *res = PQexec (app->conn,
            "SELECT id, summary, expires_at FROM memories WHERE expires_at IS NOT NULL "
            "AND expires_at < CURRENT_TIMESTAMP AND is_archived = FALSE ORDER BY expires_at");
        gint i;
        for (i = 0; i < PQntuples (res); i++)
            g_print ("  [%.8s] expired %s: %s\n",
                PQgetvalue (res, i, 0), PQgetvalue (res, i, 2),
                PQgetvalue (res, i, 1)[0] ? PQgetvalue (res, i, 1) : "(no summary)");
        PQclear (res);
        return 0;
    }

    PGresult *res;
    if (do_delete)
        res = PQexec (app->conn,
            "DELETE FROM memories WHERE expires_at IS NOT NULL "
            "AND expires_at < CURRENT_TIMESTAMP");
    else
        res = PQexec (app->conn,
            "UPDATE memories SET is_archived = TRUE WHERE expires_at IS NOT NULL "
            "AND expires_at < CURRENT_TIMESTAMP AND is_archived = FALSE");

    gint affected = atoi (PQcmdTuples (res));
    PQclear (res);
    g_autofree gchar *msg = _green (g_strdup_printf ("%s %d expired memories",
        do_delete ? "Deleted" : "Archived", affected));
    g_print ("%s\n", msg);
    return 0;
}

static gint
cmd_backfill (AppState *app, gint argc, gchar **argv)
{
    gboolean include_archived = FALSE;
    GOptionEntry entries[] = {
        { "include-archived", 0, 0, G_OPTION_ARG_NONE, &include_archived, "Include archived", NULL },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new (NULL);
    g_option_context_add_main_entries (ctx, entries, NULL);
    g_option_context_parse (ctx, &argc, &argv, NULL);

    const gchar *where = include_archived
        ? "embedding IS NULL"
        : "embedding IS NULL AND is_archived = FALSE";
    g_autofree gchar *sql = g_strdup_printf (
        "SELECT id, content, summary FROM memories WHERE %s ORDER BY created_at ASC", where);

    PGresult *res = PQexec (app->conn, sql);
    gint total = PQntuples (res);
    if (total == 0) { g_print ("All memories already have embeddings.\n"); PQclear (res); return 0; }

    g_print ("Backfilling embeddings for %d memories...\n", total);
    gint success = 0, failed = 0;

    gint i;

    for (i = 0; i < total; i++) {
        const gchar *id = PQgetvalue (res, i, 0);
        const gchar *content = PQgetvalue (res, i, 1);
        const gchar *summary = PQgetvalue (res, i, 2);
        g_autofree gchar *embed_text = g_strdup_printf ("%s %s", summary ? summary : "", content);
        g_autofree gchar *embedding = generate_embedding_sync (
            app->soup, app->ollama_url, app->embed_model, embed_text);
        if (embedding) {
            const gchar *up[] = { embedding, id };
            PGresult *u = PQexecParams (app->conn,
                "UPDATE memories SET embedding = $1 WHERE id = $2::uuid",
                2, NULL, up, NULL, NULL, 0);
            PQclear (u);
            success++;
        } else {
            failed++;
            if (failed == 1) {
                g_autofree gchar *msg = _yellow ("Warning: ollama may be unavailable");
                g_printerr ("%s\n", msg);
            }
        }
        if ((i + 1) % 10 == 0 || i == total - 1)
            g_print ("  [%d/%d] %d embedded, %d failed\r", i + 1, total, success, failed);
    }
    PQclear (res);

    g_autofree gchar *ok = _green (g_strdup_printf ("Done: %d embedded", success));
    g_autofree gchar *fail = failed ? _yellow (g_strdup_printf ("%d failed", failed)) : g_strdup ("0 failed");
    g_print ("\n%s, %s\n", ok, fail);
    return 0;
}

static gint
cmd_bootstrap_cli (AppState *app, gint argc, gchar **argv)
{
    g_autofree gchar *format = NULL;
    gint limit = 5;
    GOptionEntry entries[] = {
        { "format", 0, 0, G_OPTION_ARG_STRING, &format, "Output format", "FMT" },
        { "limit",  0, 0, G_OPTION_ARG_INT,    &limit, "Recent per category", "N" },
        { NULL }
    };
    g_autoptr (GOptionContext) ctx = g_option_context_new (NULL);
    g_option_context_add_main_entries (ctx, entries, NULL);
    g_option_context_parse (ctx, &argc, &argv, NULL);

    if (format && g_strcmp0 (format, "json") == 0) {
        g_autofree gchar *json = bootstrap_json (app->conn, limit, FALSE);
        g_print ("%s\n", json);
    } else {
        g_autofree gchar *text = bootstrap_text (app->conn, limit, FALSE);
        g_print ("%s", text);
    }
    return 0;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Main
 * ═══════════════════════════════════════════════════════════════════════════ */

static void
print_help (void)
{
    g_print (
        "agent_memories — searchable, persistent memory for AI agents\n\n"
        "Usage: agent_memories <command> [options]\n\n"
        "Commands:\n"
        "  add          Add a new memory\n"
        "  show         Show memory details\n"
        "  list         List memories with filters\n"
        "  search       Search memories (fuzzy/exact/semantic)\n"
        "  edit         Edit memory in $EDITOR\n"
        "  archive      Archive/unarchive a memory\n"
        "  pin          Pin/unpin a memory\n"
        "  rm           Permanently delete a memory\n"
        "  tag          Add/remove tags\n"
        "  stats        Show statistics\n"
        "  export       Export all memories\n"
        "  import       Import memories from file\n"
        "  prune        Archive/delete expired memories\n"
        "  backfill     Generate missing embeddings\n"
        "  bootstrap    Output bootstrap context\n"
        "  mcp          Run MCP stdio server\n\n"
        "Categories: general, decision, preference, fact, project, learning,\n"
        "  insight, todo, relationship, technical, workflow, debug, research,\n"
        "  config, personal\n\n"
        "Importance: low, normal, high, critical\n\n"
        "Examples:\n"
        "  agent_memories add \"Zach prefers YAML\" -c preference -t config\n"
        "  agent_memories search \"yaml preference\"\n"
        "  agent_memories search --semantic \"coding style preferences\"\n"
        "  agent_memories list --category decision --importance high\n"
        "  agent_memories show --fzf\n"
        "  agent_memories stats\n"
        "  agent_memories bootstrap --format json\n"
    );
}

gint
main (gint argc, gchar **argv)
{
    dex_init ();
    g_no_color = g_getenv ("NO_COLOR") != NULL;

    if (argc < 2 || g_strcmp0 (argv[1], "--help") == 0 || g_strcmp0 (argv[1], "-h") == 0) {
        print_help ();
        return 0;
    }

    if (g_strcmp0 (argv[1], "--license") == 0) {
        g_print ("agent_memories — AGPLv3\n"
                 "Copyright (C) 2026  Zach Podbielniak\n"
                 "https://www.gnu.org/licenses/agpl-3.0.html\n");
        return 0;
    }

    const gchar *command = argv[1];

    /* Shift argv past the subcommand */
    gint sub_argc = argc - 1;
    gchar **sub_argv = argv + 1;

    /* Connect to database */
    PGconn *conn = db_connect ();
    if (!conn) return 1;

    /* Set up HTTP session for Ollama */
    SoupSession *soup = soup_session_new ();
    soup_session_set_timeout (soup, 30);

    const gchar *ollama_url = g_getenv ("OLLAMA_URL");
    const gchar *embed_model = g_getenv ("EMBED_MODEL");

    AppState app = {
        .conn = conn,
        .soup = soup,
        .ollama_url = (gchar *)(ollama_url ? ollama_url : "http://127.0.0.1:11434"),
        .embed_model = (gchar *)(embed_model ? embed_model : "nomic-embed-text"),
        .no_color = g_no_color,
    };

    gint ret = 0;

    if (g_strcmp0 (command, "add") == 0)            ret = cmd_add (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "show") == 0)       ret = cmd_show (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "list") == 0)       ret = cmd_list (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "search") == 0)     ret = cmd_search (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "edit") == 0)       ret = cmd_edit (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "archive") == 0)    ret = cmd_archive (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "pin") == 0)        ret = cmd_pin (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "rm") == 0)         ret = cmd_rm (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "tag") == 0)        ret = cmd_tag (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "stats") == 0)      ret = cmd_stats (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "export") == 0)     ret = cmd_export (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "import") == 0)     ret = cmd_import (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "prune") == 0)      ret = cmd_prune (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "backfill") == 0)   ret = cmd_backfill (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "bootstrap") == 0)  ret = cmd_bootstrap_cli (&app, sub_argc, sub_argv);
    else if (g_strcmp0 (command, "mcp") == 0)        ret = cmd_mcp (&app);
    else {
        g_printerr ("Unknown command: %s\n", command);
        print_help ();
        ret = 1;
    }

    g_object_unref (soup);
    PQfinish (conn);
    return ret;
}
