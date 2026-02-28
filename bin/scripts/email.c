#!/usr/bin/crispy
#define CRISPY_PARAMS "-std=gnu89 -O2 $(pkg-config --cflags --libs yaml-glib-1.0 json-glib-1.0 mcp-glib-1.0) -letpan -Wno-unused-function"

/*
 * email.c -- CLI email client (IMAP/SMTP)
 * Copyright (C) 2026  Zach Podbielniak -- AGPLv3
 *
 * Crispy C script for sending and reading emails.
 * Uses libetpan for IMAP/SMTP, yaml-glib for config.
 */

#include <yaml-glib.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <gio/gio.h>
#include <json-glib/json-glib.h>
#include <mcp.h>
#include <libetpan/libetpan.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>

/* ═══════════════════════════════════════════════════════════════════════════
 * Config
 * ═══════════════════════════════════════════════════════════════════════════ */

typedef struct {
    gchar *from_addr;
    gchar *imap_host;
    gint   imap_port;
    gboolean imap_starttls;
    gchar *smtp_host;
    gint   smtp_port;
    gchar *username;
    gchar *password;
} EmailConfig;

static void
free_config (EmailConfig *cfg)
{
    if (!cfg)
        return;
    g_free (cfg->from_addr);
    g_free (cfg->imap_host);
    g_free (cfg->smtp_host);
    g_free (cfg->username);
    g_free (cfg->password);
    g_free (cfg);
}

static EmailConfig *
load_config (const gchar *path)
{
    YamlParser *parser;
    YamlNode *root;
    YamlMapping *map;
    EmailConfig *cfg;
    GError *error = NULL;
    gchar *expanded;

    if (!path)
        path = "~/.config/email.yaml";

    expanded = NULL;
    if (path[0] == '~' && path[1] == '/') {
        expanded = g_build_filename (g_get_home_dir (), path + 2, NULL);
    } else {
        expanded = g_strdup (path);
    }

    parser = yaml_parser_new_immutable ();
    if (!yaml_parser_load_from_file (parser, expanded, &error)) {
        g_printerr ("Failed to load config '%s': %s\n", expanded, error->message);
        g_error_free (error);
        g_object_unref (parser);
        g_free (expanded);
        return NULL;
    }
    g_free (expanded);

    root = yaml_parser_get_root (parser);
    if (!root) {
        g_printerr ("Config file is empty\n");
        g_object_unref (parser);
        return NULL;
    }

    map = yaml_node_get_mapping (root);
    if (!map) {
        g_printerr ("Config root is not a mapping\n");
        g_object_unref (parser);
        return NULL;
    }

    cfg = g_new0 (EmailConfig, 1);
    cfg->from_addr = g_strdup (yaml_mapping_get_string_member (map, "from"));
    cfg->imap_host = g_strdup (yaml_mapping_get_string_member (map, "imap_host"));
    cfg->imap_port = (gint) yaml_mapping_get_int_member (map, "imap_port");
    cfg->smtp_host = g_strdup (yaml_mapping_get_string_member (map, "smtp_host"));
    cfg->smtp_port = (gint) yaml_mapping_get_int_member (map, "smtp_port");
    cfg->username  = g_strdup (yaml_mapping_get_string_member (map, "username"));
    cfg->password  = g_strdup (yaml_mapping_get_string_member (map, "password"));

    if (!cfg->imap_port)
        cfg->imap_port = 993;
    if (!cfg->smtp_port)
        cfg->smtp_port = 587;

    /* Check for explicit imap_starttls setting, otherwise auto-detect:
     * port 993 = direct SSL, anything else (143, 1143, etc.) = STARTTLS */
    if (yaml_mapping_has_member (map, "imap_starttls"))
        cfg->imap_starttls = (gboolean) yaml_mapping_get_int_member (map, "imap_starttls");
    else
        cfg->imap_starttls = (cfg->imap_port != 993);

    g_object_unref (parser);
    return cfg;
}

/*
 * imap_connect:
 * @imap: an existing mailimap session (from mailimap_new)
 * @cfg: email config with host, port, and starttls flag
 *
 * Connects to the IMAP server using direct SSL (port 993) or
 * plain+STARTTLS (port 143, 1143, etc.) based on cfg->imap_starttls.
 *
 * Returns: MAILIMAP_NO_ERROR on success, or a libetpan error code.
 */
static gint
imap_connect (
    mailimap       *imap,
    EmailConfig    *cfg
){
    gint r;

    if (cfg->imap_starttls) {
        /* Plain connection, then upgrade to TLS */
        r = mailimap_socket_connect (imap, cfg->imap_host, (uint16_t) cfg->imap_port);
        if (r != MAILIMAP_NO_ERROR && r != MAILIMAP_NO_ERROR_AUTHENTICATED
            && r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED)
            return r;

        r = mailimap_socket_starttls (imap);
        return r;
    }

    /* Direct SSL connection (port 993) */
    return mailimap_ssl_connect (imap, cfg->imap_host, (uint16_t) cfg->imap_port);
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Helpers
 * ═══════════════════════════════════════════════════════════════════════════ */

static gchar *
read_stdin_body (void)
{
    GString *buf;
    gchar chunk[4096];
    gssize n;

    if (isatty (STDIN_FILENO))
        return NULL;

    buf = g_string_new (NULL);
    while ((n = read (STDIN_FILENO, chunk, sizeof (chunk))) > 0)
        g_string_append_len (buf, chunk, n);

    if (buf->len == 0) {
        g_string_free (buf, TRUE);
        return NULL;
    }

    return g_string_free (buf, FALSE);
}

static gchar *
format_rfc2822_date (void)
{
    time_t now;
    struct tm tm_buf;
    gchar buf[128];

    now = time (NULL);
    localtime_r (&now, &tm_buf);
    strftime (buf, sizeof (buf), "%a, %d %b %Y %H:%M:%S %z", &tm_buf);
    return g_strdup (buf);
}

static const gchar *
get_first_from_addr (struct mailimap_envelope *env)
{
    clistiter *it;
    struct mailimap_address *addr;

    if (!env || !env->env_from || !env->env_from->frm_list)
        return "(unknown)";

    it = clist_begin (env->env_from->frm_list);
    if (!it)
        return "(unknown)";

    addr = (struct mailimap_address *) clist_content (it);
    if (addr->ad_personal_name)
        return addr->ad_personal_name;
    if (addr->ad_mailbox_name)
        return addr->ad_mailbox_name;

    return "(unknown)";
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Send command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_send (EmailConfig *cfg, int argc, char **argv)
{
    gchar **to_addrs = NULL;
    gchar **cc_addrs = NULL;
    gchar **bcc_addrs = NULL;
    gchar *subject = NULL;
    gchar *body_arg = NULL;
    gboolean html = FALSE;
    gchar *config_path = NULL;

    GOptionEntry entries[] = {
        { "to",      't', 0, G_OPTION_ARG_STRING_ARRAY, &to_addrs,  "Recipient address",    "ADDR" },
        { "cc",      'c', 0, G_OPTION_ARG_STRING_ARRAY, &cc_addrs,  "CC address",           "ADDR" },
        { "bcc",     'b', 0, G_OPTION_ARG_STRING_ARRAY, &bcc_addrs, "BCC address",          "ADDR" },
        { "subject", 's', 0, G_OPTION_ARG_STRING,       &subject,   "Subject line",         "TEXT" },
        { "body",     0,  0, G_OPTION_ARG_STRING,       &body_arg,  "Body text",            "TEXT" },
        { "html",     0,  0, G_OPTION_ARG_NONE,         &html,      "Send as HTML",         NULL },
        { "config",   0,  0, G_OPTION_ARG_STRING,       &config_path, "Config file path",   "PATH" },
        { NULL }
    };

    GOptionContext *ctx;
    GError *error = NULL;
    gchar *stdin_body = NULL;
    GString *full_body = NULL;
    GString *msg = NULL;
    gchar *date_str = NULL;
    mailsmtp *smtp = NULL;
    clist *rcpts = NULL;
    gint ret = 1;
    gint r;
    gint i;

    ctx = g_option_context_new ("- send an email");
    g_option_context_add_main_entries (ctx, entries, NULL);
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("send: %s\n", error->message);
        g_error_free (error);
        g_option_context_free (ctx);
        return 1;
    }
    g_option_context_free (ctx);

    if (!to_addrs || !to_addrs[0]) {
        g_printerr ("send: --to is required\n");
        goto cleanup;
    }
    if (!subject) {
        g_printerr ("send: --subject is required\n");
        goto cleanup;
    }

    /* Build body: CLI arg first, then stdin */
    stdin_body = read_stdin_body ();
    full_body = g_string_new (NULL);
    if (body_arg)
        g_string_append (full_body, body_arg);
    if (stdin_body) {
        if (full_body->len > 0)
            g_string_append_c (full_body, '\n');
        g_string_append (full_body, stdin_body);
    }
    if (full_body->len == 0) {
        g_printerr ("send: no body provided (use --body or pipe to stdin)\n");
        goto cleanup;
    }

    /* Build RFC 2822 message */
    date_str = format_rfc2822_date ();
    msg = g_string_new (NULL);
    g_string_append_printf (msg, "From: %s\r\n", cfg->from_addr);

    for (i = 0; to_addrs[i]; i++)
        g_string_append_printf (msg, "To: %s\r\n", to_addrs[i]);

    if (cc_addrs) {
        for (i = 0; cc_addrs[i]; i++)
            g_string_append_printf (msg, "Cc: %s\r\n", cc_addrs[i]);
    }

    g_string_append_printf (msg, "Subject: %s\r\n", subject);
    g_string_append_printf (msg, "Date: %s\r\n", date_str);
    g_string_append (msg, "MIME-Version: 1.0\r\n");

    if (html)
        g_string_append (msg, "Content-Type: text/html; charset=UTF-8\r\n");
    else
        g_string_append (msg, "Content-Type: text/plain; charset=UTF-8\r\n");

    g_string_append (msg, "Content-Transfer-Encoding: 8bit\r\n");
    g_string_append (msg, "\r\n");
    g_string_append (msg, full_body->str);

    /* SMTP */
    signal (SIGPIPE, SIG_IGN);
    smtp = mailsmtp_new (0, NULL);
    if (!smtp) {
        g_printerr ("send: failed to create SMTP session\n");
        goto cleanup;
    }

    if (cfg->smtp_port == 465) {
        r = mailsmtp_ssl_connect (smtp, cfg->smtp_host, (uint16_t) cfg->smtp_port);
    } else {
        r = mailsmtp_socket_connect (smtp, cfg->smtp_host, (uint16_t) cfg->smtp_port);
    }
    if (r != MAILSMTP_NO_ERROR) {
        g_printerr ("send: SMTP connect failed: %s\n", mailsmtp_strerror (r));
        goto cleanup;
    }

    r = mailsmtp_init (smtp);
    if (r != MAILSMTP_NO_ERROR) {
        g_printerr ("send: SMTP EHLO failed: %s\n", mailsmtp_strerror (r));
        goto cleanup;
    }

    if (cfg->smtp_port == 587) {
        r = mailesmtp_starttls (smtp);
        if (r != MAILSMTP_NO_ERROR) {
            g_printerr ("send: STARTTLS failed: %s\n", mailsmtp_strerror (r));
            goto cleanup;
        }
        r = mailsmtp_init (smtp);
        if (r != MAILSMTP_NO_ERROR) {
            g_printerr ("send: SMTP re-EHLO failed: %s\n", mailsmtp_strerror (r));
            goto cleanup;
        }
    }

    r = mailsmtp_auth (smtp, cfg->username, cfg->password);
    if (r != MAILSMTP_NO_ERROR) {
        g_printerr ("send: SMTP auth failed: %s\n", mailsmtp_strerror (r));
        goto cleanup;
    }

    /* Build recipient list */
    rcpts = esmtp_address_list_new ();
    for (i = 0; to_addrs[i]; i++)
        esmtp_address_list_add (rcpts, to_addrs[i], 0, NULL);
    if (cc_addrs) {
        for (i = 0; cc_addrs[i]; i++)
            esmtp_address_list_add (rcpts, cc_addrs[i], 0, NULL);
    }
    if (bcc_addrs) {
        for (i = 0; bcc_addrs[i]; i++)
            esmtp_address_list_add (rcpts, bcc_addrs[i], 0, NULL);
    }

    r = mailesmtp_send (smtp, cfg->from_addr, 0, NULL, rcpts, msg->str, msg->len);
    if (r != MAILSMTP_NO_ERROR) {
        g_printerr ("send: SMTP send failed: %s\n", mailsmtp_strerror (r));
        goto cleanup;
    }

    g_print ("Email sent to %s\n", to_addrs[0]);
    if (to_addrs[1])
        g_print ("  (and %d more recipients)\n", (int) g_strv_length (to_addrs) - 1);

    ret = 0;

cleanup:
    if (rcpts)
        esmtp_address_list_free (rcpts);
    if (smtp)
        mailsmtp_free (smtp);
    if (msg)
        g_string_free (msg, TRUE);
    if (full_body)
        g_string_free (full_body, TRUE);
    g_free (date_str);
    g_free (stdin_body);
    g_free (body_arg);
    g_free (subject);
    g_free (config_path);
    g_strfreev (to_addrs);
    g_strfreev (cc_addrs);
    g_strfreev (bcc_addrs);
    return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Read command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_read (EmailConfig *cfg, int argc, char **argv)
{
    gchar *folder = NULL;
    gint limit = 10;
    gboolean unread_only = FALSE;
    gint uid_val = 0;
    gboolean full = FALSE;
    gboolean mark_read = FALSE;
    gchar *config_path = NULL;

    GOptionEntry entries[] = {
        { "folder",    'f', 0, G_OPTION_ARG_STRING, &folder,      "IMAP folder",         "NAME" },
        { "limit",     'n', 0, G_OPTION_ARG_INT,    &limit,       "Max messages",        "N" },
        { "unread",    'u', 0, G_OPTION_ARG_NONE,   &unread_only, "Only unread messages", NULL },
        { "uid",        0,  0, G_OPTION_ARG_INT,    &uid_val,     "Fetch by UID",        "UID" },
        { "full",       0,  0, G_OPTION_ARG_NONE,   &full,        "Show full body",       NULL },
        { "mark-read",  0,  0, G_OPTION_ARG_NONE,   &mark_read,   "Mark as read",        NULL },
        { "config",     0,  0, G_OPTION_ARG_STRING,  &config_path, "Config file path",    "PATH" },
        { NULL }
    };

    GOptionContext *ctx;
    GError *error = NULL;
    mailimap *imap = NULL;
    struct mailimap_fetch_type *fetch_type = NULL;
    struct mailimap_fetch_att *fetch_att;
    struct mailimap_set *set = NULL;
    clist *fetch_result = NULL;
    clistiter *it;
    gint r;
    gint ret = 1;

    ctx = g_option_context_new ("- read emails");
    g_option_context_add_main_entries (ctx, entries, NULL);
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("read: %s\n", error->message);
        g_error_free (error);
        g_option_context_free (ctx);
        return 1;
    }
    g_option_context_free (ctx);

    if (!folder)
        folder = g_strdup ("INBOX");

    /* Connect IMAP */
    imap = mailimap_new (0, NULL);
    if (!imap) {
        g_printerr ("read: failed to create IMAP session\n");
        goto cleanup;
    }

    r = imap_connect (imap, cfg);
    if (r != MAILIMAP_NO_ERROR && r != MAILIMAP_NO_ERROR_AUTHENTICATED
        && r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED) {
        g_printerr ("read: IMAP connect to %s:%d failed (error %d)\n",
                     cfg->imap_host, cfg->imap_port, r);
        mailimap_free (imap);
        imap = NULL;
        goto cleanup;
    }

    r = mailimap_login (imap, cfg->username, cfg->password);
    if (r != MAILIMAP_NO_ERROR) {
        g_printerr ("read: IMAP login failed (error %d)\n", r);
        goto cleanup;
    }

    r = mailimap_select (imap, folder);
    if (r != MAILIMAP_NO_ERROR) {
        g_printerr ("read: failed to select folder '%s' (error %d)\n", folder, r);
        goto cleanup;
    }

    /* Build fetch type: envelope + UID, optionally body text */
    fetch_type = mailimap_fetch_type_new_fetch_att_list_empty ();

    fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_ENVELOPE, NULL, 0, 0, NULL);
    mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

    fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_UID, NULL, 0, 0, NULL);
    mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

    if (full || uid_val) {
        struct mailimap_section *section;
        section = mailimap_section_new_text ();
        fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_BODY_PEEK_SECTION,
                                            section, 0, 0, NULL);
        mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);
    }

    /* Determine which messages to fetch */
    if (uid_val > 0) {
        /* Fetch specific UID */
        set = mailimap_set_new_single ((uint32_t) uid_val);
        r = mailimap_uid_fetch (imap, set, fetch_type, &fetch_result);
    } else if (unread_only) {
        /* Search for unseen, then fetch */
        struct mailimap_search_key *key;
        clist *search_result = NULL;
        clistiter *sit;
        uint32_t count;

        key = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_UNSEEN,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
            NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
            NULL, NULL, NULL, 0, NULL, NULL, NULL);
        r = mailimap_uid_search (imap, NULL, key, &search_result);
        mailimap_search_key_free (key);

        if (r != MAILIMAP_NO_ERROR) {
            g_printerr ("read: IMAP search failed (error %d)\n", r);
            goto cleanup;
        }

        count = 0;
        set = mailimap_set_new_empty ();
        for (sit = clist_begin (search_result); sit; sit = clist_next (sit)) {
            uint32_t *uid_p = (uint32_t *) clist_content (sit);
            mailimap_set_add_single (set, *uid_p);
            count++;
            if ((gint) count >= limit)
                break;
        }
        mailimap_search_result_free (search_result);

        if (count == 0) {
            g_print ("No unread messages.\n");
            ret = 0;
            goto cleanup;
        }

        r = mailimap_uid_fetch (imap, set, fetch_type, &fetch_result);
    } else {
        /* Fetch last N messages by sequence number */
        uint32_t total;
        uint32_t from_seq;

        total = imap->imap_selection_info->sel_exists;
        if (total == 0) {
            g_print ("Mailbox is empty.\n");
            ret = 0;
            goto cleanup;
        }

        from_seq = (total > (uint32_t) limit) ? (total - (uint32_t) limit + 1) : 1;
        set = mailimap_set_new_interval (from_seq, 0);
        r = mailimap_fetch (imap, set, fetch_type, &fetch_result);
    }

    if (r != MAILIMAP_NO_ERROR) {
        g_printerr ("read: IMAP fetch failed (error %d)\n", r);
        goto cleanup;
    }

    /* Print results */
    if (!full && !uid_val) {
        g_print ("%-8s  %-26s  %-24s  %s\n", "UID", "Date", "From", "Subject");
        g_print ("%-8s  %-26s  %-24s  %s\n", "--------", "--------------------------",
                 "------------------------", "-------");
    }

    for (it = clist_begin (fetch_result); it; it = clist_next (it)) {
        struct mailimap_msg_att *msg_att;
        clistiter *ait;
        struct mailimap_envelope *env = NULL;
        uint32_t uid = 0;
        const gchar *body_text = NULL;
        gsize body_len = 0;

        msg_att = (struct mailimap_msg_att *) clist_content (it);

        for (ait = clist_begin (msg_att->att_list); ait; ait = clist_next (ait)) {
            struct mailimap_msg_att_item *item;

            item = (struct mailimap_msg_att_item *) clist_content (ait);
            if (item->att_type == MAILIMAP_MSG_ATT_ITEM_STATIC) {
                struct mailimap_msg_att_static *s;
                s = item->att_data.att_static;

                switch (s->att_type) {
                case MAILIMAP_MSG_ATT_ENVELOPE:
                    env = s->att_data.att_env;
                    break;
                case MAILIMAP_MSG_ATT_UID:
                    uid = s->att_data.att_uid;
                    break;
                case MAILIMAP_MSG_ATT_BODY_SECTION:
                    if (s->att_data.att_body_section) {
                        body_text = s->att_data.att_body_section->sec_body_part;
                        body_len = s->att_data.att_body_section->sec_length;
                    }
                    break;
                }
            }
        }

        if (!env)
            continue;

        if (full || uid_val) {
            g_print ("UID: %u\n", (unsigned) uid);
            g_print ("Date: %s\n", env->env_date ? env->env_date : "(none)");
            g_print ("From: %s\n", get_first_from_addr (env));
            g_print ("Subject: %s\n", env->env_subject ? env->env_subject : "(none)");
            g_print ("---\n");
            if (body_text && body_len > 0) {
                fwrite (body_text, 1, body_len, stdout);
                if (body_text[body_len - 1] != '\n')
                    g_print ("\n");
            } else {
                g_print ("(no body)\n");
            }
            g_print ("\n");
        } else {
            gchar uid_str[16];
            gchar date_trunc[27];
            gchar from_trunc[25];

            g_snprintf (uid_str, sizeof (uid_str), "%u", (unsigned) uid);

            if (env->env_date) {
                g_strlcpy (date_trunc, env->env_date, sizeof (date_trunc));
            } else {
                g_strlcpy (date_trunc, "(none)", sizeof (date_trunc));
            }

            g_strlcpy (from_trunc, get_first_from_addr (env), sizeof (from_trunc));

            g_print ("%-8s  %-26s  %-24s  %s\n",
                     uid_str, date_trunc, from_trunc,
                     env->env_subject ? env->env_subject : "(none)");
        }

        /* Mark as read if requested */
        if (mark_read && uid > 0) {
            struct mailimap_flag *flag;
            struct mailimap_flag_list *flag_list;
            struct mailimap_store_att_flags *store_flags;
            struct mailimap_set *uid_set;

            flag = mailimap_flag_new_seen ();
            flag_list = mailimap_flag_list_new_empty ();
            mailimap_flag_list_add (flag_list, flag);
            store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
            uid_set = mailimap_set_new_single (uid);
            mailimap_uid_store (imap, uid_set, store_flags);
            mailimap_set_free (uid_set);
            mailimap_store_att_flags_free (store_flags);
        }
    }

    ret = 0;

cleanup:
    if (fetch_result)
        mailimap_fetch_list_free (fetch_result);
    if (set)
        mailimap_set_free (set);
    if (imap) {
        mailimap_logout (imap);
        mailimap_free (imap);
    }
    g_free (folder);
    g_free (config_path);
    return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Rm command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_rm (EmailConfig *cfg, int argc, char **argv)
{
    gchar *folder = NULL;
    gchar *config_path = NULL;
    gboolean expunge = TRUE;

    GOptionEntry entries[] = {
        { "folder",  'f', 0, G_OPTION_ARG_STRING, &folder,      "IMAP folder",       "NAME" },
        { "config",   0,  0, G_OPTION_ARG_STRING, &config_path, "Config file path",  "PATH" },
        { NULL }
    };

    GOptionContext *ctx;
    GError *error = NULL;
    mailimap *imap = NULL;
    gint r;
    gint ret = 1;
    gint i;

    ctx = g_option_context_new ("UID [UID...] - delete emails by UID");
    g_option_context_add_main_entries (ctx, entries, NULL);
    if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
        g_printerr ("rm: %s\n", error->message);
        g_error_free (error);
        g_option_context_free (ctx);
        return 1;
    }
    g_option_context_free (ctx);

    if (argc < 2) {
        g_printerr ("rm: at least one UID is required\n");
        goto cleanup;
    }

    if (!folder)
        folder = g_strdup ("INBOX");

    /* Connect IMAP */
    imap = mailimap_new (0, NULL);
    r = imap_connect (imap, cfg);
    if (r != MAILIMAP_NO_ERROR && r != MAILIMAP_NO_ERROR_AUTHENTICATED
        && r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED) {
        g_printerr ("rm: IMAP connect failed (error %d)\n", r);
        mailimap_free (imap);
        imap = NULL;
        goto cleanup;
    }

    r = mailimap_login (imap, cfg->username, cfg->password);
    if (r != MAILIMAP_NO_ERROR) {
        g_printerr ("rm: IMAP login failed (error %d)\n", r);
        goto cleanup;
    }

    r = mailimap_select (imap, folder);
    if (r != MAILIMAP_NO_ERROR) {
        g_printerr ("rm: failed to select folder '%s' (error %d)\n", folder, r);
        goto cleanup;
    }

    /* Mark each UID as \Deleted */
    for (i = 1; i < argc; i++) {
        uint32_t uid;
        struct mailimap_flag *flag;
        struct mailimap_flag_list *flag_list;
        struct mailimap_store_att_flags *store_flags;
        struct mailimap_set *uid_set;

        uid = (uint32_t) g_ascii_strtoull (argv[i], NULL, 10);
        if (uid == 0) {
            g_printerr ("rm: invalid UID '%s'\n", argv[i]);
            continue;
        }

        flag = mailimap_flag_new_deleted ();
        flag_list = mailimap_flag_list_new_empty ();
        mailimap_flag_list_add (flag_list, flag);
        store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
        uid_set = mailimap_set_new_single (uid);
        r = mailimap_uid_store (imap, uid_set, store_flags);
        mailimap_set_free (uid_set);
        mailimap_store_att_flags_free (store_flags);

        if (r != MAILIMAP_NO_ERROR) {
            g_printerr ("rm: failed to flag UID %u (error %d)\n", (unsigned) uid, r);
        } else {
            g_print ("Deleted UID %u\n", (unsigned) uid);
        }
    }

    /* Expunge to permanently remove */
    if (expunge) {
        r = mailimap_expunge (imap);
        if (r != MAILIMAP_NO_ERROR)
            g_printerr ("rm: expunge failed (error %d)\n", r);
    }

    ret = 0;

cleanup:
    if (imap) {
        mailimap_logout (imap);
        mailimap_free (imap);
    }
    g_free (folder);
    g_free (config_path);
    return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * MCP Server
 * ═══════════════════════════════════════════════════════════════════════════ */

static const gchar *MCP_TOOLS_JSON =
"["
"  {\"name\":\"email_send\","
"   \"description\":\"Send an email via SMTP.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"to\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"Recipient addresses\"},"
"     \"cc\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"CC addresses\"},"
"     \"bcc\":{\"type\":\"array\",\"items\":{\"type\":\"string\"},\"description\":\"BCC addresses\"},"
"     \"subject\":{\"type\":\"string\",\"description\":\"Subject line\"},"
"     \"body\":{\"type\":\"string\",\"description\":\"Email body text\"},"
"     \"html\":{\"type\":\"boolean\",\"description\":\"Send as HTML\"}"
"   },\"required\":[\"to\",\"subject\",\"body\"]}},"
"  {\"name\":\"email_read\","
"   \"description\":\"Read emails from IMAP. Returns JSON array of messages.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"},"
"     \"limit\":{\"type\":\"integer\",\"description\":\"Max messages (default: 10)\"},"
"     \"unread\":{\"type\":\"boolean\",\"description\":\"Only unread messages\"},"
"     \"uid\":{\"type\":\"integer\",\"description\":\"Fetch specific message by UID\"},"
"     \"full\":{\"type\":\"boolean\",\"description\":\"Include message body\"},"
"     \"mark_read\":{\"type\":\"boolean\",\"description\":\"Mark fetched as read\"}"
"   }}},"
"  {\"name\":\"email_rm\","
"   \"description\":\"Delete emails by UID.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"uids\":{\"type\":\"array\",\"items\":{\"type\":\"integer\"},\"description\":\"UIDs to delete\"},"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"}"
"   },\"required\":[\"uids\"]}}"
"]";

static const gchar *
_json_get_string (JsonObject *obj, const gchar *name, const gchar *def)
{
    if (json_object_has_member (obj, name) &&
        json_object_get_null_member (obj, name) == FALSE)
        return json_object_get_string_member (obj, name);
    return def;
}

static gboolean
_json_get_bool (JsonObject *obj, const gchar *name, gboolean def)
{
    if (json_object_has_member (obj, name))
        return json_object_get_boolean_member (obj, name);
    return def;
}

static gint64
_json_get_int (JsonObject *obj, const gchar *name, gint64 def)
{
    if (json_object_has_member (obj, name))
        return json_object_get_int_member (obj, name);
    return def;
}

static McpToolResult *
_mcp_text_result (const gchar *text, gboolean is_error)
{
    McpToolResult *r = mcp_tool_result_new (is_error);
    mcp_tool_result_add_text (r, text);
    return r;
}

static McpToolResult *
mcp_handle_send (EmailConfig *cfg, JsonObject *args)
{
    JsonArray *to_arr;
    JsonArray *cc_arr;
    JsonArray *bcc_arr;
    const gchar *subject;
    const gchar *body;
    gboolean html;
    GString *msg;
    gchar *date_str;
    mailsmtp *smtp;
    clist *rcpts;
    gint r;
    guint i;

    if (!json_object_has_member (args, "to"))
        return _mcp_text_result ("Missing required field: to", TRUE);

    to_arr = json_object_get_array_member (args, "to");
    if (!to_arr || json_array_get_length (to_arr) == 0)
        return _mcp_text_result ("'to' must have at least one address", TRUE);

    subject = _json_get_string (args, "subject", "(no subject)");
    body = _json_get_string (args, "body", "");
    html = _json_get_bool (args, "html", FALSE);

    /* Build RFC 2822 */
    date_str = format_rfc2822_date ();
    msg = g_string_new (NULL);
    g_string_append_printf (msg, "From: %s\r\n", cfg->from_addr);

    for (i = 0; i < json_array_get_length (to_arr); i++)
        g_string_append_printf (msg, "To: %s\r\n", json_array_get_string_element (to_arr, i));

    cc_arr = json_object_has_member (args, "cc") ? json_object_get_array_member (args, "cc") : NULL;
    if (cc_arr) {
        for (i = 0; i < json_array_get_length (cc_arr); i++)
            g_string_append_printf (msg, "Cc: %s\r\n", json_array_get_string_element (cc_arr, i));
    }

    g_string_append_printf (msg, "Subject: %s\r\n", subject);
    g_string_append_printf (msg, "Date: %s\r\n", date_str);
    g_string_append (msg, "MIME-Version: 1.0\r\n");
    g_string_append_printf (msg, "Content-Type: %s; charset=UTF-8\r\n",
                            html ? "text/html" : "text/plain");
    g_string_append (msg, "Content-Transfer-Encoding: 8bit\r\n\r\n");
    g_string_append (msg, body);
    g_free (date_str);

    /* SMTP */
    signal (SIGPIPE, SIG_IGN);
    smtp = mailsmtp_new (0, NULL);
    if (!smtp) {
        g_string_free (msg, TRUE);
        return _mcp_text_result ("Failed to create SMTP session", TRUE);
    }

    if (cfg->smtp_port == 465)
        r = mailsmtp_ssl_connect (smtp, cfg->smtp_host, (uint16_t) cfg->smtp_port);
    else
        r = mailsmtp_socket_connect (smtp, cfg->smtp_host, (uint16_t) cfg->smtp_port);

    if (r != MAILSMTP_NO_ERROR) {
        g_string_free (msg, TRUE);
        mailsmtp_free (smtp);
        return _mcp_text_result (g_strdup_printf ("SMTP connect failed: %s", mailsmtp_strerror (r)), TRUE);
    }

    r = mailsmtp_init (smtp);
    if (r != MAILSMTP_NO_ERROR) {
        g_string_free (msg, TRUE);
        mailsmtp_free (smtp);
        return _mcp_text_result ("SMTP EHLO failed", TRUE);
    }

    if (cfg->smtp_port == 587) {
        mailesmtp_starttls (smtp);
        mailsmtp_init (smtp);
    }

    r = mailsmtp_auth (smtp, cfg->username, cfg->password);
    if (r != MAILSMTP_NO_ERROR) {
        g_string_free (msg, TRUE);
        mailsmtp_free (smtp);
        return _mcp_text_result ("SMTP auth failed", TRUE);
    }

    rcpts = esmtp_address_list_new ();
    for (i = 0; i < json_array_get_length (to_arr); i++)
        esmtp_address_list_add (rcpts, (char *) json_array_get_string_element (to_arr, i), 0, NULL);

    bcc_arr = json_object_has_member (args, "bcc") ? json_object_get_array_member (args, "bcc") : NULL;
    if (cc_arr) {
        for (i = 0; i < json_array_get_length (cc_arr); i++)
            esmtp_address_list_add (rcpts, (char *) json_array_get_string_element (cc_arr, i), 0, NULL);
    }
    if (bcc_arr) {
        for (i = 0; i < json_array_get_length (bcc_arr); i++)
            esmtp_address_list_add (rcpts, (char *) json_array_get_string_element (bcc_arr, i), 0, NULL);
    }

    r = mailesmtp_send (smtp, cfg->from_addr, 0, NULL, rcpts, msg->str, msg->len);
    esmtp_address_list_free (rcpts);
    g_string_free (msg, TRUE);
    mailsmtp_free (smtp);

    if (r != MAILSMTP_NO_ERROR)
        return _mcp_text_result ("SMTP send failed", TRUE);

    {
        g_autofree gchar *result = g_strdup_printf (
            "{\"status\":\"sent\",\"to\":\"%s\"}",
            json_array_get_string_element (to_arr, 0));
        return _mcp_text_result (result, FALSE);
    }
}

static McpToolResult *
mcp_handle_read (EmailConfig *cfg, JsonObject *args)
{
    const gchar *folder;
    gint64 limit;
    gboolean unread_only;
    gint64 uid_val;
    gboolean full;
    gboolean mark_read;
    mailimap *imap;
    struct mailimap_fetch_type *fetch_type;
    struct mailimap_fetch_att *fetch_att;
    struct mailimap_set *set = NULL;
    clist *fetch_result = NULL;
    clistiter *it;
    gint r;
    JsonBuilder *jb;
    JsonGenerator *gen;
    JsonNode *jnode;
    gchar *json_out;

    folder = _json_get_string (args, "folder", "INBOX");
    limit = _json_get_int (args, "limit", 10);
    unread_only = _json_get_bool (args, "unread", FALSE);
    uid_val = _json_get_int (args, "uid", 0);
    full = _json_get_bool (args, "full", FALSE);
    mark_read = _json_get_bool (args, "mark_read", FALSE);

    imap = mailimap_new (0, NULL);
    r = imap_connect (imap, cfg);
    if (r != MAILIMAP_NO_ERROR && r != MAILIMAP_NO_ERROR_AUTHENTICATED
        && r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED) {
        mailimap_free (imap);
        return _mcp_text_result ("IMAP connect failed", TRUE);
    }

    r = mailimap_login (imap, cfg->username, cfg->password);
    if (r != MAILIMAP_NO_ERROR) {
        mailimap_logout (imap);
        mailimap_free (imap);
        return _mcp_text_result ("IMAP login failed", TRUE);
    }

    r = mailimap_select (imap, folder);
    if (r != MAILIMAP_NO_ERROR) {
        mailimap_logout (imap);
        mailimap_free (imap);
        return _mcp_text_result ("Failed to select folder", TRUE);
    }

    fetch_type = mailimap_fetch_type_new_fetch_att_list_empty ();
    fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_ENVELOPE, NULL, 0, 0, NULL);
    mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);
    fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_UID, NULL, 0, 0, NULL);
    mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

    if (full || uid_val) {
        struct mailimap_section *section = mailimap_section_new_text ();
        fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_BODY_PEEK_SECTION,
                                            section, 0, 0, NULL);
        mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);
    }

    if (uid_val > 0) {
        set = mailimap_set_new_single ((uint32_t) uid_val);
        r = mailimap_uid_fetch (imap, set, fetch_type, &fetch_result);
    } else if (unread_only) {
        struct mailimap_search_key *key;
        clist *search_result = NULL;
        clistiter *sit;
        uint32_t count;

        key = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_UNSEEN,
            NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
            NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
            NULL, NULL, NULL, 0, NULL, NULL, NULL);
        r = mailimap_uid_search (imap, NULL, key, &search_result);
        mailimap_search_key_free (key);

        if (r != MAILIMAP_NO_ERROR) {
            mailimap_logout (imap);
            mailimap_free (imap);
            return _mcp_text_result ("IMAP search failed", TRUE);
        }

        count = 0;
        set = mailimap_set_new_empty ();
        for (sit = clist_begin (search_result); sit; sit = clist_next (sit)) {
            uint32_t *uid_p = (uint32_t *) clist_content (sit);
            mailimap_set_add_single (set, *uid_p);
            count++;
            if ((gint64) count >= limit)
                break;
        }
        mailimap_search_result_free (search_result);

        if (count == 0) {
            mailimap_logout (imap);
            mailimap_free (imap);
            return _mcp_text_result ("[]", FALSE);
        }
        r = mailimap_uid_fetch (imap, set, fetch_type, &fetch_result);
    } else {
        uint32_t total = imap->imap_selection_info->sel_exists;
        uint32_t from_seq;
        if (total == 0) {
            mailimap_logout (imap);
            mailimap_free (imap);
            return _mcp_text_result ("[]", FALSE);
        }
        from_seq = (total > (uint32_t) limit) ? (total - (uint32_t) limit + 1) : 1;
        set = mailimap_set_new_interval (from_seq, 0);
        r = mailimap_fetch (imap, set, fetch_type, &fetch_result);
    }

    if (r != MAILIMAP_NO_ERROR) {
        if (set) mailimap_set_free (set);
        mailimap_logout (imap);
        mailimap_free (imap);
        return _mcp_text_result ("IMAP fetch failed", TRUE);
    }

    /* Build JSON result */
    jb = json_builder_new ();
    json_builder_begin_array (jb);

    for (it = clist_begin (fetch_result); it; it = clist_next (it)) {
        struct mailimap_msg_att *msg_att;
        clistiter *ait;
        struct mailimap_envelope *env = NULL;
        uint32_t uid = 0;
        const gchar *body_text = NULL;
        gsize body_len = 0;

        msg_att = (struct mailimap_msg_att *) clist_content (it);
        for (ait = clist_begin (msg_att->att_list); ait; ait = clist_next (ait)) {
            struct mailimap_msg_att_item *item;
            item = (struct mailimap_msg_att_item *) clist_content (ait);
            if (item->att_type == MAILIMAP_MSG_ATT_ITEM_STATIC) {
                struct mailimap_msg_att_static *s = item->att_data.att_static;
                switch (s->att_type) {
                case MAILIMAP_MSG_ATT_ENVELOPE:
                    env = s->att_data.att_env;
                    break;
                case MAILIMAP_MSG_ATT_UID:
                    uid = s->att_data.att_uid;
                    break;
                case MAILIMAP_MSG_ATT_BODY_SECTION:
                    if (s->att_data.att_body_section) {
                        body_text = s->att_data.att_body_section->sec_body_part;
                        body_len = s->att_data.att_body_section->sec_length;
                    }
                    break;
                }
            }
        }

        if (!env) continue;

        json_builder_begin_object (jb);
        json_builder_set_member_name (jb, "uid");
        json_builder_add_int_value (jb, uid);
        json_builder_set_member_name (jb, "date");
        json_builder_add_string_value (jb, env->env_date ? env->env_date : "");
        json_builder_set_member_name (jb, "from");
        json_builder_add_string_value (jb, get_first_from_addr (env));
        json_builder_set_member_name (jb, "subject");
        json_builder_add_string_value (jb, env->env_subject ? env->env_subject : "");

        if ((full || uid_val) && body_text && body_len > 0) {
            g_autofree gchar *body_copy = g_strndup (body_text, body_len);
            json_builder_set_member_name (jb, "body");
            json_builder_add_string_value (jb, body_copy);
        }
        json_builder_end_object (jb);

        if (mark_read && uid > 0) {
            struct mailimap_flag *flag = mailimap_flag_new_seen ();
            struct mailimap_flag_list *fl = mailimap_flag_list_new_empty ();
            struct mailimap_store_att_flags *sf;
            struct mailimap_set *us;
            mailimap_flag_list_add (fl, flag);
            sf = mailimap_store_att_flags_new_add_flags (fl);
            us = mailimap_set_new_single (uid);
            mailimap_uid_store (imap, us, sf);
            mailimap_set_free (us);
            mailimap_store_att_flags_free (sf);
        }
    }

    json_builder_end_array (jb);
    jnode = json_builder_get_root (jb);
    gen = json_generator_new ();
    json_generator_set_root (gen, jnode);
    json_out = json_generator_to_data (gen, NULL);
    json_node_unref (jnode);
    g_object_unref (gen);
    g_object_unref (jb);

    if (fetch_result)
        mailimap_fetch_list_free (fetch_result);
    if (set)
        mailimap_set_free (set);
    mailimap_logout (imap);
    mailimap_free (imap);

    {
        McpToolResult *result = _mcp_text_result (json_out, FALSE);
        g_free (json_out);
        return result;
    }
}

static McpToolResult *
mcp_handle_rm (EmailConfig *cfg, JsonObject *args)
{
    JsonArray *uids_arr;
    const gchar *folder;
    mailimap *imap;
    gint r;
    guint i;
    guint deleted = 0;

    if (!json_object_has_member (args, "uids"))
        return _mcp_text_result ("Missing required field: uids", TRUE);

    uids_arr = json_object_get_array_member (args, "uids");
    if (!uids_arr || json_array_get_length (uids_arr) == 0)
        return _mcp_text_result ("'uids' must have at least one UID", TRUE);

    folder = _json_get_string (args, "folder", "INBOX");

    imap = mailimap_new (0, NULL);
    r = imap_connect (imap, cfg);
    if (r != MAILIMAP_NO_ERROR && r != MAILIMAP_NO_ERROR_AUTHENTICATED
        && r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED) {
        mailimap_free (imap);
        return _mcp_text_result ("IMAP connect failed", TRUE);
    }

    r = mailimap_login (imap, cfg->username, cfg->password);
    if (r != MAILIMAP_NO_ERROR) {
        mailimap_logout (imap);
        mailimap_free (imap);
        return _mcp_text_result ("IMAP login failed", TRUE);
    }

    r = mailimap_select (imap, folder);
    if (r != MAILIMAP_NO_ERROR) {
        mailimap_logout (imap);
        mailimap_free (imap);
        return _mcp_text_result ("Failed to select folder", TRUE);
    }

    for (i = 0; i < json_array_get_length (uids_arr); i++) {
        uint32_t uid = (uint32_t) json_array_get_int_element (uids_arr, i);
        struct mailimap_flag *flag;
        struct mailimap_flag_list *flag_list;
        struct mailimap_store_att_flags *store_flags;
        struct mailimap_set *uid_set;

        flag = mailimap_flag_new_deleted ();
        flag_list = mailimap_flag_list_new_empty ();
        mailimap_flag_list_add (flag_list, flag);
        store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
        uid_set = mailimap_set_new_single (uid);
        r = mailimap_uid_store (imap, uid_set, store_flags);
        mailimap_set_free (uid_set);
        mailimap_store_att_flags_free (store_flags);
        if (r == MAILIMAP_NO_ERROR)
            deleted++;
    }

    mailimap_expunge (imap);
    mailimap_logout (imap);
    mailimap_free (imap);

    {
        g_autofree gchar *result = g_strdup_printf (
            "{\"status\":\"deleted\",\"count\":%u}", deleted);
        return _mcp_text_result (result, FALSE);
    }
}

static McpToolResult *
mcp_handle_tool_call (McpServer *server, const gchar *name,
                      JsonObject *args, gpointer user_data)
{
    EmailConfig *cfg = (EmailConfig *) user_data;
    JsonObject *_empty_args = NULL;

    (void) server;

    if (!args) {
        _empty_args = json_object_new ();
        args = _empty_args;
    }

    if (g_strcmp0 (name, "email_send") == 0) {
        McpToolResult *r = mcp_handle_send (cfg, args);
        if (_empty_args) json_object_unref (_empty_args);
        return r;
    }
    if (g_strcmp0 (name, "email_read") == 0) {
        McpToolResult *r = mcp_handle_read (cfg, args);
        if (_empty_args) json_object_unref (_empty_args);
        return r;
    }
    if (g_strcmp0 (name, "email_rm") == 0) {
        McpToolResult *r = mcp_handle_rm (cfg, args);
        if (_empty_args) json_object_unref (_empty_args);
        return r;
    }

    if (_empty_args) json_object_unref (_empty_args);
    return _mcp_text_result ("Unknown tool", TRUE);
}

static void
on_client_disconnected (McpServer *server, gpointer user_data)
{
    GMainLoop *loop = (GMainLoop *) user_data;
    (void) server;
    g_main_loop_quit (loop);
}

static gint
cmd_mcp (EmailConfig *cfg)
{
    GMainLoop *loop;
    McpServer *server;
    McpStdioTransport *transport;
    JsonParser *tools_parser;
    JsonArray *tools_arr;
    guint i;

    loop = g_main_loop_new (NULL, FALSE);
    server = mcp_server_new ("email", "1.0.0");
    mcp_server_set_instructions (server,
        "Email client for sending, reading, and deleting emails via IMAP/SMTP.");

    g_signal_connect (server, "client-disconnected",
                      G_CALLBACK (on_client_disconnected), loop);

    tools_parser = json_parser_new ();
    json_parser_load_from_data (tools_parser, MCP_TOOLS_JSON, -1, NULL);
    tools_arr = json_node_get_array (json_parser_get_root (tools_parser));

    for (i = 0; i < json_array_get_length (tools_arr); i++) {
        JsonNode *tool_node = json_array_get_element (tools_arr, i);
        GError *err = NULL;
        McpTool *tool = mcp_tool_new_from_json (tool_node, &err);
        if (tool) {
            mcp_server_add_tool (server, tool, mcp_handle_tool_call, cfg, NULL);
            g_object_unref (tool);
        } else {
            g_warning ("Failed to register tool: %s", err ? err->message : "unknown");
            g_clear_error (&err);
        }
    }

    transport = mcp_stdio_transport_new ();
    mcp_server_set_transport (server, MCP_TRANSPORT (transport));
    mcp_server_start_async (server, NULL, NULL, NULL);

    g_main_loop_run (loop);
    g_main_loop_unref (loop);
    g_object_unref (transport);
    g_object_unref (tools_parser);
    g_object_unref (server);

    return 0;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Main
 * ═══════════════════════════════════════════════════════════════════════════ */

static void
print_usage (const gchar *prog)
{
    g_print ("Usage: %s <command> [options]\n\n", prog);
    g_print ("Commands:\n");
    g_print ("  send    Send an email\n");
    g_print ("  read    Read emails from IMAP\n");
    g_print ("  rm      Delete emails by UID\n");
    g_print ("  mcp     Run as MCP server (stdio)\n");
    g_print ("\nUse '%s <command> --help' for command-specific options.\n", prog);
    g_print ("\nConfig: ~/.config/email.yaml (override with --config)\n");
}

int
main (int argc, char **argv)
{
    EmailConfig *cfg;
    const gchar *cmd;
    gchar *config_path = NULL;
    gint i;
    gint ret;

    if (argc < 2) {
        print_usage (argv[0]);
        return 1;
    }

    cmd = argv[1];
    if (g_str_equal (cmd, "--help") || g_str_equal (cmd, "-h")) {
        print_usage (argv[0]);
        return 0;
    }

    /* Peek at argv for --config before dispatching */
    for (i = 2; i < argc; i++) {
        if (g_str_equal (argv[i], "--config") && i + 1 < argc) {
            config_path = argv[i + 1];
            break;
        }
    }

    /* Shift argv so subcommand sees itself as argv[0] */
    argc--;
    argv++;

    /* Allow --help without a config file */
    for (i = 1; i < argc; i++) {
        if (g_str_equal (argv[i], "--help") || g_str_equal (argv[i], "-h")) {
            cfg = g_new0 (EmailConfig, 1);
            if (g_str_equal (cmd, "send"))
                ret = cmd_send (cfg, argc, argv);
            else if (g_str_equal (cmd, "read"))
                ret = cmd_read (cfg, argc, argv);
            else if (g_str_equal (cmd, "rm"))
                ret = cmd_rm (cfg, argc, argv);
            else
                print_usage (argv[0]);
            free_config (cfg);
            return ret;
        }
    }

    cfg = load_config (config_path);
    if (!cfg)
        return 1;

    if (g_str_equal (cmd, "send")) {
        ret = cmd_send (cfg, argc, argv);
    } else if (g_str_equal (cmd, "read")) {
        ret = cmd_read (cfg, argc, argv);
    } else if (g_str_equal (cmd, "rm")) {
        ret = cmd_rm (cfg, argc, argv);
    } else if (g_str_equal (cmd, "mcp")) {
        ret = cmd_mcp (cfg);
    } else {
        g_printerr ("Unknown command: %s\n", cmd);
        print_usage (argv[0]);
        ret = 1;
    }

    free_config (cfg);
    return ret;
}
