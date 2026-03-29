#!/usr/bin/env crispy
#define CRISPY_PARAMS "-std=gnu89 -O2 $(pkg-config --cflags --libs yaml-glib-1.0 json-glib-1.0 mcp-glib-1.0) -letpan -Wno-unused-function"

/*
 * email.c -- CLI email client & MCP server (IMAP/SMTP)
 * Copyright (C) 2026  Zach Podbielniak -- AGPLv3
 *
 * Crispy C script for sending, reading, searching, and managing emails.
 * Uses libetpan for IMAP/SMTP, yaml-glib for config, mcp-glib for MCP.
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

#define AGPLV3_NOTICE \
    "email.c -- CLI email client & MCP server\n" \
    "Copyright (C) 2026  Zach Podbielniak\n\n" \
    "This program is free software: you can redistribute it and/or modify\n" \
    "it under the terms of the GNU Affero General Public License as published by\n" \
    "the Free Software Foundation, either version 3 of the License, or\n" \
    "(at your option) any later version.\n\n" \
    "This program is distributed in the hope that it will be useful,\n" \
    "but WITHOUT ANY WARRANTY; without even the implied warranty of\n" \
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n" \
    "GNU Affero General Public License for more details.\n\n" \
    "You should have received a copy of the GNU Affero General Public License\n" \
    "along with this program.  If not, see <https://www.gnu.org/licenses/>.\n"

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

static gchar *
get_first_from_email (struct mailimap_envelope *env)
{
	clistiter *it;
	struct mailimap_address *addr;

	if (!env || !env->env_from || !env->env_from->frm_list)
		return NULL;

	it = clist_begin (env->env_from->frm_list);
	if (!it)
		return NULL;

	addr = (struct mailimap_address *) clist_content (it);
	if (addr->ad_mailbox_name && addr->ad_host_name)
		return g_strdup_printf ("%s@%s", addr->ad_mailbox_name, addr->ad_host_name);
	if (addr->ad_mailbox_name)
		return g_strdup (addr->ad_mailbox_name);

	return NULL;
}

static gchar **
parse_csv_whitelist (const gchar *csv)
{
	gchar **parts;
	guint i;

	if (!csv || !*csv)
		return NULL;

	parts = g_strsplit (csv, ",", -1);
	for (i = 0; parts[i]; i++)
		g_strstrip (parts[i]);

	return parts;
}

static gboolean
is_whitelisted (const gchar *email, gchar **whitelist)
{
	guint i;

	if (!whitelist)
		return TRUE;
	if (!email)
		return FALSE;

	for (i = 0; whitelist[i]; i++) {
		if (g_ascii_strcasecmp (email, whitelist[i]) == 0)
			return TRUE;
	}

	return FALSE;
}

/*
 * imap_open:
 * @cfg: email config
 * @error_msg: (out) (nullable): set to error string on failure (caller frees)
 *
 * Creates a new IMAP session, connects, and logs in.
 * Does NOT select a folder (some commands don't need one).
 *
 * Returns: connected+authenticated mailimap*, or NULL on error.
 */
static mailimap *
imap_open (EmailConfig *cfg, gchar **error_msg)
{
	mailimap *imap;
	gint r;

	imap = mailimap_new (0, NULL);
	if (!imap) {
		if (error_msg)
			*error_msg = g_strdup ("Failed to create IMAP session");
		return NULL;
	}

	r = imap_connect (imap, cfg);
	if (r != MAILIMAP_NO_ERROR && r != MAILIMAP_NO_ERROR_AUTHENTICATED
		&& r != MAILIMAP_NO_ERROR_NON_AUTHENTICATED) {
		if (error_msg)
			*error_msg = g_strdup_printf ("IMAP connect to %s:%d failed (error %d)",
				cfg->imap_host, cfg->imap_port, r);
		mailimap_free (imap);
		return NULL;
	}

	r = mailimap_login (imap, cfg->username, cfg->password);
	if (r != MAILIMAP_NO_ERROR) {
		if (error_msg)
			*error_msg = g_strdup_printf ("IMAP login failed (error %d)", r);
		mailimap_logout (imap);
		mailimap_free (imap);
		return NULL;
	}

	return imap;
}

/*
 * generate_message_id:
 * @from_addr: sender email (e.g. "user@example.com")
 *
 * Generates a unique RFC 2822 Message-ID header value.
 * Format: <TIMESTAMP.PID.RANDOM@domain>
 *
 * Returns: newly allocated Message-ID string (caller frees).
 */
static gchar *
generate_message_id (const gchar *from_addr)
{
	const gchar *domain;
	gchar *msg_id;
	time_t now;

	domain = "localhost";
	if (from_addr) {
		const gchar *at = strchr (from_addr, '@');
		if (at)
			domain = at + 1;
	}

	now = time (NULL);
	msg_id = g_strdup_printf ("<%ld.%d.%08x@%s>",
		(long) now, (int) getpid (), g_random_int (), domain);
	return msg_id;
}

/*
 * Compare function for g_list_sort to sort strings alphabetically.
 */
static gint
compare_strings (gconstpointer a, gconstpointer b)
{
	return g_strcmp0 ((const gchar *) a, (const gchar *) b);
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
	gchar **attach_files = NULL;
	gchar *subject = NULL;
	gchar *body_arg = NULL;
	gchar *reply_to = NULL;
	gchar *in_reply_to = NULL;
	gboolean html = FALSE;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "to",          't', 0, G_OPTION_ARG_STRING_ARRAY, &to_addrs,     "Recipient address",    "ADDR" },
		{ "cc",          'c', 0, G_OPTION_ARG_STRING_ARRAY, &cc_addrs,     "CC address",           "ADDR" },
		{ "bcc",         'b', 0, G_OPTION_ARG_STRING_ARRAY, &bcc_addrs,    "BCC address",          "ADDR" },
		{ "subject",     's', 0, G_OPTION_ARG_STRING,       &subject,      "Subject line",         "TEXT" },
		{ "body",         0,  0, G_OPTION_ARG_STRING,       &body_arg,     "Body text",            "TEXT" },
		{ "html",         0,  0, G_OPTION_ARG_NONE,         &html,         "Send as HTML",         NULL },
		{ "attach",      'a', 0, G_OPTION_ARG_STRING_ARRAY, &attach_files, "Attach file",          "FILE" },
		{ "reply-to",     0,  0, G_OPTION_ARG_STRING,       &reply_to,     "Reply-To address",     "ADDR" },
		{ "in-reply-to",  0,  0, G_OPTION_ARG_STRING,       &in_reply_to,  "In-Reply-To Message-ID", "MSGID" },
		{ "config",       0,  0, G_OPTION_ARG_STRING,       &config_path,  "Config file path",     "PATH" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	gchar *stdin_body = NULL;
	GString *full_body = NULL;
	GString *msg = NULL;
	gchar *date_str = NULL;
	gchar *message_id = NULL;
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
	message_id = generate_message_id (cfg->from_addr);
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
	g_string_append_printf (msg, "Message-ID: %s\r\n", message_id);

	if (reply_to)
		g_string_append_printf (msg, "Reply-To: %s\r\n", reply_to);
	if (in_reply_to)
		g_string_append_printf (msg, "In-Reply-To: %s\r\n", in_reply_to);

	g_string_append (msg, "MIME-Version: 1.0\r\n");

	if (attach_files && attach_files[0]) {
		/* Multipart message with attachments */
		gchar *boundary;

		boundary = g_strdup_printf ("----=_Part_%08x_%08x", g_random_int (), g_random_int ());
		g_string_append_printf (msg, "Content-Type: multipart/mixed; boundary=\"%s\"\r\n", boundary);
		g_string_append (msg, "\r\n");

		/* Body part */
		g_string_append_printf (msg, "--%s\r\n", boundary);
		if (html)
			g_string_append (msg, "Content-Type: text/html; charset=UTF-8\r\n");
		else
			g_string_append (msg, "Content-Type: text/plain; charset=UTF-8\r\n");
		g_string_append (msg, "Content-Transfer-Encoding: 8bit\r\n");
		g_string_append (msg, "\r\n");
		g_string_append (msg, full_body->str);
		g_string_append (msg, "\r\n");

		/* Attachment parts */
		for (i = 0; attach_files[i]; i++) {
			gchar *file_contents = NULL;
			gsize file_len = 0;
			gchar *base64_data = NULL;
			gchar *basename_str = NULL;
			gchar *content_type = NULL;
			gchar *mime_type = NULL;
			gboolean uncertain = FALSE;
			gsize b64_len;
			gsize line_start;

			if (!g_file_get_contents (attach_files[i], &file_contents, &file_len, &error)) {
				g_printerr ("send: cannot read attachment '%s': %s\n",
					attach_files[i], error->message);
				g_clear_error (&error);
				g_free (boundary);
				goto cleanup;
			}

			basename_str = g_path_get_basename (attach_files[i]);
			content_type = g_content_type_guess (basename_str, (const guchar *) file_contents,
				file_len, &uncertain);
			mime_type = g_content_type_get_mime_type (content_type);
			if (!mime_type)
				mime_type = g_strdup ("application/octet-stream");

			base64_data = g_base64_encode ((const guchar *) file_contents, file_len);
			b64_len = strlen (base64_data);

			g_string_append_printf (msg, "--%s\r\n", boundary);
			g_string_append_printf (msg, "Content-Type: %s; name=\"%s\"\r\n",
				mime_type, basename_str);
			g_string_append (msg, "Content-Transfer-Encoding: base64\r\n");
			g_string_append_printf (msg, "Content-Disposition: attachment; filename=\"%s\"\r\n",
				basename_str);
			g_string_append (msg, "\r\n");

			/* Break base64 into 76-char lines */
			for (line_start = 0; line_start < b64_len; line_start += 76) {
				gsize chunk_len = b64_len - line_start;
				if (chunk_len > 76)
					chunk_len = 76;
				g_string_append_len (msg, base64_data + line_start, chunk_len);
				g_string_append (msg, "\r\n");
			}

			g_free (file_contents);
			g_free (base64_data);
			g_free (basename_str);
			g_free (content_type);
			g_free (mime_type);
		}

		/* Closing boundary */
		g_string_append_printf (msg, "--%s--\r\n", boundary);
		g_free (boundary);
	} else {
		/* Simple message without attachments */
		if (html)
			g_string_append (msg, "Content-Type: text/html; charset=UTF-8\r\n");
		else
			g_string_append (msg, "Content-Type: text/plain; charset=UTF-8\r\n");

		g_string_append (msg, "Content-Transfer-Encoding: 8bit\r\n");
		g_string_append (msg, "\r\n");
		g_string_append (msg, full_body->str);
	}

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
	g_free (message_id);
	g_free (stdin_body);
	g_free (body_arg);
	g_free (subject);
	g_free (reply_to);
	g_free (in_reply_to);
	g_free (config_path);
	g_strfreev (to_addrs);
	g_strfreev (cc_addrs);
	g_strfreev (bcc_addrs);
	g_strfreev (attach_files);
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
	gboolean json_output = FALSE;
	gchar *from_filter = NULL;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "folder",    'f', 0, G_OPTION_ARG_STRING, &folder,       "IMAP folder",          "NAME" },
		{ "limit",     'n', 0, G_OPTION_ARG_INT,    &limit,        "Max messages",         "N" },
		{ "unread",    'u', 0, G_OPTION_ARG_NONE,   &unread_only,  "Only unread messages",  NULL },
		{ "uid",        0,  0, G_OPTION_ARG_INT,    &uid_val,      "Fetch by UID",         "UID" },
		{ "full",       0,  0, G_OPTION_ARG_NONE,   &full,         "Show full body",        NULL },
		{ "mark-read",  0,  0, G_OPTION_ARG_NONE,   &mark_read,    "Mark as read",          NULL },
		{ "json",       0,  0, G_OPTION_ARG_NONE,   &json_output,  "Output as JSON",        NULL },
		{ "from",       0,  0, G_OPTION_ARG_STRING, &from_filter,  "Filter by sender",     "ADDR" },
		{ "config",     0,  0, G_OPTION_ARG_STRING, &config_path,  "Config file path",     "PATH" },
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
	JsonBuilder *jb = NULL;

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

	/* Build fetch type: envelope + UID + FLAGS, optionally body text + BODYSTRUCTURE */
	fetch_type = mailimap_fetch_type_new_fetch_att_list_empty ();

	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_ENVELOPE, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_UID, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_FLAGS, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

	if (full || uid_val) {
		struct mailimap_section *section;
		section = mailimap_section_new_text ();
		fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_BODY_PEEK_SECTION,
											section, 0, 0, NULL);
		mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

		fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_BODYSTRUCTURE, NULL, 0, 0, NULL);
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
			if (json_output)
				g_print ("[]");
			else
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
			if (json_output)
				g_print ("[]");
			else
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

	/* Init JSON builder if needed */
	if (json_output) {
		jb = json_builder_new ();
		json_builder_begin_array (jb);
	}

	/* Print results */
	if (!json_output && !full && !uid_val) {
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
		struct mailimap_body *bodystructure = NULL;
		struct mailimap_msg_att_dynamic *dynamic_flags = NULL;
		gboolean is_seen = FALSE;
		gboolean is_flagged = FALSE;

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
				case MAILIMAP_MSG_ATT_BODYSTRUCTURE:
					bodystructure = s->att_data.att_body;
					break;
				}
			} else if (item->att_type == MAILIMAP_MSG_ATT_ITEM_DYNAMIC) {
				dynamic_flags = item->att_data.att_dyn;
			}
		}

		/* Parse flags */
		if (dynamic_flags && dynamic_flags->att_list) {
			clistiter *fit;
			for (fit = clist_begin (dynamic_flags->att_list); fit; fit = clist_next (fit)) {
				struct mailimap_flag_fetch *ff;
				ff = (struct mailimap_flag_fetch *) clist_content (fit);
				if (ff->fl_type == MAILIMAP_FLAG_FETCH_OTHER && ff->fl_flag) {
					if (ff->fl_flag->fl_type == MAILIMAP_FLAG_SEEN)
						is_seen = TRUE;
					else if (ff->fl_flag->fl_type == MAILIMAP_FLAG_FLAGGED)
						is_flagged = TRUE;
				}
			}
		}

		if (!env)
			continue;

		/* Apply --from filter */
		if (from_filter) {
			g_autofree gchar *sender_email = get_first_from_email (env);
			if (!sender_email || g_ascii_strcasecmp (sender_email, from_filter) != 0)
				continue;
		}

		if (json_output) {
			json_builder_begin_object (jb);
			json_builder_set_member_name (jb, "uid");
			json_builder_add_int_value (jb, uid);
			json_builder_set_member_name (jb, "date");
			json_builder_add_string_value (jb, env->env_date ? env->env_date : "");
			json_builder_set_member_name (jb, "from");
			json_builder_add_string_value (jb, get_first_from_addr (env));
			json_builder_set_member_name (jb, "subject");
			json_builder_add_string_value (jb, env->env_subject ? env->env_subject : "");
			json_builder_set_member_name (jb, "seen");
			json_builder_add_boolean_value (jb, is_seen);
			json_builder_set_member_name (jb, "flagged");
			json_builder_add_boolean_value (jb, is_flagged);

			if ((full || uid_val) && body_text && body_len > 0) {
				g_autofree gchar *body_copy = g_strndup (body_text, body_len);
				json_builder_set_member_name (jb, "body");
				json_builder_add_string_value (jb, body_copy);
			}
			json_builder_end_object (jb);
		} else if (full || uid_val) {
			g_print ("UID: %u\n", (unsigned) uid);
			g_print ("Date: %s\n", env->env_date ? env->env_date : "(none)");
			g_print ("From: %s\n", get_first_from_addr (env));
			g_print ("Subject: %s\n", env->env_subject ? env->env_subject : "(none)");
			g_print ("Status: %s%s\n",
				is_seen ? "read" : "unread",
				is_flagged ? ", flagged" : "");

			/* Show attachment filenames from BODYSTRUCTURE */
			if (bodystructure && bodystructure->bd_type == MAILIMAP_BODY_MPART) {
				struct mailimap_body_type_mpart *mpart;
				clistiter *pit;
				gboolean first_att = TRUE;

				mpart = bodystructure->bd_data.bd_body_mpart;
				if (mpart && mpart->bd_list) {
					for (pit = clist_begin (mpart->bd_list); pit; pit = clist_next (pit)) {
						struct mailimap_body *part;
						struct mailimap_body_type_1part *onepart;
						struct mailimap_body_fld_dsp *dsp;

						part = (struct mailimap_body *) clist_content (pit);
						if (part->bd_type != MAILIMAP_BODY_1PART)
							continue;
						onepart = part->bd_data.bd_body_1part;
						if (!onepart || !onepart->bd_ext_1part)
							continue;
						dsp = onepart->bd_ext_1part->bd_disposition;
						if (!dsp || !dsp->dsp_type)
							continue;
						if (g_ascii_strcasecmp (dsp->dsp_type, "attachment") == 0) {
							/* Extract filename from disposition params */
							const gchar *filename = NULL;
							if (dsp->dsp_attributes) {
								clistiter *plit;
								for (plit = clist_begin (dsp->dsp_attributes->pa_list);
									 plit; plit = clist_next (plit)) {
									struct mailimap_single_body_fld_param *param;
									param = (struct mailimap_single_body_fld_param *) clist_content (plit);
									if (param->pa_name &&
										g_ascii_strcasecmp (param->pa_name, "filename") == 0) {
										filename = param->pa_value;
										break;
									}
								}
							}
							if (first_att) {
								g_print ("Attachments: ");
								first_att = FALSE;
							} else {
								g_print (", ");
							}
							g_print ("%s", filename ? filename : "(unnamed)");
						}
					}
					if (!first_att)
						g_print ("\n");
				}
			}

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

	/* Finalize JSON output */
	if (json_output && jb) {
		JsonNode *jnode;
		JsonGenerator *gen;
		gchar *json_out;

		json_builder_end_array (jb);
		jnode = json_builder_get_root (jb);
		gen = json_generator_new ();
		json_generator_set_pretty (gen, TRUE);
		json_generator_set_root (gen, jnode);
		json_out = json_generator_to_data (gen, NULL);
		g_print ("%s\n", json_out);
		g_free (json_out);
		json_node_unref (jnode);
		g_object_unref (gen);
	}

	ret = 0;

cleanup:
	if (jb)
		g_object_unref (jb);
	if (fetch_result)
		mailimap_fetch_list_free (fetch_result);
	if (set)
		mailimap_set_free (set);
	if (imap) {
		mailimap_logout (imap);
		mailimap_free (imap);
	}
	g_free (folder);
	g_free (from_filter);
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
	gboolean no_expunge = FALSE;

	GOptionEntry entries[] = {
		{ "folder",      'f', 0, G_OPTION_ARG_STRING,  &folder,      "IMAP folder",              "NAME" },
		{ "no-expunge",   0,  0, G_OPTION_ARG_NONE,    &no_expunge,  "Mark deleted but don't expunge", NULL },
		{ "config",       0,  0, G_OPTION_ARG_STRING,  &config_path, "Config file path",         "PATH" },
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

	/* Expunge to permanently remove (unless --no-expunge) */
	if (!no_expunge) {
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
 * Folders command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_folders (EmailConfig *cfg, int argc, char **argv)
{
	gchar *pattern = NULL;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "pattern", 'p', 0, G_OPTION_ARG_STRING, &pattern,     "Filter pattern (glob)", "PATTERN" },
		{ "config",   0,  0, G_OPTION_ARG_STRING, &config_path, "Config file path",      "PATH" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	mailimap *imap = NULL;
	gchar *err_msg = NULL;
	clist *result = NULL;
	clistiter *it;
	GList *folder_names = NULL;
	GList *li;
	gint r;
	gint ret = 1;

	ctx = g_option_context_new ("- list IMAP folders");
	g_option_context_add_main_entries (ctx, entries, NULL);
	if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
		g_printerr ("folders: %s\n", error->message);
		g_error_free (error);
		g_option_context_free (ctx);
		return 1;
	}
	g_option_context_free (ctx);

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		g_printerr ("folders: %s\n", err_msg);
		g_free (err_msg);
		goto cleanup;
	}

	r = mailimap_list (imap, "", "*", &result);
	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("folders: LIST failed (error %d)\n", r);
		goto cleanup;
	}

	/* Collect folder names, optionally filtering */
	for (it = clist_begin (result); it; it = clist_next (it)) {
		struct mailimap_mailbox_list *mb;
		mb = (struct mailimap_mailbox_list *) clist_content (it);
		if (!mb || !mb->mb_name)
			continue;

		if (pattern && !g_pattern_match_simple (pattern, mb->mb_name))
			continue;

		folder_names = g_list_prepend (folder_names, g_strdup (mb->mb_name));
	}

	/* Sort alphabetically and print */
	folder_names = g_list_sort (folder_names, compare_strings);
	for (li = folder_names; li; li = li->next)
		g_print ("%s\n", (const gchar *) li->data);

	ret = 0;

cleanup:
	if (folder_names) {
		g_list_free_full (folder_names, g_free);
	}
	if (result)
		mailimap_list_result_free (result);
	if (imap) {
		mailimap_logout (imap);
		mailimap_free (imap);
	}
	g_free (pattern);
	g_free (config_path);
	return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Move command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_move (EmailConfig *cfg, int argc, char **argv)
{
	gchar *folder = NULL;
	gchar *dest = NULL;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "folder", 'f', 0, G_OPTION_ARG_STRING, &folder,      "Source folder",    "NAME" },
		{ "dest",   'd', 0, G_OPTION_ARG_STRING, &dest,        "Destination folder", "NAME" },
		{ "config",  0,  0, G_OPTION_ARG_STRING, &config_path, "Config file path", "PATH" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	mailimap *imap = NULL;
	gchar *err_msg = NULL;
	gint r;
	gint ret = 1;
	gint i;

	ctx = g_option_context_new ("UID [UID...] - move emails between folders");
	g_option_context_add_main_entries (ctx, entries, NULL);
	if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
		g_printerr ("move: %s\n", error->message);
		g_error_free (error);
		g_option_context_free (ctx);
		return 1;
	}
	g_option_context_free (ctx);

	if (!dest) {
		g_printerr ("move: --dest is required\n");
		goto cleanup;
	}
	if (argc < 2) {
		g_printerr ("move: at least one UID is required\n");
		goto cleanup;
	}
	if (!folder)
		folder = g_strdup ("INBOX");

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		g_printerr ("move: %s\n", err_msg);
		g_free (err_msg);
		goto cleanup;
	}

	r = mailimap_select (imap, folder);
	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("move: failed to select folder '%s' (error %d)\n", folder, r);
		goto cleanup;
	}

	/* Build UID set from all positional args */
	{
		struct mailimap_set *uid_set;

		uid_set = mailimap_set_new_empty ();
		for (i = 1; i < argc; i++) {
			uint32_t uid;
			uid = (uint32_t) g_ascii_strtoull (argv[i], NULL, 10);
			if (uid == 0) {
				g_printerr ("move: invalid UID '%s'\n", argv[i]);
				continue;
			}
			mailimap_set_add_single (uid_set, uid);
		}

		/* Try MOVE extension first (atomic COPY+DELETE) */
		r = mailimap_uid_move (imap, uid_set, dest);
		if (r != MAILIMAP_NO_ERROR) {
			/* Fallback: COPY + flag \Deleted + expunge */
			r = mailimap_uid_copy (imap, uid_set, dest);
			if (r != MAILIMAP_NO_ERROR) {
				g_printerr ("move: COPY failed (error %d)\n", r);
				mailimap_set_free (uid_set);
				goto cleanup;
			}

			/* Flag as deleted in source */
			{
				struct mailimap_flag *flag;
				struct mailimap_flag_list *flag_list;
				struct mailimap_store_att_flags *store_flags;

				flag = mailimap_flag_new_deleted ();
				flag_list = mailimap_flag_list_new_empty ();
				mailimap_flag_list_add (flag_list, flag);
				store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
				mailimap_uid_store (imap, uid_set, store_flags);
				mailimap_store_att_flags_free (store_flags);
			}

			mailimap_expunge (imap);
		}
		mailimap_set_free (uid_set);
	}

	for (i = 1; i < argc; i++)
		g_print ("Moved UID %s to %s\n", argv[i], dest);

	ret = 0;

cleanup:
	if (imap) {
		mailimap_logout (imap);
		mailimap_free (imap);
	}
	g_free (folder);
	g_free (dest);
	g_free (config_path);
	return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Count command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_count (EmailConfig *cfg, int argc, char **argv)
{
	gchar *folder = NULL;
	gboolean json_output = FALSE;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "folder", 'f', 0, G_OPTION_ARG_STRING, &folder,       "IMAP folder",    "NAME" },
		{ "json",    0,  0, G_OPTION_ARG_NONE,   &json_output,  "Output as JSON",  NULL },
		{ "config",  0,  0, G_OPTION_ARG_STRING, &config_path,  "Config file path", "PATH" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	mailimap *imap = NULL;
	gchar *err_msg = NULL;
	struct mailimap_status_att_list *status_att;
	struct mailimap_mailbox_data_status *status_result = NULL;
	clistiter *it;
	gint r;
	gint ret = 1;
	guint32 total_count = 0;
	guint32 unseen_count = 0;

	ctx = g_option_context_new ("- show message counts");
	g_option_context_add_main_entries (ctx, entries, NULL);
	if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
		g_printerr ("count: %s\n", error->message);
		g_error_free (error);
		g_option_context_free (ctx);
		return 1;
	}
	g_option_context_free (ctx);

	if (!folder)
		folder = g_strdup ("INBOX");

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		g_printerr ("count: %s\n", err_msg);
		g_free (err_msg);
		goto cleanup;
	}

	/* Build STATUS attribute list */
	status_att = mailimap_status_att_list_new_empty ();
	mailimap_status_att_list_add (status_att, MAILIMAP_STATUS_ATT_MESSAGES);
	mailimap_status_att_list_add (status_att, MAILIMAP_STATUS_ATT_UNSEEN);

	r = mailimap_status (imap, folder, status_att, &status_result);
	mailimap_status_att_list_free (status_att);

	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("count: STATUS failed (error %d)\n", r);
		goto cleanup;
	}

	/* Parse results */
	if (status_result && status_result->st_info_list) {
		for (it = clist_begin (status_result->st_info_list); it; it = clist_next (it)) {
			struct mailimap_status_info *info;
			info = (struct mailimap_status_info *) clist_content (it);
			if (info->st_att == MAILIMAP_STATUS_ATT_MESSAGES)
				total_count = info->st_value;
			else if (info->st_att == MAILIMAP_STATUS_ATT_UNSEEN)
				unseen_count = info->st_value;
		}
	}

	if (json_output) {
		g_print ("{\"folder\":\"%s\",\"total\":%u,\"unread\":%u}\n",
			folder, (unsigned) total_count, (unsigned) unseen_count);
	} else {
		g_print ("%s: %u total, %u unread\n",
			folder, (unsigned) total_count, (unsigned) unseen_count);
	}

	ret = 0;

cleanup:
	if (status_result)
		mailimap_mailbox_data_status_free (status_result);
	if (imap) {
		mailimap_logout (imap);
		mailimap_free (imap);
	}
	g_free (folder);
	g_free (config_path);
	return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Search command
 * ═══════════════════════════════════════════════════════════════════════════ */

static gint
cmd_search (EmailConfig *cfg, int argc, char **argv)
{
	gchar *folder = NULL;
	gchar *from_str = NULL;
	gchar *subject_str = NULL;
	gchar *since_str = NULL;
	gchar *before_str = NULL;
	gchar *body_str = NULL;
	gboolean unread = FALSE;
	gboolean flagged = FALSE;
	gint limit = 25;
	gboolean json_output = FALSE;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "folder",  'f', 0, G_OPTION_ARG_STRING, &folder,      "IMAP folder",          "NAME" },
		{ "from",     0,  0, G_OPTION_ARG_STRING, &from_str,    "From address",         "ADDR" },
		{ "subject",  0,  0, G_OPTION_ARG_STRING, &subject_str, "Subject keyword",      "TEXT" },
		{ "since",    0,  0, G_OPTION_ARG_STRING, &since_str,   "Since date (YYYY-MM-DD)", "DATE" },
		{ "before",   0,  0, G_OPTION_ARG_STRING, &before_str,  "Before date (YYYY-MM-DD)", "DATE" },
		{ "body",     0,  0, G_OPTION_ARG_STRING, &body_str,    "Body keyword",         "TEXT" },
		{ "unread",  'u', 0, G_OPTION_ARG_NONE,   &unread,      "Only unread",           NULL },
		{ "flagged",  0,  0, G_OPTION_ARG_NONE,   &flagged,     "Only flagged",          NULL },
		{ "limit",   'n', 0, G_OPTION_ARG_INT,    &limit,       "Max results",          "N" },
		{ "json",     0,  0, G_OPTION_ARG_NONE,   &json_output, "Output as JSON",        NULL },
		{ "config",   0,  0, G_OPTION_ARG_STRING, &config_path, "Config file path",     "PATH" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	mailimap *imap = NULL;
	gchar *err_msg = NULL;
	struct mailimap_search_key *compound_key = NULL;
	clist *key_list = NULL;
	clist *search_result = NULL;
	clistiter *sit;
	struct mailimap_set *uid_set = NULL;
	struct mailimap_fetch_type *fetch_type = NULL;
	struct mailimap_fetch_att *fetch_att;
	clist *fetch_result = NULL;
	clistiter *fit;
	uint32_t count;
	gint r;
	gint ret = 1;
	JsonBuilder *jb = NULL;

	ctx = g_option_context_new ("- search emails");
	g_option_context_add_main_entries (ctx, entries, NULL);
	if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
		g_printerr ("search: %s\n", error->message);
		g_error_free (error);
		g_option_context_free (ctx);
		return 1;
	}
	g_option_context_free (ctx);

	if (!folder)
		folder = g_strdup ("INBOX");

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		g_printerr ("search: %s\n", err_msg);
		g_free (err_msg);
		goto cleanup;
	}

	r = mailimap_select (imap, folder);
	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("search: failed to select folder '%s' (error %d)\n", folder, r);
		goto cleanup;
	}

	/* Build compound search key from provided criteria */
	key_list = clist_new ();

	/* mailimap_search_key_new params:
	 * (type, bcc, before, body, cc, from, keyword, on, since,
	 *  subject, text, to, unkeyword, header_name, header_value,
	 *  larger, not, or1, or2, sentbefore, senton, sentsince,
	 *  smaller, uid, set, multiple) */

	if (from_str) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_FROM,
			NULL, NULL, NULL, NULL,              /* bcc, before, body, cc */
			g_strdup (from_str),                 /* from */
			NULL, NULL, NULL, NULL, NULL, NULL,  /* keyword..to */
			NULL, NULL, NULL, 0,                 /* unkeyword..larger */
			NULL, NULL, NULL,                    /* not, or1, or2 */
			NULL, NULL, NULL, 0,                 /* sentbefore..smaller */
			NULL, NULL, NULL);                   /* uid, set, multiple */
		clist_append (key_list, k);
	}

	if (subject_str) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_SUBJECT,
			NULL, NULL, NULL, NULL,              /* bcc, before, body, cc */
			NULL, NULL, NULL, NULL,              /* from, keyword, on, since */
			g_strdup (subject_str),              /* subject */
			NULL, NULL,                          /* text, to */
			NULL, NULL, NULL, 0,                 /* unkeyword..larger */
			NULL, NULL, NULL,                    /* not, or1, or2 */
			NULL, NULL, NULL, 0,                 /* sentbefore..smaller */
			NULL, NULL, NULL);                   /* uid, set, multiple */
		clist_append (key_list, k);
	}

	if (body_str) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_BODY,
			NULL, NULL,                          /* bcc, before */
			g_strdup (body_str),                 /* body */
			NULL, NULL, NULL, NULL, NULL,        /* cc..since */
			NULL, NULL, NULL,                    /* subject, text, to */
			NULL, NULL, NULL, 0,                 /* unkeyword..larger */
			NULL, NULL, NULL,                    /* not, or1, or2 */
			NULL, NULL, NULL, 0,                 /* sentbefore..smaller */
			NULL, NULL, NULL);                   /* uid, set, multiple */
		clist_append (key_list, k);
	}

	if (since_str) {
		gint year = 0, month = 0, day = 0;
		if (sscanf (since_str, "%d-%d-%d", &year, &month, &day) == 3) {
			struct mailimap_date *date;
			struct mailimap_search_key *k;
			date = mailimap_date_new (day, month, year);
			k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_SINCE,
				NULL, NULL, NULL, NULL,          /* bcc, before, body, cc */
				NULL, NULL, NULL,                /* from, keyword, on */
				date,                            /* since */
				NULL, NULL, NULL,                /* subject, text, to */
				NULL, NULL, NULL, 0,             /* unkeyword..larger */
				NULL, NULL, NULL,                /* not, or1, or2 */
				NULL, NULL, NULL, 0,             /* sentbefore..smaller */
				NULL, NULL, NULL);               /* uid, set, multiple */
			clist_append (key_list, k);
		} else {
			g_printerr ("search: invalid --since date format (use YYYY-MM-DD)\n");
			goto cleanup;
		}
	}

	if (before_str) {
		gint year = 0, month = 0, day = 0;
		if (sscanf (before_str, "%d-%d-%d", &year, &month, &day) == 3) {
			struct mailimap_date *date;
			struct mailimap_search_key *k;
			date = mailimap_date_new (day, month, year);
			k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_BEFORE,
				NULL,                            /* bcc */
				date,                            /* before */
				NULL, NULL, NULL, NULL, NULL,    /* body..on */
				NULL, NULL, NULL, NULL,          /* since..to */
				NULL, NULL, NULL, 0,             /* unkeyword..larger */
				NULL, NULL, NULL,                /* not, or1, or2 */
				NULL, NULL, NULL, 0,             /* sentbefore..smaller */
				NULL, NULL, NULL);               /* uid, set, multiple */
			clist_append (key_list, k);
		} else {
			g_printerr ("search: invalid --before date format (use YYYY-MM-DD)\n");
			goto cleanup;
		}
	}

	if (unread) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_UNSEEN,
			NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			NULL, NULL, NULL, 0, NULL, NULL, NULL);
		clist_append (key_list, k);
	}

	if (flagged) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_FLAGGED,
			NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			NULL, NULL, NULL, 0, NULL, NULL, NULL);
		clist_append (key_list, k);
	}

	if (clist_isempty (key_list)) {
		/* No criteria given — match all */
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_ALL,
			NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			NULL, NULL, NULL, 0, NULL, NULL, NULL);
		clist_append (key_list, k);
	}

	if (clist_count (key_list) == 1) {
		compound_key = (struct mailimap_search_key *) clist_content (clist_begin (key_list));
		clist_free (key_list);
		key_list = NULL;
	} else {
		compound_key = mailimap_search_key_new_multiple (key_list);
		key_list = NULL; /* ownership transferred */
	}

	r = mailimap_uid_search (imap, NULL, compound_key, &search_result);
	mailimap_search_key_free (compound_key);
	compound_key = NULL;

	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("search: IMAP search failed (error %d)\n", r);
		goto cleanup;
	}

	/* Collect UIDs up to limit */
	count = 0;
	uid_set = mailimap_set_new_empty ();
	for (sit = clist_begin (search_result); sit; sit = clist_next (sit)) {
		uint32_t *uid_p = (uint32_t *) clist_content (sit);
		mailimap_set_add_single (uid_set, *uid_p);
		count++;
		if ((gint) count >= limit)
			break;
	}
	mailimap_search_result_free (search_result);
	search_result = NULL;

	if (count == 0) {
		if (json_output)
			g_print ("[]\n");
		else
			g_print ("No matching messages.\n");
		ret = 0;
		goto cleanup;
	}

	/* Fetch envelopes for matching UIDs */
	fetch_type = mailimap_fetch_type_new_fetch_att_list_empty ();
	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_ENVELOPE, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);
	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_UID, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

	r = mailimap_uid_fetch (imap, uid_set, fetch_type, &fetch_result);
	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("search: fetch failed (error %d)\n", r);
		goto cleanup;
	}

	/* Display results */
	if (json_output) {
		jb = json_builder_new ();
		json_builder_begin_array (jb);
	} else {
		g_print ("%-8s  %-26s  %-24s  %s\n", "UID", "Date", "From", "Subject");
		g_print ("%-8s  %-26s  %-24s  %s\n", "--------", "--------------------------",
				 "------------------------", "-------");
	}

	for (fit = clist_begin (fetch_result); fit; fit = clist_next (fit)) {
		struct mailimap_msg_att *msg_att;
		clistiter *ait;
		struct mailimap_envelope *env = NULL;
		uint32_t uid = 0;

		msg_att = (struct mailimap_msg_att *) clist_content (fit);
		for (ait = clist_begin (msg_att->att_list); ait; ait = clist_next (ait)) {
			struct mailimap_msg_att_item *item;
			item = (struct mailimap_msg_att_item *) clist_content (ait);
			if (item->att_type == MAILIMAP_MSG_ATT_ITEM_STATIC) {
				struct mailimap_msg_att_static *s = item->att_data.att_static;
				if (s->att_type == MAILIMAP_MSG_ATT_ENVELOPE)
					env = s->att_data.att_env;
				else if (s->att_type == MAILIMAP_MSG_ATT_UID)
					uid = s->att_data.att_uid;
			}
		}

		if (!env)
			continue;

		if (json_output) {
			json_builder_begin_object (jb);
			json_builder_set_member_name (jb, "uid");
			json_builder_add_int_value (jb, uid);
			json_builder_set_member_name (jb, "date");
			json_builder_add_string_value (jb, env->env_date ? env->env_date : "");
			json_builder_set_member_name (jb, "from");
			json_builder_add_string_value (jb, get_first_from_addr (env));
			json_builder_set_member_name (jb, "subject");
			json_builder_add_string_value (jb, env->env_subject ? env->env_subject : "");
			json_builder_end_object (jb);
		} else {
			gchar uid_str[16];
			gchar date_trunc[27];
			gchar from_trunc[25];

			g_snprintf (uid_str, sizeof (uid_str), "%u", (unsigned) uid);
			if (env->env_date)
				g_strlcpy (date_trunc, env->env_date, sizeof (date_trunc));
			else
				g_strlcpy (date_trunc, "(none)", sizeof (date_trunc));
			g_strlcpy (from_trunc, get_first_from_addr (env), sizeof (from_trunc));

			g_print ("%-8s  %-26s  %-24s  %s\n",
				uid_str, date_trunc, from_trunc,
				env->env_subject ? env->env_subject : "(none)");
		}
	}

	/* Finalize JSON */
	if (json_output && jb) {
		JsonNode *jnode;
		JsonGenerator *gen;
		gchar *json_out;

		json_builder_end_array (jb);
		jnode = json_builder_get_root (jb);
		gen = json_generator_new ();
		json_generator_set_pretty (gen, TRUE);
		json_generator_set_root (gen, jnode);
		json_out = json_generator_to_data (gen, NULL);
		g_print ("%s\n", json_out);
		g_free (json_out);
		json_node_unref (jnode);
		g_object_unref (gen);
	}

	ret = 0;

cleanup:
	if (jb)
		g_object_unref (jb);
	if (fetch_result)
		mailimap_fetch_list_free (fetch_result);
	if (uid_set)
		mailimap_set_free (uid_set);
	if (search_result)
		mailimap_search_result_free (search_result);
	if (key_list) {
		/* Free remaining search keys */
		clistiter *ki;
		for (ki = clist_begin (key_list); ki; ki = clist_next (ki))
			mailimap_search_key_free ((struct mailimap_search_key *) clist_content (ki));
		clist_free (key_list);
	}
	if (imap) {
		mailimap_logout (imap);
		mailimap_free (imap);
	}
	g_free (folder);
	g_free (from_str);
	g_free (subject_str);
	g_free (since_str);
	g_free (before_str);
	g_free (body_str);
	g_free (config_path);
	return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * Flag command
 * ═══════════════════════════════════════════════════════════════════════════ */

static struct mailimap_flag *
parse_flag_name (const gchar *name)
{
	if (!name)
		return NULL;
	if (g_ascii_strcasecmp (name, "seen") == 0)
		return mailimap_flag_new_seen ();
	if (g_ascii_strcasecmp (name, "flagged") == 0)
		return mailimap_flag_new_flagged ();
	if (g_ascii_strcasecmp (name, "answered") == 0)
		return mailimap_flag_new_answered ();
	if (g_ascii_strcasecmp (name, "deleted") == 0)
		return mailimap_flag_new_deleted ();
	if (g_ascii_strcasecmp (name, "draft") == 0)
		return mailimap_flag_new_draft ();
	return NULL;
}

static gint
cmd_flag (EmailConfig *cfg, int argc, char **argv)
{
	gchar *folder = NULL;
	gchar *add_flag = NULL;
	gchar *remove_flag = NULL;
	gchar *config_path = NULL;

	GOptionEntry entries[] = {
		{ "folder",  'f', 0, G_OPTION_ARG_STRING, &folder,       "IMAP folder",      "NAME" },
		{ "add",      0,  0, G_OPTION_ARG_STRING, &add_flag,     "Add flag",          "FLAG" },
		{ "remove",   0,  0, G_OPTION_ARG_STRING, &remove_flag,  "Remove flag",       "FLAG" },
		{ "config",   0,  0, G_OPTION_ARG_STRING, &config_path,  "Config file path",  "PATH" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	mailimap *imap = NULL;
	gchar *err_msg = NULL;
	gint r;
	gint ret = 1;
	gint i;

	ctx = g_option_context_new ("UID [UID...] - manage message flags");
	g_option_context_add_main_entries (ctx, entries, NULL);
	if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
		g_printerr ("flag: %s\n", error->message);
		g_error_free (error);
		g_option_context_free (ctx);
		return 1;
	}
	g_option_context_free (ctx);

	if (!add_flag && !remove_flag) {
		g_printerr ("flag: --add or --remove is required\n");
		goto cleanup;
	}
	if (add_flag && remove_flag) {
		g_printerr ("flag: use --add or --remove, not both\n");
		goto cleanup;
	}
	if (argc < 2) {
		g_printerr ("flag: at least one UID is required\n");
		goto cleanup;
	}
	if (!folder)
		folder = g_strdup ("INBOX");

	/* Validate flag name */
	{
		const gchar *flag_name = add_flag ? add_flag : remove_flag;
		struct mailimap_flag *test_flag = parse_flag_name (flag_name);
		if (!test_flag) {
			g_printerr ("flag: unknown flag '%s' (valid: seen, flagged, answered, deleted, draft)\n",
				flag_name);
			goto cleanup;
		}
		mailimap_flag_free (test_flag);
	}

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		g_printerr ("flag: %s\n", err_msg);
		g_free (err_msg);
		goto cleanup;
	}

	r = mailimap_select (imap, folder);
	if (r != MAILIMAP_NO_ERROR) {
		g_printerr ("flag: failed to select folder '%s' (error %d)\n", folder, r);
		goto cleanup;
	}

	for (i = 1; i < argc; i++) {
		uint32_t uid;
		struct mailimap_flag *flag;
		struct mailimap_flag_list *flag_list;
		struct mailimap_store_att_flags *store_flags;
		struct mailimap_set *uid_set;
		const gchar *flag_name;

		uid = (uint32_t) g_ascii_strtoull (argv[i], NULL, 10);
		if (uid == 0) {
			g_printerr ("flag: invalid UID '%s'\n", argv[i]);
			continue;
		}

		flag_name = add_flag ? add_flag : remove_flag;
		flag = parse_flag_name (flag_name);
		flag_list = mailimap_flag_list_new_empty ();
		mailimap_flag_list_add (flag_list, flag);

		if (add_flag)
			store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
		else
			store_flags = mailimap_store_att_flags_new_remove_flags (flag_list);

		uid_set = mailimap_set_new_single (uid);
		r = mailimap_uid_store (imap, uid_set, store_flags);
		mailimap_set_free (uid_set);
		mailimap_store_att_flags_free (store_flags);

		if (r != MAILIMAP_NO_ERROR) {
			g_printerr ("flag: failed to update UID %u (error %d)\n", (unsigned) uid, r);
		} else {
			if (add_flag)
				g_print ("Added '%s' to UID %u\n", add_flag, (unsigned) uid);
			else
				g_print ("Removed '%s' from UID %u\n", remove_flag, (unsigned) uid);
		}
	}

	ret = 0;

cleanup:
	if (imap) {
		mailimap_logout (imap);
		mailimap_free (imap);
	}
	g_free (folder);
	g_free (add_flag);
	g_free (remove_flag);
	g_free (config_path);
	return ret;
}

/* ═══════════════════════════════════════════════════════════════════════════
 * MCP Server
 * ═══════════════════════════════════════════════════════════════════════════ */

typedef struct {
	EmailConfig *cfg;
	gchar      **to_whitelist;
	gchar      **from_whitelist;
} McpContext;

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
"     \"html\":{\"type\":\"boolean\",\"description\":\"Send as HTML\"},"
"     \"reply_to\":{\"type\":\"string\",\"description\":\"Reply-To address\"},"
"     \"in_reply_to\":{\"type\":\"string\",\"description\":\"In-Reply-To Message-ID for threading\"}"
"   },\"required\":[\"to\",\"subject\",\"body\"]}},"
"  {\"name\":\"email_read\","
"   \"description\":\"Read emails from IMAP. Returns JSON array of messages.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"},"
"     \"limit\":{\"type\":\"integer\",\"description\":\"Max messages (default: 10)\"},"
"     \"unread\":{\"type\":\"boolean\",\"description\":\"Only unread messages\"},"
"     \"uid\":{\"type\":\"integer\",\"description\":\"Fetch specific message by UID\"},"
"     \"full\":{\"type\":\"boolean\",\"description\":\"Include message body\"},"
"     \"mark_read\":{\"type\":\"boolean\",\"description\":\"Mark fetched as read\"},"
"     \"from\":{\"type\":\"string\",\"description\":\"Filter by sender address\"}"
"   }}},"
"  {\"name\":\"email_rm\","
"   \"description\":\"Delete emails by UID.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"uids\":{\"type\":\"array\",\"items\":{\"type\":\"integer\"},\"description\":\"UIDs to delete\"},"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"},"
"     \"no_expunge\":{\"type\":\"boolean\",\"description\":\"Mark deleted but don't expunge\"}"
"   },\"required\":[\"uids\"]}},"
"  {\"name\":\"email_move\","
"   \"description\":\"Move messages between IMAP folders.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"uids\":{\"type\":\"array\",\"items\":{\"type\":\"integer\"},\"description\":\"UIDs to move\"},"
"     \"folder\":{\"type\":\"string\",\"description\":\"Source folder (default: INBOX)\"},"
"     \"dest\":{\"type\":\"string\",\"description\":\"Destination folder\"}"
"   },\"required\":[\"uids\",\"dest\"]}},"
"  {\"name\":\"email_folders\","
"   \"description\":\"List IMAP folders.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"pattern\":{\"type\":\"string\",\"description\":\"Glob pattern to filter folders\"}"
"   }}},"
"  {\"name\":\"email_count\","
"   \"description\":\"Get message counts for a folder.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"}"
"   }}},"
"  {\"name\":\"email_flag\","
"   \"description\":\"Add or remove flags on messages.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"uids\":{\"type\":\"array\",\"items\":{\"type\":\"integer\"},\"description\":\"UIDs to modify\"},"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"},"
"     \"add\":{\"type\":\"string\",\"description\":\"Flag to add (seen, flagged, answered, deleted, draft)\"},"
"     \"remove\":{\"type\":\"string\",\"description\":\"Flag to remove\"}"
"   },\"required\":[\"uids\"]}},"
"  {\"name\":\"email_search\","
"   \"description\":\"Search emails by criteria.\","
"   \"inputSchema\":{\"type\":\"object\",\"properties\":{"
"     \"from\":{\"type\":\"string\",\"description\":\"From address\"},"
"     \"subject\":{\"type\":\"string\",\"description\":\"Subject keyword\"},"
"     \"since\":{\"type\":\"string\",\"description\":\"Since date (YYYY-MM-DD)\"},"
"     \"before\":{\"type\":\"string\",\"description\":\"Before date (YYYY-MM-DD)\"},"
"     \"body\":{\"type\":\"string\",\"description\":\"Body keyword\"},"
"     \"unread\":{\"type\":\"boolean\",\"description\":\"Only unread messages\"},"
"     \"flagged\":{\"type\":\"boolean\",\"description\":\"Only flagged messages\"},"
"     \"folder\":{\"type\":\"string\",\"description\":\"IMAP folder (default: INBOX)\"},"
"     \"limit\":{\"type\":\"integer\",\"description\":\"Max results (default: 25)\"}"
"   }}}"
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
mcp_handle_send (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	JsonArray *to_arr;
	JsonArray *cc_arr;
	JsonArray *bcc_arr;
	const gchar *subject;
	const gchar *body;
	const gchar *reply_to;
	const gchar *in_reply_to;
	gboolean html;
	GString *msg;
	gchar *date_str;
	gchar *message_id;
	mailsmtp *smtp;
	clist *rcpts;
	gint r;
	guint i;

	if (!json_object_has_member (args, "to"))
		return _mcp_text_result ("Missing required field: to", TRUE);

	to_arr = json_object_get_array_member (args, "to");
	if (!to_arr || json_array_get_length (to_arr) == 0)
		return _mcp_text_result ("'to' must have at least one address", TRUE);

	/* Validate all recipients against to-whitelist */
	if (mctx->to_whitelist) {
		for (i = 0; i < json_array_get_length (to_arr); i++) {
			const gchar *addr = json_array_get_string_element (to_arr, i);
			if (!is_whitelisted (addr, mctx->to_whitelist))
				return _mcp_text_result (
					g_strdup_printf ("Recipient not whitelisted: %s", addr), TRUE);
		}
		cc_arr = json_object_has_member (args, "cc") ? json_object_get_array_member (args, "cc") : NULL;
		if (cc_arr) {
			for (i = 0; i < json_array_get_length (cc_arr); i++) {
				const gchar *addr = json_array_get_string_element (cc_arr, i);
				if (!is_whitelisted (addr, mctx->to_whitelist))
					return _mcp_text_result (
						g_strdup_printf ("CC recipient not whitelisted: %s", addr), TRUE);
			}
		}
		bcc_arr = json_object_has_member (args, "bcc") ? json_object_get_array_member (args, "bcc") : NULL;
		if (bcc_arr) {
			for (i = 0; i < json_array_get_length (bcc_arr); i++) {
				const gchar *addr = json_array_get_string_element (bcc_arr, i);
				if (!is_whitelisted (addr, mctx->to_whitelist))
					return _mcp_text_result (
						g_strdup_printf ("BCC recipient not whitelisted: %s", addr), TRUE);
			}
		}
	}

	subject = _json_get_string (args, "subject", "(no subject)");
	body = _json_get_string (args, "body", "");
	html = _json_get_bool (args, "html", FALSE);
	reply_to = _json_get_string (args, "reply_to", NULL);
	in_reply_to = _json_get_string (args, "in_reply_to", NULL);

	/* Build RFC 2822 */
	date_str = format_rfc2822_date ();
	message_id = generate_message_id (cfg->from_addr);
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
	g_string_append_printf (msg, "Message-ID: %s\r\n", message_id);

	if (reply_to)
		g_string_append_printf (msg, "Reply-To: %s\r\n", reply_to);
	if (in_reply_to)
		g_string_append_printf (msg, "In-Reply-To: %s\r\n", in_reply_to);

	g_string_append (msg, "MIME-Version: 1.0\r\n");
	g_string_append_printf (msg, "Content-Type: %s; charset=UTF-8\r\n",
							html ? "text/html" : "text/plain");
	g_string_append (msg, "Content-Transfer-Encoding: 8bit\r\n\r\n");
	g_string_append (msg, body);
	g_free (date_str);
	g_free (message_id);

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
mcp_handle_read (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	const gchar *folder;
	gint64 limit;
	gboolean unread_only;
	gint64 uid_val;
	gboolean full;
	gboolean mark_read;
	const gchar *from_filter;
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
	from_filter = _json_get_string (args, "from", NULL);

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

		/* Filter by from-whitelist (MCP server restriction) */
		if (mctx->from_whitelist) {
			gchar *sender_email = get_first_from_email (env);
			gboolean allowed = is_whitelisted (sender_email, mctx->from_whitelist);
			g_free (sender_email);
			if (!allowed)
				continue;
		}

		/* Filter by from parameter */
		if (from_filter) {
			gchar *sender_email = get_first_from_email (env);
			gboolean matches = (sender_email && g_ascii_strcasecmp (sender_email, from_filter) == 0);
			g_free (sender_email);
			if (!matches)
				continue;
		}

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
mcp_handle_rm (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	JsonArray *uids_arr;
	const gchar *folder;
	gboolean no_expunge;
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
	no_expunge = _json_get_bool (args, "no_expunge", FALSE);

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

	if (!no_expunge)
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
mcp_handle_move (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	JsonArray *uids_arr;
	const gchar *folder;
	const gchar *dest;
	mailimap *imap;
	gchar *err_msg = NULL;
	struct mailimap_set *uid_set;
	gint r;
	guint i;

	if (!json_object_has_member (args, "uids"))
		return _mcp_text_result ("Missing required field: uids", TRUE);
	if (!json_object_has_member (args, "dest"))
		return _mcp_text_result ("Missing required field: dest", TRUE);

	uids_arr = json_object_get_array_member (args, "uids");
	if (!uids_arr || json_array_get_length (uids_arr) == 0)
		return _mcp_text_result ("'uids' must have at least one UID", TRUE);

	folder = _json_get_string (args, "folder", "INBOX");
	dest = json_object_get_string_member (args, "dest");

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		McpToolResult *res = _mcp_text_result (err_msg, TRUE);
		g_free (err_msg);
		return res;
	}

	r = mailimap_select (imap, folder);
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("Failed to select folder", TRUE);
	}

	uid_set = mailimap_set_new_empty ();
	for (i = 0; i < json_array_get_length (uids_arr); i++) {
		uint32_t uid = (uint32_t) json_array_get_int_element (uids_arr, i);
		mailimap_set_add_single (uid_set, uid);
	}

	/* Try MOVE extension first */
	r = mailimap_uid_move (imap, uid_set, dest);
	if (r != MAILIMAP_NO_ERROR) {
		/* Fallback: COPY + flag \Deleted + expunge */
		r = mailimap_uid_copy (imap, uid_set, dest);
		if (r != MAILIMAP_NO_ERROR) {
			mailimap_set_free (uid_set);
			mailimap_logout (imap);
			mailimap_free (imap);
			return _mcp_text_result ("MOVE/COPY failed", TRUE);
		}

		{
			struct mailimap_flag *flag;
			struct mailimap_flag_list *flag_list;
			struct mailimap_store_att_flags *store_flags;

			flag = mailimap_flag_new_deleted ();
			flag_list = mailimap_flag_list_new_empty ();
			mailimap_flag_list_add (flag_list, flag);
			store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
			mailimap_uid_store (imap, uid_set, store_flags);
			mailimap_store_att_flags_free (store_flags);
		}
		mailimap_expunge (imap);
	}

	mailimap_set_free (uid_set);
	mailimap_logout (imap);
	mailimap_free (imap);

	{
		g_autofree gchar *result = g_strdup_printf (
			"{\"status\":\"moved\",\"count\":%u,\"dest\":\"%s\"}",
			json_array_get_length (uids_arr), dest);
		return _mcp_text_result (result, FALSE);
	}
}

static McpToolResult *
mcp_handle_folders (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	const gchar *pattern;
	mailimap *imap;
	gchar *err_msg = NULL;
	clist *result = NULL;
	clistiter *it;
	GList *folder_names = NULL;
	GList *li;
	JsonBuilder *jb;
	JsonGenerator *gen;
	JsonNode *jnode;
	gchar *json_out;
	gint r;

	pattern = _json_get_string (args, "pattern", NULL);

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		McpToolResult *res = _mcp_text_result (err_msg, TRUE);
		g_free (err_msg);
		return res;
	}

	r = mailimap_list (imap, "", "*", &result);
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("LIST failed", TRUE);
	}

	for (it = clist_begin (result); it; it = clist_next (it)) {
		struct mailimap_mailbox_list *mb;
		mb = (struct mailimap_mailbox_list *) clist_content (it);
		if (!mb || !mb->mb_name)
			continue;
		if (pattern && !g_pattern_match_simple (pattern, mb->mb_name))
			continue;
		folder_names = g_list_prepend (folder_names, g_strdup (mb->mb_name));
	}

	folder_names = g_list_sort (folder_names, compare_strings);

	jb = json_builder_new ();
	json_builder_begin_array (jb);
	for (li = folder_names; li; li = li->next) {
		json_builder_add_string_value (jb, (const gchar *) li->data);
	}
	json_builder_end_array (jb);

	jnode = json_builder_get_root (jb);
	gen = json_generator_new ();
	json_generator_set_root (gen, jnode);
	json_out = json_generator_to_data (gen, NULL);
	json_node_unref (jnode);
	g_object_unref (gen);
	g_object_unref (jb);

	g_list_free_full (folder_names, g_free);
	mailimap_list_result_free (result);
	mailimap_logout (imap);
	mailimap_free (imap);

	{
		McpToolResult *res = _mcp_text_result (json_out, FALSE);
		g_free (json_out);
		return res;
	}
}

static McpToolResult *
mcp_handle_count (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	const gchar *folder;
	mailimap *imap;
	gchar *err_msg = NULL;
	struct mailimap_status_att_list *status_att;
	struct mailimap_mailbox_data_status *status_result = NULL;
	clistiter *it;
	gint r;
	guint32 total_count = 0;
	guint32 unseen_count = 0;

	folder = _json_get_string (args, "folder", "INBOX");

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		McpToolResult *res = _mcp_text_result (err_msg, TRUE);
		g_free (err_msg);
		return res;
	}

	status_att = mailimap_status_att_list_new_empty ();
	mailimap_status_att_list_add (status_att, MAILIMAP_STATUS_ATT_MESSAGES);
	mailimap_status_att_list_add (status_att, MAILIMAP_STATUS_ATT_UNSEEN);

	r = mailimap_status (imap, folder, status_att, &status_result);
	mailimap_status_att_list_free (status_att);

	if (r != MAILIMAP_NO_ERROR) {
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("STATUS failed", TRUE);
	}

	if (status_result && status_result->st_info_list) {
		for (it = clist_begin (status_result->st_info_list); it; it = clist_next (it)) {
			struct mailimap_status_info *info;
			info = (struct mailimap_status_info *) clist_content (it);
			if (info->st_att == MAILIMAP_STATUS_ATT_MESSAGES)
				total_count = info->st_value;
			else if (info->st_att == MAILIMAP_STATUS_ATT_UNSEEN)
				unseen_count = info->st_value;
		}
	}

	mailimap_mailbox_data_status_free (status_result);
	mailimap_logout (imap);
	mailimap_free (imap);

	{
		g_autofree gchar *result = g_strdup_printf (
			"{\"folder\":\"%s\",\"total\":%u,\"unread\":%u}",
			folder, (unsigned) total_count, (unsigned) unseen_count);
		return _mcp_text_result (result, FALSE);
	}
}

static McpToolResult *
mcp_handle_flag (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	JsonArray *uids_arr;
	const gchar *folder;
	const gchar *add_name;
	const gchar *remove_name;
	const gchar *flag_name;
	gboolean is_add;
	mailimap *imap;
	gchar *err_msg = NULL;
	gint r;
	guint i;
	guint updated = 0;

	if (!json_object_has_member (args, "uids"))
		return _mcp_text_result ("Missing required field: uids", TRUE);

	uids_arr = json_object_get_array_member (args, "uids");
	if (!uids_arr || json_array_get_length (uids_arr) == 0)
		return _mcp_text_result ("'uids' must have at least one UID", TRUE);

	folder = _json_get_string (args, "folder", "INBOX");
	add_name = _json_get_string (args, "add", NULL);
	remove_name = _json_get_string (args, "remove", NULL);

	if (!add_name && !remove_name)
		return _mcp_text_result ("Either 'add' or 'remove' is required", TRUE);
	if (add_name && remove_name)
		return _mcp_text_result ("Use 'add' or 'remove', not both", TRUE);

	is_add = (add_name != NULL);
	flag_name = is_add ? add_name : remove_name;

	/* Validate flag name */
	{
		struct mailimap_flag *test = parse_flag_name (flag_name);
		if (!test)
			return _mcp_text_result ("Unknown flag (valid: seen, flagged, answered, deleted, draft)", TRUE);
		mailimap_flag_free (test);
	}

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		McpToolResult *res = _mcp_text_result (err_msg, TRUE);
		g_free (err_msg);
		return res;
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

		flag = parse_flag_name (flag_name);
		flag_list = mailimap_flag_list_new_empty ();
		mailimap_flag_list_add (flag_list, flag);

		if (is_add)
			store_flags = mailimap_store_att_flags_new_add_flags (flag_list);
		else
			store_flags = mailimap_store_att_flags_new_remove_flags (flag_list);

		uid_set = mailimap_set_new_single (uid);
		r = mailimap_uid_store (imap, uid_set, store_flags);
		mailimap_set_free (uid_set);
		mailimap_store_att_flags_free (store_flags);
		if (r == MAILIMAP_NO_ERROR)
			updated++;
	}

	mailimap_logout (imap);
	mailimap_free (imap);

	{
		g_autofree gchar *result = g_strdup_printf (
			"{\"status\":\"updated\",\"count\":%u,\"action\":\"%s\",\"flag\":\"%s\"}",
			updated, is_add ? "add" : "remove", flag_name);
		return _mcp_text_result (result, FALSE);
	}
}

static McpToolResult *
mcp_handle_search (McpContext *mctx, JsonObject *args)
{
	EmailConfig *cfg = mctx->cfg;
	const gchar *folder;
	const gchar *from_str;
	const gchar *subject_str;
	const gchar *since_str;
	const gchar *before_str;
	const gchar *body_str;
	gboolean unread;
	gboolean flagged;
	gint64 limit;
	mailimap *imap;
	gchar *err_msg = NULL;
	struct mailimap_search_key *compound_key = NULL;
	clist *key_list = NULL;
	clist *search_result = NULL;
	clistiter *sit;
	struct mailimap_set *uid_set = NULL;
	struct mailimap_fetch_type *fetch_type = NULL;
	struct mailimap_fetch_att *fetch_att;
	clist *fetch_result = NULL;
	clistiter *fit;
	uint32_t count;
	gint r;
	JsonBuilder *jb;
	JsonGenerator *gen;
	JsonNode *jnode;
	gchar *json_out;

	folder = _json_get_string (args, "folder", "INBOX");
	from_str = _json_get_string (args, "from", NULL);
	subject_str = _json_get_string (args, "subject", NULL);
	since_str = _json_get_string (args, "since", NULL);
	before_str = _json_get_string (args, "before", NULL);
	body_str = _json_get_string (args, "body", NULL);
	unread = _json_get_bool (args, "unread", FALSE);
	flagged = _json_get_bool (args, "flagged", FALSE);
	limit = _json_get_int (args, "limit", 25);

	imap = imap_open (cfg, &err_msg);
	if (!imap) {
		McpToolResult *res = _mcp_text_result (err_msg, TRUE);
		g_free (err_msg);
		return res;
	}

	r = mailimap_select (imap, folder);
	if (r != MAILIMAP_NO_ERROR) {
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("Failed to select folder", TRUE);
	}

	key_list = clist_new ();

	if (from_str) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_FROM,
			NULL, NULL, NULL, NULL,              /* bcc, before, body, cc */
			g_strdup (from_str),                 /* from */
			NULL, NULL, NULL, NULL, NULL, NULL,  /* keyword..to */
			NULL, NULL, NULL, 0,                 /* unkeyword..larger */
			NULL, NULL, NULL,                    /* not, or1, or2 */
			NULL, NULL, NULL, 0,                 /* sentbefore..smaller */
			NULL, NULL, NULL);                   /* uid, set, multiple */
		clist_append (key_list, k);
	}

	if (subject_str) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_SUBJECT,
			NULL, NULL, NULL, NULL,              /* bcc, before, body, cc */
			NULL, NULL, NULL, NULL,              /* from, keyword, on, since */
			g_strdup (subject_str),              /* subject */
			NULL, NULL,                          /* text, to */
			NULL, NULL, NULL, 0,                 /* unkeyword..larger */
			NULL, NULL, NULL,                    /* not, or1, or2 */
			NULL, NULL, NULL, 0,                 /* sentbefore..smaller */
			NULL, NULL, NULL);                   /* uid, set, multiple */
		clist_append (key_list, k);
	}

	if (body_str) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_BODY,
			NULL, NULL,                          /* bcc, before */
			g_strdup (body_str),                 /* body */
			NULL, NULL, NULL, NULL, NULL,        /* cc..since */
			NULL, NULL, NULL,                    /* subject, text, to */
			NULL, NULL, NULL, 0,                 /* unkeyword..larger */
			NULL, NULL, NULL,                    /* not, or1, or2 */
			NULL, NULL, NULL, 0,                 /* sentbefore..smaller */
			NULL, NULL, NULL);                   /* uid, set, multiple */
		clist_append (key_list, k);
	}

	if (since_str) {
		gint year = 0, month = 0, day = 0;
		if (sscanf (since_str, "%d-%d-%d", &year, &month, &day) == 3) {
			struct mailimap_date *date;
			struct mailimap_search_key *k;
			date = mailimap_date_new (day, month, year);
			k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_SINCE,
				NULL, NULL, NULL, NULL,          /* bcc, before, body, cc */
				NULL, NULL, NULL,                /* from, keyword, on */
				date,                            /* since */
				NULL, NULL, NULL,                /* subject, text, to */
				NULL, NULL, NULL, 0,             /* unkeyword..larger */
				NULL, NULL, NULL,                /* not, or1, or2 */
				NULL, NULL, NULL, 0,             /* sentbefore..smaller */
				NULL, NULL, NULL);               /* uid, set, multiple */
			clist_append (key_list, k);
		}
	}

	if (before_str) {
		gint year = 0, month = 0, day = 0;
		if (sscanf (before_str, "%d-%d-%d", &year, &month, &day) == 3) {
			struct mailimap_date *date;
			struct mailimap_search_key *k;
			date = mailimap_date_new (day, month, year);
			k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_BEFORE,
				NULL,                            /* bcc */
				date,                            /* before */
				NULL, NULL, NULL, NULL, NULL,    /* body..on */
				NULL, NULL, NULL, NULL,          /* since..to */
				NULL, NULL, NULL, 0,             /* unkeyword..larger */
				NULL, NULL, NULL,                /* not, or1, or2 */
				NULL, NULL, NULL, 0,             /* sentbefore..smaller */
				NULL, NULL, NULL);               /* uid, set, multiple */
			clist_append (key_list, k);
		}
	}

	if (unread) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_UNSEEN,
			NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			NULL, NULL, NULL, 0, NULL, NULL, NULL);
		clist_append (key_list, k);
	}

	if (flagged) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_FLAGGED,
			NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			NULL, NULL, NULL, 0, NULL, NULL, NULL);
		clist_append (key_list, k);
	}

	if (clist_isempty (key_list)) {
		struct mailimap_search_key *k;
		k = mailimap_search_key_new (MAILIMAP_SEARCH_KEY_ALL,
			NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
			NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL,
			NULL, NULL, NULL, 0, NULL, NULL, NULL);
		clist_append (key_list, k);
	}

	if (clist_count (key_list) == 1) {
		compound_key = (struct mailimap_search_key *) clist_content (clist_begin (key_list));
		clist_free (key_list);
		key_list = NULL;
	} else {
		compound_key = mailimap_search_key_new_multiple (key_list);
		key_list = NULL;
	}

	r = mailimap_uid_search (imap, NULL, compound_key, &search_result);
	mailimap_search_key_free (compound_key);

	if (r != MAILIMAP_NO_ERROR) {
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("IMAP search failed", TRUE);
	}

	count = 0;
	uid_set = mailimap_set_new_empty ();
	for (sit = clist_begin (search_result); sit; sit = clist_next (sit)) {
		uint32_t *uid_p = (uint32_t *) clist_content (sit);
		mailimap_set_add_single (uid_set, *uid_p);
		count++;
		if ((gint64) count >= limit)
			break;
	}
	mailimap_search_result_free (search_result);

	if (count == 0) {
		mailimap_set_free (uid_set);
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("[]", FALSE);
	}

	fetch_type = mailimap_fetch_type_new_fetch_att_list_empty ();
	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_ENVELOPE, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);
	fetch_att = mailimap_fetch_att_new (MAILIMAP_FETCH_ATT_UID, NULL, 0, 0, NULL);
	mailimap_fetch_type_new_fetch_att_list_add (fetch_type, fetch_att);

	r = mailimap_uid_fetch (imap, uid_set, fetch_type, &fetch_result);
	mailimap_set_free (uid_set);

	if (r != MAILIMAP_NO_ERROR) {
		mailimap_logout (imap);
		mailimap_free (imap);
		return _mcp_text_result ("Fetch failed", TRUE);
	}

	jb = json_builder_new ();
	json_builder_begin_array (jb);

	for (fit = clist_begin (fetch_result); fit; fit = clist_next (fit)) {
		struct mailimap_msg_att *msg_att;
		clistiter *ait;
		struct mailimap_envelope *env = NULL;
		uint32_t uid = 0;

		msg_att = (struct mailimap_msg_att *) clist_content (fit);
		for (ait = clist_begin (msg_att->att_list); ait; ait = clist_next (ait)) {
			struct mailimap_msg_att_item *item;
			item = (struct mailimap_msg_att_item *) clist_content (ait);
			if (item->att_type == MAILIMAP_MSG_ATT_ITEM_STATIC) {
				struct mailimap_msg_att_static *s = item->att_data.att_static;
				if (s->att_type == MAILIMAP_MSG_ATT_ENVELOPE)
					env = s->att_data.att_env;
				else if (s->att_type == MAILIMAP_MSG_ATT_UID)
					uid = s->att_data.att_uid;
			}
		}

		if (!env) continue;

		/* Apply from-whitelist filter for MCP */
		if (mctx->from_whitelist) {
			gchar *sender_email = get_first_from_email (env);
			gboolean allowed = is_whitelisted (sender_email, mctx->from_whitelist);
			g_free (sender_email);
			if (!allowed)
				continue;
		}

		json_builder_begin_object (jb);
		json_builder_set_member_name (jb, "uid");
		json_builder_add_int_value (jb, uid);
		json_builder_set_member_name (jb, "date");
		json_builder_add_string_value (jb, env->env_date ? env->env_date : "");
		json_builder_set_member_name (jb, "from");
		json_builder_add_string_value (jb, get_first_from_addr (env));
		json_builder_set_member_name (jb, "subject");
		json_builder_add_string_value (jb, env->env_subject ? env->env_subject : "");
		json_builder_end_object (jb);
	}

	json_builder_end_array (jb);
	jnode = json_builder_get_root (jb);
	gen = json_generator_new ();
	json_generator_set_root (gen, jnode);
	json_out = json_generator_to_data (gen, NULL);
	json_node_unref (jnode);
	g_object_unref (gen);
	g_object_unref (jb);

	mailimap_fetch_list_free (fetch_result);
	mailimap_logout (imap);
	mailimap_free (imap);

	{
		McpToolResult *res = _mcp_text_result (json_out, FALSE);
		g_free (json_out);
		return res;
	}
}

static McpToolResult *
mcp_handle_tool_call (McpServer *server, const gchar *name,
					  JsonObject *args, gpointer user_data)
{
	McpContext *mctx = (McpContext *) user_data;
	JsonObject *_empty_args = NULL;

	(void) server;

	if (!args) {
		_empty_args = json_object_new ();
		args = _empty_args;
	}

	if (g_strcmp0 (name, "email_send") == 0) {
		McpToolResult *r = mcp_handle_send (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_read") == 0) {
		McpToolResult *r = mcp_handle_read (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_rm") == 0) {
		McpToolResult *r = mcp_handle_rm (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_move") == 0) {
		McpToolResult *r = mcp_handle_move (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_folders") == 0) {
		McpToolResult *r = mcp_handle_folders (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_count") == 0) {
		McpToolResult *r = mcp_handle_count (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_flag") == 0) {
		McpToolResult *r = mcp_handle_flag (mctx, args);
		if (_empty_args) json_object_unref (_empty_args);
		return r;
	}
	if (g_strcmp0 (name, "email_search") == 0) {
		McpToolResult *r = mcp_handle_search (mctx, args);
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
cmd_mcp (EmailConfig *cfg, int argc, char **argv)
{
	gchar *to_whitelist_csv = NULL;
	gchar *from_whitelist_csv = NULL;

	GOptionEntry entries[] = {
		{ "to-whitelist",   0, 0, G_OPTION_ARG_STRING, &to_whitelist_csv,
		  "CSV of allowed recipient addresses", "ADDRS" },
		{ "from-whitelist", 0, 0, G_OPTION_ARG_STRING, &from_whitelist_csv,
		  "CSV of allowed sender addresses",    "ADDRS" },
		{ NULL }
	};

	GOptionContext *ctx;
	GError *error = NULL;
	McpContext mctx;
	GMainLoop *loop;
	McpServer *server;
	McpStdioTransport *transport;
	JsonParser *tools_parser;
	JsonArray *tools_arr;
	guint i;

	ctx = g_option_context_new ("- run as MCP server");
	g_option_context_add_main_entries (ctx, entries, NULL);
	if (!g_option_context_parse (ctx, &argc, &argv, &error)) {
		g_printerr ("mcp: %s\n", error->message);
		g_error_free (error);
		g_option_context_free (ctx);
		return 1;
	}
	g_option_context_free (ctx);

	mctx.cfg = cfg;
	mctx.to_whitelist = parse_csv_whitelist (to_whitelist_csv);
	mctx.from_whitelist = parse_csv_whitelist (from_whitelist_csv);

	loop = g_main_loop_new (NULL, FALSE);
	server = mcp_server_new ("email", "2.0.0");

	{
		GString *instructions = g_string_new (
			"Email client for sending, reading, searching, and managing emails via IMAP/SMTP.");
		if (mctx.to_whitelist) {
			g_string_append (instructions,
				" SEND RESTRICTION: You may only send to these addresses: ");
			g_string_append (instructions, to_whitelist_csv);
			g_string_append_c (instructions, '.');
		}
		if (mctx.from_whitelist) {
			g_string_append (instructions,
				" READ RESTRICTION: Only messages from these senders will be returned: ");
			g_string_append (instructions, from_whitelist_csv);
			g_string_append_c (instructions, '.');
		}
		mcp_server_set_instructions (server, instructions->str);
		g_string_free (instructions, TRUE);
	}

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
			mcp_server_add_tool (server, tool, mcp_handle_tool_call, &mctx, NULL);
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
	g_strfreev (mctx.to_whitelist);
	g_strfreev (mctx.from_whitelist);
	g_free (to_whitelist_csv);
	g_free (from_whitelist_csv);

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
	g_print ("  send      Send an email\n");
	g_print ("  read      Read emails from IMAP\n");
	g_print ("  rm        Delete emails by UID\n");
	g_print ("  folders   List IMAP folders\n");
	g_print ("  move      Move messages between folders\n");
	g_print ("  count     Show message counts\n");
	g_print ("  search    Search messages by criteria\n");
	g_print ("  flag      Add/remove message flags\n");
	g_print ("  mcp       Run as MCP server (stdio)\n");
	g_print ("\nUse '%s <command> --help' for command-specific options.\n", prog);
	g_print ("Use '%s --license' for license information.\n", prog);
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
	if (g_str_equal (cmd, "--license")) {
		g_print ("%s", AGPLV3_NOTICE);
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

	/* Allow --help without a config file (except mcp, which has its own args) */
	if (!g_str_equal (cmd, "mcp")) {
		for (i = 1; i < argc; i++) {
			if (g_str_equal (argv[i], "--help") || g_str_equal (argv[i], "-h")) {
				cfg = g_new0 (EmailConfig, 1);
				if (g_str_equal (cmd, "send"))
					ret = cmd_send (cfg, argc, argv);
				else if (g_str_equal (cmd, "read"))
					ret = cmd_read (cfg, argc, argv);
				else if (g_str_equal (cmd, "rm"))
					ret = cmd_rm (cfg, argc, argv);
				else if (g_str_equal (cmd, "folders"))
					ret = cmd_folders (cfg, argc, argv);
				else if (g_str_equal (cmd, "move"))
					ret = cmd_move (cfg, argc, argv);
				else if (g_str_equal (cmd, "count"))
					ret = cmd_count (cfg, argc, argv);
				else if (g_str_equal (cmd, "search"))
					ret = cmd_search (cfg, argc, argv);
				else if (g_str_equal (cmd, "flag"))
					ret = cmd_flag (cfg, argc, argv);
				else
					print_usage (argv[0]);
				free_config (cfg);
				return ret;
			}
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
	} else if (g_str_equal (cmd, "folders")) {
		ret = cmd_folders (cfg, argc, argv);
	} else if (g_str_equal (cmd, "move")) {
		ret = cmd_move (cfg, argc, argv);
	} else if (g_str_equal (cmd, "count")) {
		ret = cmd_count (cfg, argc, argv);
	} else if (g_str_equal (cmd, "search")) {
		ret = cmd_search (cfg, argc, argv);
	} else if (g_str_equal (cmd, "flag")) {
		ret = cmd_flag (cfg, argc, argv);
	} else if (g_str_equal (cmd, "mcp")) {
		ret = cmd_mcp (cfg, argc, argv);
	} else {
		g_printerr ("Unknown command: %s\n", cmd);
		print_usage (argv[0]);
		ret = 1;
	}

	free_config (cfg);
	return ret;
}
