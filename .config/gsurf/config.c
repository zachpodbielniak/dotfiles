/*
 * gsurf - personal C configuration ported from sensible-config.yaml
 *
 * Copyright (C) 2026 Zach Podbielniak
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * The C-config equivalent of data/sensible-config.yaml: a curated,
 * privacy-leaning daily-driver setup. Install as ~/.config/gsurf/config.c
 * (gsurf compiles it via crispy with content-hash caching) and it overrides
 * the YAML config.
 *
 * What is set where, and why:
 *   - Core scalar settings (browser, web engine, TLS, storage, network,
 *     window) are plain struct assignments — the natural C path.
 *   - Key/mouse bindings use gsurf_config_set_keybind()/_set_mousebind(),
 *     which normalize the binding string exactly like the YAML loader.
 *   - Module options are read by each module from a YAML `modules:` mapping
 *     (gsurf_config_get_module_node()); there is no typed C struct for them,
 *     so the module block is layered in via gsurf_config_load_from_data().
 *     That is a normal, supported technique: a C config may call the library
 *     loader to populate the parts that live in the YAML mapping.
 */

/* Installed headers need their root on the include path; the image's
 * compiled-in development path points at its former build directory. */
#define CRISPY_PARAMS "-I/usr/include/gsurf"

#include <gsurf/gsurf.h>

/* The module section, identical to the `modules:` block of
 * data/sensible-config.yaml. Layered over the active config below. */
static const char *SENSIBLE_MODULES_YAML =
"modules:\n"
"  modal:\n"
"    enabled: true\n"
"    start_mode: normal\n"
"    scroll_step: 60\n"
"    hint_key: \"f\"\n"
"    hint_key_newview: \"F\"\n"
"    hint_chars: \"asdfghjkl\"\n"
"    hint_bg: \"#ffd700\"\n"
"    hint_fg: \"#000000\"\n"
"    hint_font_size: 24\n" /* 18pt = 24 CSS pixels. */
"  chromebar:\n"
"    enabled: true\n"
"    key_focus: \"Ctrl+l\"\n"
"  search_engines:\n"
"    enabled: true\n"
"    default: lr\n"
"    default_is_search: true\n"
"    engines:\n"
"      lr:     { prefix: \"l \",  url: \"http://127.0.0.1:9000/search?q=%s&category=web\" }\n"
"      ddg:    { prefix: \"d \",  url: \"https://duckduckgo.com/?q=%s\" }\n"
"      google: { prefix: \"g \",  url: \"https://www.google.com/search?q=%s\" }\n"
"      wiki:   { prefix: \"w \",  url: \"https://en.wikipedia.org/w/index.php?search=%s\" }\n"
"      gh:     { prefix: \"gh \", url: \"https://github.com/search?q=%s\" }\n"
"  tabs:\n"
"    enabled: true\n"
"    key_new: \"Ctrl+t\"\n"
"    key_close: \"Ctrl+w\"\n"
"    key_next: \"Ctrl+Tab\"\n"
"    key_prev: \"Ctrl+Shift+Tab\"\n"
"    strip: false\n" /* No top strip: tabs live in the Ctrl+Shift+. sidebar. */
/*
 * Containers, mirroring the LibreWolf profile:
 *   - Temporary Containers (automatic mode on new tabs, gray, removed
 *     15 minutes after the last tab closes) -> new_tab: temporary,
 *     temp_color, temp_cleanup_delay: 900;
 *   - Sidebery's per-container "reopen rules" (URL substrings) -> hosts
 *     regexes. gsurf matches the bare host, so a word that would also
 *     hit unrelated hosts ("chase" in "purchase", "citi" in "cities",
 *     "amazon" in "amazonaws") is anchored to its domain; brand and
 *     internal-host fragments stay substrings, as in Sidebery.
 * Colours are Firefox's container colours.
 */
"  containers:\n"
"    enabled: true\n"
"    new_tab: temporary\n"
"    inherit_opener: true\n"
"    temp_cleanup_delay: 900\n"
"    temp_color: \"#7c7c7d\"\n"
"    containers:\n"
"      - name: Personal\n"
"        color: \"#37adff\"\n"
"        hosts:\n"
"          - '(^|\\.)gmail\\.com$'\n"
"          - '(^|\\.)claude\\.(ai|com)$'\n"
"          - '(^|\\.)grok\\.com$'\n"
"          - '(^|\\.)perplexity\\.ai$'\n"
"          - '(^|\\.)proton\\.me$'\n"
"          - '(^|\\.)protonmail\\.com$'\n"
"      - name: Work\n"
"        color: \"#ff9f00\"\n"
"        hosts:\n"
"          - 'dynatrace'\n"
"          - 'dt-rnd'\n"
"          - 'dt-planning'\n"
"          - '(^|\\.)office\\.com$'\n"
"      - name: Banking\n"
"        color: \"#51cd00\"\n"
"        hosts:\n"
"          - 'fidelity'\n"
"          - 'americanexpress'\n"
"          - '(^|\\.)chase\\.com$'\n"
"          - '(^|\\.)citi(bank|cards)?\\.com$'\n"
"          - 'schwab'\n"
"          - 'bankofamerica'\n"
"      - name: Shopping\n"
"        color: \"#ff4bda\"\n"
"        hosts:\n"
"          - '(^|\\.)amazon\\.com$'\n"
"          - '(^|\\.)walmart\\.com$'\n"
"          - '(^|\\.)midwayusa\\.com$'\n"
"          - '(^|\\.)ebay\\.com$'\n"
"          - 'pipesandcigars'\n"
"      - name: Media\n"
"        color: \"#af51f5\"\n"
"        hosts:\n"
"          - '(^|\\.)youtube\\.com$'\n"
"          - '(^|\\.)youtu\\.be$'\n"
"          - '(^|\\.)facebook\\.com$'\n"
/*
 * wmux: tmux-style panes (docs/wmux.org). tmux defaults: prefix C-b,
 * % / " split (C-% / C-" duplicate the page), C-h/j/k/l move between
 * panes (so C-l no longer focuses the address bar; use o), q numbers,
 * z zoom, c/n/p windows, : command prompt. Pane borders take the tab's
 * container colour; the active pane is brighter and wider.
 * Every key is listed at its default so it is easy to tweak. Extras on
 * top of tmux: | and - split, H/J/K/L resize by 5 (repeatable), g opens
 * GitHub in a split, M-d main-vertical layout. To keep C-l for the
 * address bar, set root_bindings: {C-l: ""} (see the docs cookbook).
 */
"  wmux:\n"
"    enabled: true\n"
"    prefix: \"C-b\"\n"
"    prefix_timeout: 0\n"
"    repeat_time: 500\n"
"    display_panes_time: 1000\n"
"    split_ratio: 50\n"
"    min_pane_pct: 10\n"
"    on_pane_kill: close_tab\n"
"    on_tab_close: close_pane\n"
"    confirm_kill: true\n"
"    mouse: true\n"
"    remember_layout: true\n"
"    gap: 2\n"
"    border_width: 1\n"
"    active_border_width: 2\n"
"    border_color: \"#45475a\"\n"
"    active_border_color: \"#89b4fa\"\n"
"    container_colors: true\n"
"    inactive_opacity: 0.55\n"
"    active_brighten: 0.25\n"
"    status: true\n"
"    status_position: bottom\n"
"    status_bg: \"#181825\"\n"
"    status_fg: \"#cdd6f4\"\n"
"    status_current_fg: \"#89b4fa\"\n"
"    window_name_max: 20\n"
"    bindings:\n"
"      '|': split-window -h\n"
"      '-': split-window -v\n"
"      H: resize-pane -L 5\n"
"      J: resize-pane -D 5\n"
"      K: resize-pane -U 5\n"
"      L: resize-pane -R 5\n"
"      g: split-window -h https://github.com\n"
"      M-d: select-layout main-vertical\n"
"    repeatable: [H, J, K, L]\n"
"    root_bindings: {}\n"
"  history:\n"
"    enabled: true\n"
"    file: \"~/.local/share/gsurf/history\"\n"
"    log_titles: true\n"
"  homepage:\n"
"    enabled: true\n"
"    uri: \"http://127.0.0.1:9000/\"\n"
"    key_home: \"Ctrl+Home\"\n"
"  toggles:\n"
"    enabled: true\n"
"    notify_on_toggle: true\n"
"    keys:\n"
"      javascript: \"Ctrl+Shift+s\"\n"
"      images: \"Ctrl+Shift+m\"\n"
"      webgl: \"Ctrl+Shift+w\"\n"
"      caret_browsing: \"Ctrl+i\"\n"
"      developer_extras: \"Ctrl+Shift+d\"\n"
"  find_bar:\n"
"    enabled: true\n"
"    key_next: \"n\"\n"
"    key_prev: \"N\"\n"
"  cookie_policy:\n"
"    enabled: true\n"
"    policy: no-third-party\n"
"    key_toggle: \"Ctrl+Shift+at\"\n"
"  adblock:\n"
"    enabled: true\n"
"    host_files:\n"
"      - \"~/.config/gsurf/adblock/hosts\"\n"
"    whitelist: []\n"
"  downloads:\n"
"    enabled: true\n"
"    dir: \"~/Downloads\"\n"
"  bookmarks:\n"
"    enabled: true\n"
"    file: \"~/.local/share/gsurf/bookmarks\"\n"
"    dmenu_cmd: \"dmenu -l 20 -i\"\n"
"    key_add: \"Ctrl+d\"\n"
"    key_open: \"Ctrl+Shift+d\"\n"
"  playexternal:\n"
"    enabled: true\n"
"    command: \"mpv --\"\n"
"    key: \"Ctrl+m\"\n"
"  inspector:\n"
"    enabled: true\n"
"    key: \"Ctrl+Shift+i\"\n"
"  dark_mode:\n"
"    enabled: true\n"
"    mode: auto\n"
"    force_dark_css: false\n"
"  status_bar:\n"
"    enabled: true\n"
"  geolocation:\n"
"    enabled: true\n"
"    default: deny\n"
"  notifications:\n"
"    enabled: true\n"
"    allow_web_notifications: ask\n";

G_MODULE_EXPORT gboolean
gsurf_config_init(void)
{
	GsurfConfig *config = gsurf_config_get_default();
	GsurfSettings *s;

	if (config == NULL)
		return FALSE;
	s = config->settings;

	/* ===== Browser / general ===== */
	g_free(config->homepage);
	config->homepage = g_strdup("http://127.0.0.1:9000/");
	g_free(config->new_view_uri);
	config->new_view_uri = g_strdup("http://127.0.0.1:9000/");
	config->default_zoom = 1.0;
	config->smooth_scroll = TRUE;
	config->fullscreen_on_start = FALSE;

	/* Empty UA = engine default (the useragent module can override per-site). */
	g_free(config->user_agent);
	config->user_agent = g_strdup("");
	gsurf_settings_set_user_agent(s, "");

	/* ===== Web engine defaults (privacy-leaning) ===== */
	s->javascript = TRUE;
	s->images = TRUE;
	s->webgl = FALSE;                 /* fingerprinting / GPU surface */
	s->webaudio = TRUE;
	s->media = TRUE;
	s->media_stream = FALSE;          /* camera/mic off until trusted */
	s->webrtc = FALSE;                /* avoid local-IP leakage */
	s->plugins = FALSE;
	s->smooth_scrolling = TRUE;
	s->caret_browsing = FALSE;
	s->dns_prefetch = FALSE;
	s->hyperlink_auditing = FALSE;    /* drop <a ping> beacons */
	s->site_quirks = TRUE;
	s->developer_extras = FALSE;      /* inspector module flips on demand */
	s->js_can_open_windows = FALSE;
	s->js_can_access_clipboard = FALSE;
	s->default_font_size = 16;
	s->default_monospace_font_size = 13;
	gsurf_settings_set_default_charset(s, "UTF-8");

	/* ===== TLS / storage / network ===== */
	config->tls_strict = TRUE;

	g_free(config->cache_dir);
	config->cache_dir = g_build_filename(g_get_user_cache_dir(), "gsurf", NULL);
	g_free(config->data_dir);
	config->data_dir = g_build_filename(g_get_user_data_dir(), "gsurf", NULL);
	g_free(config->cookie_jar);
	config->cookie_jar = g_build_filename(g_get_user_data_dir(), "gsurf", "cookies.sqlite", NULL);
	config->enable_disk_cache = TRUE;

	g_free(config->proxy_mode);
	config->proxy_mode = g_strdup("default");      /* default | none | custom */
	g_free(config->proxy_uri);
	config->proxy_uri = g_strdup("");
	config->do_not_track = TRUE;

	/* ===== Window ===== */
	g_free(config->window_title);
	config->window_title = g_strdup("gsurf");
	config->window_width = 1280;
	config->window_height = 800;

	/* ===== Core keybinds (vim letters come from the `modal` module) ===== */
	gsurf_config_set_keybind(config, "Ctrl+r",       GSURF_ACTION_RELOAD);
	gsurf_config_set_keybind(config, "Ctrl+Shift+r", GSURF_ACTION_RELOAD_NOCACHE);
	gsurf_config_set_keybind(config, "Escape",       GSURF_ACTION_STOP);
	gsurf_config_set_keybind(config, "Alt+Left",     GSURF_ACTION_BACK);
	gsurf_config_set_keybind(config, "Alt+Right",    GSURF_ACTION_FORWARD);
	gsurf_config_set_keybind(config, "Ctrl+plus",    GSURF_ACTION_ZOOM_IN);
	gsurf_config_set_keybind(config, "Ctrl+minus",   GSURF_ACTION_ZOOM_OUT);
	gsurf_config_set_keybind(config, "Ctrl+0",       GSURF_ACTION_ZOOM_RESET);
	gsurf_config_set_keybind(config, "F11",          GSURF_ACTION_TOGGLE_FULLSCREEN);
	gsurf_config_set_keybind(config, "Ctrl+q",       GSURF_ACTION_QUIT);

	/* ===== Mousebinds ===== */
	gsurf_config_set_mousebind(config, "Button8",      GSURF_ACTION_BACK);
	gsurf_config_set_mousebind(config, "Button9",      GSURF_ACTION_FORWARD);
	gsurf_config_set_mousebind(config, "Button2",      GSURF_ACTION_OPEN_NEW_VIEW_BACKGROUND);
	gsurf_config_set_mousebind(config, "Ctrl+Button1", GSURF_ACTION_OPEN_NEW_VIEW_BACKGROUND);

	/* ===== Modules ===== (read from a YAML mapping; layered via the loader) */
	/* Propagate module-config failures instead of accepting a partial config. */
	return gsurf_config_load_from_data(config, SENSIBLE_MODULES_YAML, -1, NULL);
}
