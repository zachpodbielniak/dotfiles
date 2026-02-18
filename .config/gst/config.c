/*
 * GST - C Configuration
 * ~/.config/gst/config.c
 *
 * dotfiles - Personal configuration files and scripts
 * Copyright (C) 2026  Zach Podbielniak
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <gst/gst.h>

G_MODULE_EXPORT gboolean
gst_config_init(void)
{
	GstConfig *config;

	/* Catppuccin Mocha palette */
	static const gchar *palette[] = {
		"#45475a",  /* surface1  (black)        */
		"#f38ba8",  /* red                      */
		"#a6e3a1",  /* green                    */
		"#f9e2af",  /* yellow                   */
		"#89b4fa",  /* blue                     */
		"#f5c2e7",  /* pink      (magenta)      */
		"#94e2d5",  /* teal      (cyan)         */
		"#bac2de",  /* subtext1  (white)        */
		"#585b70",  /* surface2  (bright black) */
		"#f38ba8",  /* red                      */
		"#a6e3a1",  /* green                    */
		"#f9e2af",  /* yellow                   */
		"#89b4fa",  /* blue                     */
		"#f5c2e7",  /* pink                     */
		"#94e2d5",  /* teal                     */
		"#a6adc8",  /* subtext0  (bright white) */
		NULL
	};

	static const gchar *fallback_fonts[] = {
		"Noto Color Emoji:pixelsize=18",
		NULL
	};

	config = gst_config_get_default();

	/* --- Terminal --- */
	gst_config_set_shell(config, "/bin/bash");
	gst_config_set_term_name(config, "st-256color");
	gst_config_set_tabspaces(config, 8);

	/* --- Window --- */
	gst_config_set_title(config, "gst");
	gst_config_set_cols(config, 80);
	gst_config_set_rows(config, 24);
	gst_config_set_border_px(config, 2);

	/* --- Font --- */
	gst_config_set_font_primary(config, "Hack Nerd Font Mono:pixelsize=18");
	gst_config_set_font_fallbacks(config, fallback_fonts);

	/* --- Colors: Catppuccin Mocha --- */
	gst_config_set_fg_hex(config, "#cdd6f4");       /* text      */
	gst_config_set_bg_hex(config, "#1e1e2e");       /* base      */
	gst_config_set_cursor_fg_hex(config, "#1e1e2e"); /* base      */
	gst_config_set_cursor_bg_hex(config, "#f5e0dc"); /* rosewater */
	gst_config_set_palette_hex(config, palette, 16);

	/* --- Cursor --- */
	gst_config_set_cursor_shape(config, GST_CURSOR_SHAPE_BLOCK);
	gst_config_set_cursor_blink(config, FALSE);
	gst_config_set_blink_rate(config, 500);

	/* --- Selection --- */
	gst_config_set_word_delimiters(config, " `'\"()[]{}|");

	/* --- Keybinds (replace all defaults) --- */
	gst_config_clear_keybinds(config);
	gst_config_add_keybind(config, "Ctrl+Shift+c", "clipboard_copy");
	gst_config_add_keybind(config, "Ctrl+Shift+v", "clipboard_paste");
	gst_config_add_keybind(config, "Shift+Page_Up", "scroll_up");
	gst_config_add_keybind(config, "Shift+Page_Down", "scroll_down");
	gst_config_add_keybind(config, "Ctrl+Shift+Page_Up", "scroll_top");
	gst_config_add_keybind(config, "Ctrl+Shift+Page_Down", "scroll_bottom");
	gst_config_add_keybind(config, "Ctrl+Shift+Home", "scroll_top");
	gst_config_add_keybind(config, "Ctrl+Shift+End", "scroll_bottom");
	gst_config_add_keybind(config, "Ctrl+Shift+plus", "zoom_in");
	gst_config_add_keybind(config, "Ctrl+Shift+minus", "zoom_out");
	gst_config_add_keybind(config, "Ctrl+Shift+0", "zoom_reset");

	/* --- Mousebinds (replace all defaults) --- */
	gst_config_clear_mousebinds(config);
	gst_config_add_mousebind(config, "Button4", "scroll_up");
	gst_config_add_mousebind(config, "Button5", "scroll_down");
	gst_config_add_mousebind(config, "Shift+Button4", "scroll_up_fast");
	gst_config_add_mousebind(config, "Shift+Button5", "scroll_down_fast");

	/* --- Modules --- */

	/* scrollback */
	gst_config_set_module_config_bool(config, "scrollback", "enabled", TRUE);
	gst_config_set_module_config_int(config, "scrollback", "lines", 10000);
	gst_config_set_module_config_int(config, "scrollback", "mouse_scroll_lines", 3);

	/* transparency */
	gst_config_set_module_config_bool(config, "transparency", "enabled", TRUE);
	gst_config_set_module_config_double(config, "transparency", "opacity", 0.9);

	/* urlclick */
	gst_config_set_module_config_bool(config, "urlclick", "enabled", TRUE);
	gst_config_set_module_config_string(config, "urlclick", "opener", "xdg-open");
	gst_config_set_module_config_string(config, "urlclick", "regex", "(https?|ftp|file)://[\\w\\-_.~:/?#\\[\\]@!$&'()*+,;=%]+");
	gst_config_set_module_config_string(config, "urlclick", "modifiers", "Ctrl");

	/* externalpipe */
	gst_config_set_module_config_bool(config, "externalpipe", "enabled", FALSE);
	gst_config_set_module_config_string(config, "externalpipe", "command", "");
	gst_config_set_module_config_string(config, "externalpipe", "key", "Ctrl+Shift+e");

	/* boxdraw */
	gst_config_set_module_config_bool(config, "boxdraw", "enabled", TRUE);
	gst_config_set_module_config_int(config, "boxdraw", "bold_offset", 1);

	/* visualbell */
	gst_config_set_module_config_bool(config, "visualbell", "enabled", FALSE);
	gst_config_set_module_config_int(config, "visualbell", "duration", 100);

	/* undercurl */
	gst_config_set_module_config_bool(config, "undercurl", "enabled", TRUE);

	/* clipboard */
	gst_config_set_module_config_bool(config, "clipboard", "enabled", TRUE);

	/* font2 */
	gst_config_set_module_config_bool(config, "font2", "enabled", TRUE);
	{
		static const gchar *font2_fonts[] = {
			"NotoColorEmoji:pixelsize=20:antialias=true:autohint=true",
			NULL
		};
		gst_config_set_module_config_strv(config, "font2", "fonts", font2_fonts);
	}

	/* keyboard_select */
	gst_config_set_module_config_bool(config, "keyboard_select", "enabled", FALSE);
	gst_config_set_module_config_string(config, "keyboard_select", "key", "Ctrl+Shift+Escape");
	gst_config_set_module_config_bool(config, "keyboard_select", "show_crosshair", TRUE);
	gst_config_set_module_config_string(config, "keyboard_select", "highlight_color", "#ff8800");
	gst_config_set_module_config_int(config, "keyboard_select", "highlight_alpha", 100);
	gst_config_set_module_config_string(config, "keyboard_select", "search_color", "#ffff00");
	gst_config_set_module_config_int(config, "keyboard_select", "search_alpha", 150);

	/* kittygfx */
	gst_config_set_module_config_bool(config, "kittygfx", "enabled", FALSE);
	gst_config_set_module_config_int(config, "kittygfx", "max_total_ram_mb", 256);
	gst_config_set_module_config_int(config, "kittygfx", "max_single_image_mb", 64);
	gst_config_set_module_config_int(config, "kittygfx", "max_placements", 4096);
	gst_config_set_module_config_bool(config, "kittygfx", "allow_file_transfer", FALSE);
	gst_config_set_module_config_bool(config, "kittygfx", "allow_shm_transfer", FALSE);

	/* mcp */
	gst_config_set_module_config_bool(config, "mcp", "enabled", TRUE);
	gst_config_set_module_config_string(config, "mcp", "transport", "unix-socket");

	/* mcp tools */
	gst_config_set_module_config_sub_bool(config, "mcp","tools", "read_screen", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "read_scrollback", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "search_scrollback", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_cursor_position", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_cell_attributes", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_foreground_process", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_working_directory", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "is_shell_idle", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_pty_info", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "list_detected_urls", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_config", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "list_modules", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "set_config", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "toggle_module", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "get_window_info", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "set_window_title", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "send_text", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "send_keys", TRUE);
	gst_config_set_module_config_sub_bool(config, "mcp", "tools", "screenshot", TRUE);

	return TRUE;
}
