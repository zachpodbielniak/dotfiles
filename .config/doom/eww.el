;;; eww.el -*- lexical-binding: t; -*-
;;
;; eww.el - Emacs as a browser: eww + shr readability + URL routing
;; Copyright (C) 2026  Zach Podbielniak
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Browser config modeled on <https://joshblais.com/blog/emacs-as-my-browser/>
;; but fit to this config's workflow (Doom + Evil + catppuccin + mpv).
;;
;; Moving parts:
;;   - eww + shr: readability-oriented defaults, DuckDuckGo HTML search,
;;     downloads land in ~/Downloads/.
;;   - URL-handler routing: YouTube/Vimeo/Twitch and common video
;;     extensions -> mpv; PDFs -> pdf-tools (when available); everything
;;     else -> eww.  Any `browse-url' call in the config picks the right
;;     handler automatically.
;;   - Fallback "real browser" for JS-heavy pages: xdg-open, so the user's
;;     system default browser handles it (no hardcoded binary name).
;;   - Evil normal-state bindings inside eww-mode: zoom, readable mode,
;;     external-browser handoff, image download, bookmarks, reload, `?'
;;     help popup.
;;
;; Entry point: `zach/eww-here' opens eww in the current window
;; (not a popup); bound to SPC o B in config.el alongside SPC o T.

;;; Code:

(require 'eww)
(require 'shr)
(require 'browse-url)

;;; ---------------------------------------------------------------------
;;; Settings

(setq eww-search-prefix "https://duckduckgo.com/html/?q=")
(setq eww-download-directory (expand-file-name "~/Downloads/"))

(setq shr-width           100
      shr-max-width       120
      shr-indentation     4
      shr-use-fonts       nil
      shr-max-image-size  '(800 . 600)
      shr-image-animate   t)

;; External-browser handoff for `&' / `go' / `S-RET' in eww.
;;
;; This is the third rewrite of this block and the subtle bug made the
;; first two look broken even though they "set the right variable":
;;
;;   * `eww-browse-with-external-browser' does NOT just `funcall' the
;;     secondary browser function.  It checks that function's
;;     `browse-url-browser-kind' symbol property; if the property isn't
;;     `external', it falls through to `browse-url-with-browser-kind'
;;     which picks a different function entirely (often ending up
;;     inside `browse-url-handlers', which below is a catch-all that
;;     routes back to `eww-browse-url' — rendered as a page "reload").
;;
;;   * Even with the property set, the if-branch still calls
;;     `(browse-url url)' under a let-binding, and `browse-url'
;;     consults `browse-url-handlers' before the browser function.
;;     Our catch-all handler (which we need so in-eww link clicks
;;     stay in eww) catches the URL first and loops it back into eww.
;;
;; Fix: don't use `eww-browse-with-external-browser' at all.  Rebind
;; its key(s) to `zach/eww-open-in-external-browser', which launches
;; the browser directly via `zach/browse-url-external' — bypassing
;; browse-url's handler/kind dispatch entirely.  We still annotate
;; the handler functions with correct `browse-url-browser-kind' for
;; anything else that introspects them.
;;
;; Preference order for the external browser, first installed wins:
;;   1. Junction  (flatpak, re.sonny.Junction) — interactive per-URL
;;      chooser, the user's preferred default when installed.
;;   2. LibreWolf (flatpak, io.gitlab.librewolf-community).
;;   3. Firefox   (flatpak, org.mozilla.firefox).
;;   4. Firefox   (binary).
;;   5. Chromium  (binary).
(defun zach/eww--detect-external-browser ()
  "Detect the preferred external browser launcher.
Returns an argv list or nil.  See preference order in eww.el."
  (let ((flatpaks
         (when (executable-find "flatpak")
           (ignore-errors
             (split-string
              (shell-command-to-string
               "flatpak list --app --columns=application 2>/dev/null")
              "\n" t)))))
    (cond
     ((member "re.sonny.Junction" flatpaks)
      (list "flatpak" "run" "re.sonny.Junction"))
     ((member "io.gitlab.librewolf-community" flatpaks)
      (list "flatpak" "run" "io.gitlab.librewolf-community"))
     ((member "org.mozilla.firefox" flatpaks)
      (list "flatpak" "run" "org.mozilla.firefox"))
     ((executable-find "firefox")  (list "firefox"))
     ((executable-find "chromium") (list "chromium"))
     (t nil))))

(defvar zach/eww-external-browser-argv nil
  "Argv list used by `zach/browse-url-external' to launch a URL.
Nil means no external browser was detected.  Re-detected on every
load of this file via the `setq' below; override by `setq'ing to a
custom list before or after this module loads.")

;; `setq', not `defvar'-init: a `defvar' with an init form only runs
;; the init if the symbol is unbound, so reloading this file after a
;; bad first load wouldn't re-detect.  `setq' always runs.
(setq zach/eww-external-browser-argv (zach/eww--detect-external-browser))

(defun zach/browse-url-external (url &rest _args)
  "Launch URL in the preferred external browser.
See `zach/eww-external-browser-argv' for resolution order.  Logs the
exact argv via `message' so it's visible in *Messages* if the spawn
ever misbehaves."
  (let ((argv (or zach/eww-external-browser-argv
                  (zach/eww--detect-external-browser))))
    (unless argv
      (user-error "No external browser found; set `zach/eww-external-browser-argv'"))
    (message "[eww] launching: %s %s"
             (mapconcat #'identity argv " ") url)
    (apply #'start-process "zach-eww-browser" nil
           (append argv (list url)))))

;; Tell any Emacs code that introspects these functions' browser kind
;; the right answer.  See browse-url.el:546 (`browse-url--browser-kind').
(function-put 'zach/browse-url-external 'browse-url-browser-kind 'external)

(defun zach/eww-open-in-external-browser (&optional url)
  "Open URL (or the current eww page's URL) in the external browser.
Direct launcher — deliberately avoids `eww-browse-with-external-browser'
/ `browse-url' because the latter's handler dispatch interferes with
our in-eww catch-all (see commentary above)."
  (interactive nil eww-mode)
  (zach/browse-url-external
   (or url (plist-get eww-data :url))))

;; Force every history restore (back OR forward) to re-fetch the URL
;; instead of restoring from the `:text' cache.  Upstream
;; `eww-restore-history' inserts cached text when present, which in
;; practice can be stale/partial (saved between eww's async render and
;; the save point) and shows up as a buffer with only the URL in the
;; header and no body.
;;
;; We keep upstream's `eww-back-url' / `eww-forward-url' untouched —
;; their position-math is well-tested (and bypassing it caused a
;; "No next page" off-by-one the first time we tried custom wrappers).
;; The override goes one layer deeper, at the single function both
;; back and forward call into to apply a history element.
(defun zach/eww-restore-history-reload (elem)
  "Replacement for `eww-restore-history'.
Swap in ELEM's URL/title/etc. as the current `eww-data', then
force a re-fetch via `eww-reload'.  Skips the stale-text cache
path in the original function."
  (let ((inhibit-read-only t))
    (setq eww-data elem)
    (eww-reload)))

(advice-add 'eww-restore-history
            :override #'zach/eww-restore-history-reload)

;;;###autoload
(defun zach/eww-browser-test ()
  "Report the detected external browser argv and whether it's reachable.
Handy when `&' / `go' misbehaves — prints everything to *Messages*."
  (interactive)
  (let* ((argv zach/eww-external-browser-argv)
         (bin  (car argv)))
    (message "[eww] argv=%S | %s on exec-path: %s"
             argv
             (or bin "(nil)")
             (cond ((null bin) "n/a")
                   ((executable-find bin) "yes")
                   (t "NO")))))

;; Keep `browse-url-secondary-browser-function' pointing at our launcher
;; for any caller that *does* respect it (email clients, org links,
;; etc.).  For eww's `&' we bypass it entirely via the rebinding below.
(setq browse-url-secondary-browser-function #'zach/browse-url-external)

;;; ---------------------------------------------------------------------
;;; External handlers (mpv + pdf-tools)

(defun zach/browse-url-mpv (url &rest _args)
  "Open URL in mpv as a detached subprocess.
Used by `browse-url-handlers' for YouTube/Vimeo/Twitch and common
video file extensions."
  (start-process "mpv" nil "mpv" url))
(function-put 'zach/browse-url-mpv 'browse-url-browser-kind 'external)

(defun zach/browse-url-pdf (url &rest _args)
  "Fetch a remote PDF to a temp file and open it.
Uses `pdf-tools' when available; otherwise falls back to
`find-file-other-window' so Emacs at least opens the download.  No-ops
gracefully if pdf-tools isn't installed yet."
  (let ((tmp (make-temp-file "emacs-pdf-" nil ".pdf")))
    (url-copy-file url tmp t)
    (find-file-other-window tmp)
    (when (fboundp 'pdf-view-mode)
      (pdf-view-mode))))
(function-put 'zach/browse-url-pdf 'browse-url-browser-kind 'internal)

;;; ---------------------------------------------------------------------
;;; URL-handler routing

;; Ordering matters: first match wins.  Video sites and extensions are
;; routed to mpv, PDFs to pdf-tools, everything else to eww.  The
;; catch-all "." entry must be last.
(setq browse-url-handlers
      '(("\\(youtube\\.com\\|youtu\\.be\\|vimeo\\.com\\|twitch\\.tv\\)"
         . zach/browse-url-mpv)
        ("\\.\\(mp4\\|mkv\\|webm\\)\\'" . zach/browse-url-mpv)
        ("\\.pdf\\'"                    . zach/browse-url-pdf)
        ("."                            . eww-browse-url)))

;;; ---------------------------------------------------------------------
;;; Helpers

;;;###autoload
(defun zach/eww-here (query)
  "Open eww in the current window.
QUERY is a URL or a search string (routed through `eww-search-prefix').
Uses `display-buffer-overriding-action' to force same-window display,
mirroring `+vterm/here's open-here semantics."
  (interactive (list (read-from-minibuffer "URL or search: ")))
  (let ((display-buffer-overriding-action
         '(display-buffer-same-window)))
    (eww query)))

(defun zach/eww-download-image-at-point ()
  "Download the image at point to `eww-download-directory'.
Looks at `image-url' then `shr-url' text properties — covers both
img tags and linked images."
  (interactive)
  (let ((url (or (get-text-property (point) 'image-url)
                 (get-text-property (point) 'shr-url))))
    (if (not url)
        (message "No image at point")
      (let* ((filename (file-name-nondirectory
                        (url-filename (url-generic-parse-url url))))
             (dest (expand-file-name filename eww-download-directory)))
        (url-copy-file url dest t)
        (message "Saved: %s" dest)))))

(defun zach/eww-show-keys ()
  "Display the eww Evil keybinding cheatsheet.
Covers both evil-collection's built-in bindings and the additions
from this module."
  (interactive)
  (with-current-buffer (get-buffer-create "*eww-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       "eww keybindings (Evil normal state)\n"
       "===================================\n\n"
       "Navigation\n"
       "  H / L    back / forward        u       up a URL level\n"
       "  U        top of site           ^       up a URL level\n"
       "  TAB      next link             S-TAB   previous link\n"
       "  SPC      scroll down           S-SPC   scroll up\n"
       "  ]] / [[  next / prev page link (rel=next / rel=prev)\n"
       "  o        open URL or search    (Doom's SPC o B from anywhere)\n"
       "\n"
       "Reader / fonts / view\n"
       "  =        zoom in               -       zoom out    0  reset\n"
       "  R / r    readable mode         zf      toggle fonts\n"
       "  gr       reload                gf      view source\n"
       "\n"
       "Bookmarks / history / buffers\n"
       "  m        add bookmark          gb      list bookmarks\n"
       "  gh       list history          gt      list eww buffers\n"
       "\n"
       "External / download\n"
       "  &        open in system browser (Junction / LibreWolf / Firefox)\n"
       "  go       (same)                S-RET   (same)\n"
       "  d        download page         D       download image at point\n"
       "\n"
       "Misc\n"
       "  q / ZZ / ZQ   quit window      ?       this help\n")
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer)
                    '(display-buffer-at-bottom
                      . ((window-height . fit-window-to-buffer))))))

;;; ---------------------------------------------------------------------
;;; eww-mode setup + Evil bindings

(add-hook 'eww-mode-hook #'visual-line-mode)

;; Bindings.  Only add what evil-collection-eww doesn't already cover
;; (zoom, image-download, help popup).  Duplicating evil-collection's
;; `&' / `R' / `gr' / `m' / `gb' / `U' would have stomped its sane
;; defaults — e.g., upstream `U' is `eww-top-url', not `shr-copy-url'.
;; The help popup (`?') lists both sets so the cheat-sheet stays one
;; keystroke away.
;;
;; hjkl defensive rebind: built-in `eww-mode-map' binds lowercase `l'
;; to `eww-back-url' (eww.el:1330).  In some Evil-precedence setups
;; that shadows `evil-forward-char' and breaks cursor nav.  Force the
;; four keys back to the Evil defaults here so `H'/`L' remain the
;; (evil-collection) history back/forward keys and hjkl is never
;; ambiguous.
(after! eww
  (evil-define-key* 'normal eww-mode-map
    (kbd "h") #'evil-backward-char
    (kbd "j") #'evil-next-line
    (kbd "k") #'evil-previous-line
    (kbd "l") #'evil-forward-char
    (kbd "=") #'text-scale-increase
    (kbd "-") #'text-scale-decrease
    (kbd "0") #'text-scale-adjust
    (kbd "D") #'zach/eww-download-image-at-point
    (kbd "?") #'zach/eww-show-keys
    ;; Rebind all three "open externally" keys away from
    ;; `eww-browse-with-external-browser' — see commentary above
    ;; `zach/eww--detect-external-browser' for why it can't be trusted.
    (kbd "&")           #'zach/eww-open-in-external-browser
    (kbd "go")          #'zach/eww-open-in-external-browser
    (kbd "S-<return>")  #'zach/eww-open-in-external-browser))

(provide 'eww-config)
;;; eww.el ends here
