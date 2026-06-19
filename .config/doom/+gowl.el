;;; +gowl.el -*- lexical-binding: t; -*-
;;
;; +gowl.el - Gowl Wayland compositor integration
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
;; Configuration for running Emacs as the Gowl Wayland compositor.
;; Loaded only when the running Emacs answers `gowl-running-p'; the
;; gating constant `IS-GOWL' is defined in config.el and must precede
;; the `(load! "+gowl")' call.
;;
;; Sets up:
;;   - Wallpaper, window opacity, tiling gaps, rounded corners.
;;   - Window rules for auto-floating dialogs.
;;   - Dropdown terminal provider.
;;   - Top + bottom Catppuccin status bars (system widgets, IP, podman,
;;     pomodoro), title-coloring on the top bar.
;;   - Frame transparency (alpha-background) without forcing fullscreen.
;;   - Event-driven bar-title sync (window-buffer-change-functions and
;;     window-selection-change-functions) instead of a polling timer.
;;   - Multi-monitor layout via `cmacs-gowl-setup-monitors'.
;;
;; The `(when IS-GOWL ...)' block here no-ops on non-gowl Emacs.
;; `cmacs-gowl-setup-monitors' is defined unconditionally so it can be
;; invoked manually with M-x even outside the gowl session.

;;; Code:

(when IS-GOWL
  ;; Initialize gowl Elisp integration (loads bundled modules)
  (gowl-start)

  ;; Per-host monitor rotation: otg-zach and pda-zach have their panels
  ;; physically mounted at 270°, so apply transform 3 to compensate.
  (when (member (system-name) '("otg-zach" "pda-zach"))
    (gowl-set-monitor-transform 3))

  ;; Window opacity — dim unfocused clients
  (gowl-enable-module "alpha")
  (gowl-set-focused-alpha 0.9)
  (gowl-set-unfocused-alpha 0.9)

  ;; Wallpaper
  (gowl-set-wallpaper "~/Pictures/wallpaper.png")

  ;; Tiling gaps — outer gaps give the frame breathing room from edges
  (gowl-enable-module "vanitygaps")
  (gowl-set-gaps '(("inner-gap" . "0") ("outer-gap" . "32")))

  ;; Rounded corners -- because it looks cool
  (gowl-enable-module "roundcorners")
  (gowl-set-corner-radius 12)

  ;; Window rules: auto-float popups/dialogs/modals (Zoom, pavucontrol,
  ;; pinentry, KeePassXC unlock, file choosers, ...).  Rules come from
  ;; `cmacs-gowl-float-rules' and are pushed by `cmacs-gowl-mode' below.
  (gowl-enable-module "windowrules")

  ;; Dropdown terminals: guake-style drop-from-top windows toggled by
  ;; Super+grave.  Entries come from `cmacs-gowl-dropdowns' and are
  ;; pushed + adopted by `cmacs-gowl-mode' below.  Must be enabled
  ;; before `cmacs-gowl-mode' so its --apply-dropdowns + refresh see
  ;; a loaded provider.
  (gowl-enable-module "dropdown")

  ;; Prevent Doom from fullscreening the frame (gaps need tiled mode)
  (setq default-frame-alist
        (assq-delete-all 'fullscreen default-frame-alist))

  ;; PGTK frame transparency — render background with alpha so the
  ;; wallpaper shows through.  Text and UI elements stay fully opaque.
  (setq default-frame-alist
        (assq-delete-all 'alpha-background default-frame-alist))
  (add-to-list 'default-frame-alist '(alpha-background . 85))

  ;; Status bar — top: title + system widgets + clock
  (gowl-bar-enable)
  (gowl-bar-configure
    '(("position" . "top")
      ("widgets" . "cpu memory disk:/var battery clock")
      ("cpu-color" . "#a6e3a1")
      ("memory-color" . "#89b4fa")
      ("disk-color" . "#f9e2af")
      ("battery-color" . "#94e2d5")
      ("clock-color" . "#cdd6f4")
      ;; Colorize title text — split on delimiters, cycle Catppuccin palette
      ("title-delimiters" . "-._/: *")
      ("title-delimiter-color" . "#585b70")
      ("title-palette" . "#89b4fa #a6e3a1 #f9e2af #f5c2e7 #94e2d5 #cba6f7 #fab387 #89dceb")))

  ;; Bottom bar — networking + container + pomodoro (left → right).
  ;; Same Catppuccin style as the top bar; per-widget colors picked
  ;; from the same palette so it visually feels like a sibling.
  ;;
  ;; pomo widget uses the default 10 s cmd interval (no @N override).
  ;; At @1 the bar ticker pulled the whole compositor refresh to 1 Hz
  ;; and spawned a subprocess every second even at idle — the pomodoro
  ;; display doesn't need sub-10s granularity.
  (gowl-bar-configure
    '(("position" . "bottom")
      ("widgets" . "ip podman cmd:~/bin/scripts/pomo")
      ("ip-color" . "#cba6f7")
      ("podman-color" . "#fab387")
      ("cmd-color" . "#f5c2e7")
      ;; Colorize the title (if any) the same way the top bar does
      ("title-delimiters" . "-._/: *")
      ("title-delimiter-color" . "#585b70")
      ("title-palette" . "#89b4fa #a6e3a1 #f9e2af #f5c2e7 #94e2d5 #cba6f7 #fab387 #89dceb")))

  (defun gowl-bar-restart ()
    "Kill and re-enable the gowl bar (fixes sizing after resize)."
    (interactive)
    (gowl-bar-disable)
    (gowl-bar-enable))

  ;; Bar title sync: push the current buffer name to the top bar
  ;; whenever the visible buffer changes.  Previously polled at 5 Hz
  ;; from a repeating timer which kept the whole redisplay / GC /
  ;; pgtk draw cascade alive at idle (perf showed ~29% pixman, driven
  ;; by redisplay invalidations on every timer tick).  Hook-driven now.
  (defvar gowl--bar-last-title nil
    "Last title pushed to the gowl top bar via `gowl-bar-set-title'.")

  (defun gowl--sync-bar-title (&rest _)
    "Push the current buffer name to the gowl top bar if it changed.
Attached to `window-buffer-change-functions' and
`window-selection-change-functions' so it only runs on actual
window-state transitions."
    (ignore-errors
      (let ((title (buffer-name (window-buffer (selected-window)))))
        (unless (equal title gowl--bar-last-title)
          (setq gowl--bar-last-title title)
          (gowl-bar-set-title title)))))

  ;; After Doom finishes frame setup: un-fullscreen, set alpha-background,
  ;; and wire up the bar title sync.
  (add-hook 'doom-after-init-hook
            (lambda ()
              (set-frame-parameter nil 'fullscreen nil)
              (set-frame-parameter nil 'alpha-background 85)
              ;; Event-driven title sync. `window-buffer-change-functions'
              ;; fires once per command loop after a window's displayed
              ;; buffer changes — exactly when we need to update the bar.
              ;; A tiny idle timer handles the case where the first frame
              ;; is still settling when the hooks are installed.
              (add-hook 'window-buffer-change-functions
                        #'gowl--sync-bar-title)
              (add-hook 'window-selection-change-functions
                        #'gowl--sync-bar-title)
              (run-with-idle-timer 0.5 nil #'gowl--sync-bar-title)
              ;; Enable cmacs-gowl-mode so its --start arm pushes
              ;; `cmacs-gowl-float-rules' and `cmacs-gowl-dropdowns'
              ;; into the running gowl config and calls
              ;; `gowl-dropdown-refresh' so Super+grave binds to the
              ;; first entry's :keybind immediately.  Runs after the
              ;; windowrules + dropdown modules are loaded above so
              ;; there's a provider to receive the data.
              (when (fboundp 'cmacs-gowl-mode)
                (cmacs-gowl-mode 1))
              ;; Multi-monitor: position monitors then create per-monitor frames.
              ;; Only runs when more than one monitor is connected.
              (when (> (gowl-monitor-count) 1)
                (cmacs-gowl-setup-monitors)))))

(defun cmacs-gowl-setup-monitors ()
  "Position monitors to match physical layout and create one frame per monitor.
Monitors are identified by name.  The layout below matches the GNOME
config: two LG SDQHD side-by-side on top, laptop centered below."
  (interactive)
  (let ((monitors (gowl-list-monitors))
        (layout nil))
    ;; Build name→monitor lookup
    (dolist (m monitors)
      (push (cons (cdr (assq 'name (gowl-monitor-info m))) m) layout))
    ;; Position monitors (adjust for your physical layout)
    (let ((dp1 (or (cdr (assoc "DP-11" layout))
                   (cdr (assoc "DP-9" layout))
                   (cdr (assoc "DP-13" layout))))
          (dp2 (or (cdr (assoc "DP-12" layout))
                   (cdr (assoc "DP-10" layout))
                   (cdr (assoc "DP-14" layout))))
          (edp (cdr (assoc "eDP-1" layout))))
      (when dp1 (gowl-set-monitor-position 0 0 dp1))
      (when dp2 (gowl-set-monitor-position 2560 0 dp2))
      (when edp (gowl-set-monitor-position 1501 2880 edp)))
    ;; Wait for layout to settle, then create one frame per extra monitor
    (run-with-timer 0.5 nil
      (lambda ()
        (let ((n-extra (1- (gowl-monitor-count))))
          (dotimes (_ n-extra)
            (make-frame '((fullscreen . nil)
                          (alpha-background . 85)))))))))

;; Doom's persp-mode workspace autosave file
;; (~/.config/emacs/.local/etc/workspaces/autosave) carries a cosmetic
;; file-local `eval: (progn (pp-buffer)(indent-buffer))' footer.  persp visits
;; that file to restore the session; the unsafe eval used to pop a confirmation
;; that interrupted the visit and left a stray buffer lingering, which Doom's
;; focus auto-revert then re-surfaced after unlocking the gowl screen lock
;; (dumping a buffer full of elisp instead of returning you to where you were).
;; Ignore that eval so the file is read silently and never lingers as a buffer:
;; it is persp's internal state, the eval is only for human readability, and
;; `indent-buffer' isn't even defined here (running it just errors).
(add-to-list 'ignored-local-variable-values
             '(eval progn (pp-buffer) (indent-buffer)))

(provide '+gowl)
;;; +gowl.el ends here
