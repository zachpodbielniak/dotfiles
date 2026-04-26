;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "Zach Podbielniak")

;;; exec-path: ensure Emacs can find tools regardless of how it was launched.
;;; When started from a desktop file or systemd, PATH is minimal and misses
;;; linuxbrew, ~/bin/scripts, cargo, etc.  Add them here once rather than
;;; patching each package individually.
(dolist (dir '("/home/linuxbrew/.linuxbrew/bin"
              "/home/linuxbrew/.linuxbrew/sbin"
              "~/bin/scripts"
              "~/bin"
              "~/.local/bin"
              "~/.cargo/bin"))
  (let ((expanded (expand-file-name dir)))
    (when (file-directory-p expanded)
      (add-to-list 'exec-path expanded)
      (setenv "PATH" (concat expanded ":" (getenv "PATH"))))))
(let ((perl5lib (expand-file-name "~/perl5/lib/perl5")))
  (when (file-directory-p perl5lib)
    (setenv "PERL5LIB" (concat perl5lib ":" (or (getenv "PERL5LIB") "")))))


;;;; =========================================================================
;;;; Phase 1: Foundation — Theme, UI, Core Editor
;;;; =========================================================================

;;; Custom splash screen
(setq fancy-splash-image "~/Pictures/Immutablue/immutablue-logo.jpg")

;;; Font: Hack Nerd Font Mono (matching gst terminal config)
;;; Nerd Font covers modeline icons, Noto Color Emoji for emoji
(let ((sz (if IS-MAC 14 18))
      (big (if IS-MAC 20 24)))
  (setq doom-font (font-spec :family "Hack Nerd Font Mono" :size sz)
        doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size sz)
        doom-big-font (font-spec :family "Hack Nerd Font Mono" :size big)
        doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size sz)
        doom-emoji-font (font-spec :family "Noto Color Emoji" :size sz)))

;;; Fix font fallback height mismatches that cause vterm line-height bobbing.
;;; PGTK Emacs (Wayland) computes line height per-glyph.  TUI spinner chars
;;; (e.g. ✻ U+273B in Claude Code) fall back to system fonts with taller
;;; metrics, making the spinner line expand and pushing everything below down.
;;;
;;; Diagnostic: eval in M-: with a *font-check* buffer visible to find bad fonts
;;; Default font (Hack Nerd Font Mono) is 22px.  Measured fallback heights:
;;;   Noto Sans Symbols: 38px  |  Noto Sans Symbols 2: 32px  |  Jomolhari: 29px
;;;   Adwaita Mono: 24px  |  Adwaita Sans: 23px  |  FiraCode Nerd Font: 23px
;;;   STIX: 19px
;;;
;;; Fix (two-pronged):
;;; 1) Force Hack for ranges where it has correct-height glyphs (braille spinners)
;;; 2) Rescale ALL remaining fallback fonts to match 22px
;;;
;;; Remaining slight bob: not font-height-related (confirmed zero mismatches in
;;; vterm buffer).  Likely PGTK/Wayland compositor artifacts during rapid redraws.
;;; vterm-timer-delay 0.02 reduces this further.  Tried and rejected:
;;;   - line-height t via default-text-properties (breaks tmux status bar)
;;;   - set-fontset-font for all of 'unicode (Hack Nerd Font patched glyphs
;;;     have bad bounding boxes in PUA ranges, made bobbing worse)
(dolist (range '((#x2600 . #x26FF)    ;; Misc Symbols (★⚡)
                (#x2700 . #x27BF)    ;; Dingbats (✓✗)
                (#x2800 . #x28FF)))  ;; Braille Patterns (⠋⠙⠹⠸ — TUI spinners)
  (set-fontset-font t range "Hack Nerd Font Mono"))
(setq face-font-rescale-alist
      '(("-Noto Sans Symbols 2-" . 0.65)   ;; 32px → ≤22px
        ("-Noto Sans Symbols-" . 0.55)     ;; 38px → ≤22px
        ("-Jomolhari-" . 0.72)             ;; 29px → ≤22px
        ("-Adwaita Mono-" . 0.88)          ;; 24px → ≤22px
        ("-Adwaita Sans-" . 0.92)          ;; 23px → ≤22px
        ("-FiraCode Nerd Font-" . 0.92)    ;; 23px → ≤22px
        ("-STIX-" . 1.10)))

;;; Theme: Catppuccin Mocha (replaces doom-one, matches nvim catppuccin)
(setq doom-theme 'catppuccin
      catppuccin-flavor 'mocha)

;;; Line numbers
(setq display-line-numbers-type t)

;;; GTK-style right-click context menu (Emacs 28+).  Without this,
;;; mouse-3 is `mouse-save-then-kill'; with it, mouse-3 opens an
;;; extensible menu that packages like org-remark inject entries
;;; into (mark / open / change pen) automatically.
(context-menu-mode 1)

;;; Steady cursor — no blink.
;;; Each blink invalidates the cursor region, which runs through PGTK's
;;; redisplay → cairo → wl_surface attach+commit, damaging the gowl
;;; scene at 0.5 Hz for zero informational value.  With this off, the
;;; emacs PGTK frame truly goes silent between user input and timer
;;; firings.
(blink-cursor-mode -1)

;;; Org directory: point at PARA knowledge base root
(setq org-directory "~/Documents/notes/")

;;;; -------------------------------------------------------------------------
;;;; TRAMP: make remote file access usable
;;;; -------------------------------------------------------------------------
;;; Without these guards, opening /ssh:host:/path hangs Emacs because
;;; dirvish, vc, projectile, LSP, and syntax checkers all fire synchronous
;;; subprocess calls over the SSH connection.

;; Use ssh directly (not scp) and keep connections alive
(after! tramp
  (setq tramp-default-method "ssh"
        tramp-verbose 1                         ;; minimal logging (raise to 6 to debug)
        tramp-auto-save-directory (expand-file-name "tramp-autosave" doom-cache-dir)
        remote-file-name-inhibit-cache nil       ;; cache remote stat results
        tramp-completion-reread-directory-timeout nil  ;; don't re-stat for completion
        vc-ignore-dir-regexp (format "%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))  ;; disable VC over TRAMP

  ;; Tell eshell to use TRAMP for remote paths (cd /ssh:host:/)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;;; Eshell performance tuning & TRAMP integration (see eshell.el)
(load! "eshell")

;; text-mode (and markdown-mode, which inherits) adds `ispell-completion-at-point'
;; to `completion-at-point-functions'. It shells out to `look' via `process-file'
;; on every completion attempt — over TRAMP that's a full SSH round-trip per
;; keystroke and freezes Emacs. Kill it globally.
(setq text-mode-ispell-word-completion nil)

;; Keep auto-save files local instead of writing them through SSH each cycle
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" doom-cache-dir) t)))

(after! recentf
  (setq recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude tramp-file-name-regexp))

;; Belt and suspenders: if anything re-adds `ispell-completion-at-point' in a
;; remote buffer, strip it on find-file.
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local completion-at-point-functions
                          (remq 'ispell-completion-at-point
                                completion-at-point-functions)))))

;; Disable projectile on remote files — walking the tree over SSH is brutal
(after! projectile
  (defadvice! zach--projectile-skip-remote-a (fn &rest args)
    :around #'projectile-project-root
    (unless (file-remote-p default-directory)
      (apply fn args))))

;; Disable LSP/eglot on remote buffers
(after! eglot
  (defadvice! zach--eglot-skip-remote-a (fn &rest args)
    :around #'eglot-ensure
    (unless (file-remote-p default-directory)
      (apply fn args))))

;; Disable syntax checking on remote files
(after! flycheck
  (defadvice! zach--flycheck-skip-remote-a (fn &rest args)
    :around #'flycheck-mode
    (unless (file-remote-p default-directory)
      (apply fn args))))
(after! flymake
  (add-hook 'flymake-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (flymake-mode -1)))))

;; Dirvish: disable expensive attributes on remote directories
(after! dirvish
  (defadvice! zach--dirvish-simple-remote-a (fn &rest args)
    :around #'dirvish
    (if (file-remote-p default-directory)
        (let ((dirvish-attributes '(hl-line))
              (dirvish-preview-dispatchers nil))
          (apply fn args))
      (apply fn args)))
  ;; Also guard the dired override so plain dired on remote paths stays plain
  (defadvice! zach--dired-skip-dirvish-remote-a (fn &rest args)
    :around #'dired
    (if (and (car args) (file-remote-p (car args)))
        (let ((dirvish-override-dired-mode nil))
          (apply fn args))
        (apply fn args))))

;; Treemacs: don't set up file watchers on remote paths
(after! treemacs
  (defadvice! zach--treemacs-skip-remote-watch-a (fn &rest args)
    :around #'treemacs--start-watching
    (unless (file-remote-p default-directory)
      (apply fn args))))

;;; Dired: show dotfiles and parent directory (..)
(setq dired-listing-switches "-ahl")

;;; wdired: bulk rename by editing filenames in dired
(after! dired
  (setq wdired-allow-to-change-permissions t
        wdired-allow-to-redirect-links t)
  (map! :map dired-mode-map
        :localleader
        "w" #'wdired-change-to-wdired-mode))

;;; Dirvish: enhanced dired with preview and multi-column layout
;; Preview deps: brew install libvips mediainfo
(use-package! dirvish
  :after dired
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (setq dirvish-attributes '(hl-line subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index))
        dirvish-preview-dispatchers '(image gif video audio epub pdf archive))
  (map! :leader
        :desc "Dirvish"      "d d" #'dirvish
        :desc "Dirvish dwim" "d s" #'dirvish-side)
  (map! :map dirvish-mode-map
        :n "q"   #'dirvish-quit
        :n "TAB" #'dirvish-subtree-toggle
        :n "a"   #'dirvish-quick-access
        :n "s"   #'dirvish-quicksort
        :n "y"   #'dirvish-yank-menu
        :n "f"   #'dirvish-fd))

;;; Treemacs: quiet down the file watcher.
;;;
;;; Perf of `cmacs --gowl` at idle showed `treemacs--filewatch-callback`
;;; consuming ~8% CPU (inclusive) and driving a cascade of buffer
;;; switches → redisplay → pgtk pixman redraws.  Causes: build artifacts
;;; (native-lisp/, *.eln, *.o, build/) churning inside watched
;;; directories, and a 2 s default coalescing delay that's still too
;;; short once inotify is firing in bursts.
;;;
;;; Fixes:
;;;   1. Coalesce events over 10 s so bursts collapse into one update.
;;;   2. Hide git-ignored files so treemacs never displays (and thus
;;;      never watches) build dirs — our .gitignore already excludes
;;;      native-lisp/, *.eln, *.o, build/.
(after! treemacs
  (setq treemacs-file-event-delay 10000)
  (when (fboundp 'treemacs-hide-gitignored-files-mode)
    (treemacs-hide-gitignored-files-mode 1)))

;;; Indentation: tabs, 4 spaces width (matching nvim config)
(setq-default indent-tabs-mode t
              tab-width 4
              c-basic-offset 4
              evil-shift-width 4)
;; Doom's CC module sets c-basic-offset from tab-width after config loads,
;; so re-apply in the hook to ensure 4-wide tabs in C/C++ buffers.
(after! cc-mode
  (setq-default c-basic-offset 4)
  (add-hook 'c-mode-common-hook
            (lambda () (setq tab-width 4 c-basic-offset 4 evil-shift-width 4))))

;;; Transparent background (replaces transparent.nvim)
;;; GUI: set frame alpha; Terminal: clear face backgrounds
(if (display-graphic-p)
    (if (eq system-type 'darwin)
        ;; macOS NS port: alpha-background is not supported, use alpha instead
        (set-frame-parameter nil 'alpha '(95 . 85))
      (set-frame-parameter nil 'alpha-background 95))
  (add-hook 'doom-load-theme-hook
            (lambda ()
              (set-face-background 'default nil)
              (set-face-background 'fringe nil)
              (when (facep 'solaire-default-face)
                (set-face-background 'solaire-default-face nil)))))
;; Apply alpha to new frames too
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(alpha . (85 . 75)))
  (add-to-list 'default-frame-alist '(alpha-background . 85)))

;; macOS native fullscreen creates a separate Space and drops transparency.
;; Use non-native fullscreen (maximized within current Space) instead.
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;;; Gowl compositor modules (only when running as Wayland compositor)
(defconst IS-GOWL (and (fboundp 'gowl-running-p) (gowl-running-p)))

(when IS-GOWL
  ;; Initialize gowl Elisp integration (loads bundled modules)
  (gowl-start)

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

;;; Modeline (tmux-style status bar with catppuccin colors and icons)
(after! doom-modeline
  ;; Enable battery and time display (skip battery on headless/servers)
  (when (and (display-graphic-p)
             (not (string-empty-p (string-trim (shell-command-to-string "upower -e 2>/dev/null")))))
    (display-battery-mode 1))
  (display-time-mode 1)

  ;; General appearance
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 25
        doom-modeline-time-icon t
        ;; No "live" animated clock glyph — it ticks at 1 Hz, invalidates
        ;; the modeline, triggers redisplay + a wl_surface commit on every
        ;; tick, and you can't actually tell the difference at a glance.
        doom-modeline-time-live-icon nil
        doom-modeline-battery (display-graphic-p)
        doom-modeline-env-version t
        doom-modeline-modal-icon t
        doom-modeline-modal-modern-icon t)

  ;; Time format (matches tmux  %H:%M)
  (setq display-time-format " %H:%M"
        display-time-default-load-average nil)

  ;; ---------------------------------------------------------------------------
  ;; Custom segment: days-since trackers (cached, updates every 60s)
  ;; Matches tmux right status: 🥩:days 🥤:days ☕:days
  ;; ---------------------------------------------------------------------------
  (defvar zach-modeline--days-cache ""
    "Cached string for days-since trackers.")

  (defvar zach-modeline--days-timer nil
    "Timer for updating days-since cache.")

  (defun zach-modeline--update-days ()
    "Update the days-since cache by calling the days_since script."
    (let ((carnivore (string-trim (shell-command-to-string "days_since 2024-11-24")))
          (soda      (string-trim (shell-command-to-string "days_since 2025-07-14")))
          (coffee    (string-trim (shell-command-to-string "days_since 2025-09-20"))))
      (setq zach-modeline--days-cache
            (concat
             (propertize (format " 🥩:%s" carnivore) 'face '(:foreground "#f38ba8"))
             (propertize (format " 🥤:%s" soda)      'face '(:foreground "#89b4fa"))
             (propertize (format " ☕:%s" coffee)     'face '(:foreground "#a6e3a1"))
             " "))))

  ;; Update immediately at startup, then every 60 seconds
  (zach-modeline--update-days)
  (when zach-modeline--days-timer (cancel-timer zach-modeline--days-timer))
  (setq zach-modeline--days-timer
        (run-with-timer 60 60 #'zach-modeline--update-days))

  (doom-modeline-def-segment days-since
    "Days-since trackers: carnivore, soda, coffee."
    zach-modeline--days-cache)

  ;; ---------------------------------------------------------------------------
  ;; Custom segment: pomodoro timer (cached, updates every 5s)
  ;; ---------------------------------------------------------------------------
  (defvar zach-modeline--pomo-cache ""
    "Cached string for pomodoro status.")

  (defvar zach-modeline--pomo-timer nil
    "Timer for updating pomodoro cache.")

  (defun zach-modeline--update-pomo ()
    "Update the pomodoro cache by calling the pomo script."
    (let ((pomo (string-trim (shell-command-to-string "pomo"))))
      (setq zach-modeline--pomo-cache
            (if (string-empty-p pomo) ""
              (propertize (format " %s" pomo) 'face '(:foreground "#fab387"))))))

  (zach-modeline--update-pomo)
  (when zach-modeline--pomo-timer (cancel-timer zach-modeline--pomo-timer))
  ;; 15 s is plenty for a pomodoro display — every 5 s was spawning
  ;; a subprocess 12×/minute at idle (which also hit the redisplay +
  ;; GC path each time).
  (setq zach-modeline--pomo-timer
        (run-with-timer 15 15 #'zach-modeline--update-pomo))

  (doom-modeline-def-segment pomodoro
    "Pomodoro timer status."
    zach-modeline--pomo-cache)

  ;; ---------------------------------------------------------------------------
  ;; Custom modeline layout
  ;; Left:  evil-state | buffer | git-branch | major-mode | diagnostics |
  ;;        encoding/line-endings | battery
  ;; Right: line:col | time | pomodoro | days-since trackers
  ;; ---------------------------------------------------------------------------
  (doom-modeline-def-modeline 'zach-modeline
    '(modals matches buffer-info remote-host vcs major-mode check
      buffer-encoding battery)
    '(misc-info buffer-position time pomodoro days-since))

  ;; Apply to all buffers
  (add-hook 'doom-modeline-mode-hook
            (lambda () (doom-modeline-set-modeline 'zach-modeline 'default))))

;;; Color code highlighting (replaces nvim-colorizer.lua)
(use-package! rainbow-mode
  :hook (prog-mode . rainbow-mode))

;;; CSV/TSV: align columns visually + per-column coloring
(add-hook 'csv-mode-hook #'csv-align-mode)
(add-hook 'tsv-mode-hook #'csv-align-mode)

(use-package! rainbow-csv
  :hook ((csv-mode . rainbow-csv-mode)
         (tsv-mode . rainbow-csv-mode)))

;;; Indent guides: highlight current depth (replaces snacks.nvim indent)
(after! indent-bars
  (setq indent-bars-highlight-current-depth t))

;;; Emoji picker keybind (replaces telescope_emoji.lua)
(map! :leader
      :desc "Insert emoji" "i e" #'emoji-search)

;;; Buffer cycling: ]b / [b (vim-unimpaired style)
;;; NOTE: do NOT bind TAB in normal mode — it shadows SPC TAB (workspace prefix)
(map! :n "]b" #'next-buffer
      :n "[b" #'previous-buffer)

;;; Clipboard paste (GTK GUI)
(map! "C-S-v" #'clipboard-yank)

;;; browse-kill-ring: evil keybinds (defaults are Emacs-style)
(after! browse-kill-ring
  (set-evil-initial-state! 'browse-kill-ring-mode 'normal)
  (evil-define-key 'normal browse-kill-ring-mode-map
    "j"   #'browse-kill-ring-forward
    "k"   #'browse-kill-ring-previous
    "n"   #'browse-kill-ring-search-forward
    "N"   #'browse-kill-ring-search-backward
    "/"   #'browse-kill-ring-search-forward
    "?"   #'browse-kill-ring-search-backward
    "d"   #'browse-kill-ring-delete
    "y"   #'browse-kill-ring-insert-and-quit
    "p"   #'browse-kill-ring-insert-and-quit
    "i"   #'browse-kill-ring-insert
    "a"   #'browse-kill-ring-append-insert
    "u"   #'browse-kill-ring-undo-other-window
    "r"   #'browse-kill-ring-update
    "gr"  #'browse-kill-ring-update
    "q"   #'browse-kill-ring-quit
    (kbd "RET") #'browse-kill-ring-insert-and-quit
    (kbd "ESC") #'browse-kill-ring-quit))

;;; Scratch buffers in emacs-lisp-mode (for SPC m e e eval, etc.)
(setq initial-major-mode 'lisp-interaction-mode          ; *scratch* buffer
      doom-scratch-buffer-major-mode 'lisp-interaction-mode) ; SPC x scratch

;;; Sync all Evil yank/delete operations to system clipboard
(after! evil
  (setq evil-want-clipboard t)

  ;; Visual line motion: gj/gk move by display lines in word-wrapped buffers
  ;; Global binding for non-org modes
  (evil-define-key 'normal 'global
    "gj" #'evil-next-visual-line
    "gk" #'evil-previous-visual-line)
  (evil-define-key 'visual 'global
    "gj" #'evil-next-visual-line
    "gk" #'evil-previous-visual-line))

;; Override evil-org's gj/gk (org-forward/backward-element) with visual line motion
(after! evil-org
  (evil-define-key '(normal visual) evil-org-mode-map
    "gj" #'evil-next-visual-line
    "gk" #'evil-previous-visual-line))

;;; Fix "wrong type argument: plistp, t" when persp-mode isn't initialized yet.
;;; persp-mode returns t instead of a perspective struct on first use after
;;; launch.  Wrap affected functions so they fall back gracefully.
(defadvice! +workspaces-switch-to-project-retry-a (fn &rest args)
  "Catch plistp error on first project switch and retry."
  :around #'+workspaces-switch-to-project-h
  (condition-case _err
      (apply fn args)
    (wrong-type-argument (apply fn args))))

(defadvice! +workspace-buffer-list-safe-a (fn &rest args)
  "Fall back to global buffer list when persp-mode isn't ready."
  :around #'+workspace-buffer-list
  (condition-case _err
      (apply fn args)
    (wrong-type-argument (buffer-list))))


;;; Toggle-maximize: SPC w m m zooms current window, repeat to restore.
(defvar +my/window-maximize-register nil
  "Register used to store window config before maximizing.")

(defun +my/toggle-maximize-window ()
  "Toggle maximizing the current window."
  (interactive)
  (if (and +my/window-maximize-register (= 1 (length (window-list))))
      (progn
        (jump-to-register +my/window-maximize-register)
        (setq +my/window-maximize-register nil))
    (setq +my/window-maximize-register (gensym))
    (window-configuration-to-register +my/window-maximize-register)
    (delete-other-windows)))

(map! :leader "w m m" #'+my/toggle-maximize-window)


;;;; =========================================================================
;;;; Phase 2: LSP, Languages, Completion
;;;; =========================================================================

;;; Eglot: register standard language servers
;;; (crispy-mode, bacon-mode, podomation-mode are registered by their packages)
(after! eglot
  (add-to-list 'eglot-server-programs
               '(dockerfile-mode . ("docker-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(makefile-mode . ("autotools-language-server")))
  (add-to-list 'eglot-server-programs
               '(perl-mode . ("perlnavigator")))
  (add-to-list 'eglot-server-programs
               '(r-mode . ("R" "--slave" "-e" "languageserver::run()"))))

;;; C mode: gnu style for gnu89 (matching nvim config)
(after! cc-mode
  (setq c-default-style '((c-mode . "gnu")
                           (other . "gnu")))
  ;; Disable cc-mode electric reindentation on {, }, ;, :, #
  (setq c-electric-flag nil)
  (add-hook 'c-mode-hook (lambda () (electric-indent-local-mode -1)))
  (add-hook 'c++-mode-hook (lambda () (electric-indent-local-mode -1))))

;;; Disable electric-indent globally — manual format only (SPC f m)
(electric-indent-mode -1)

;;; LSP keybindings (most already mapped by Doom Evil+eglot defaults)
(map! :leader
      :desc "Signature help" "l s" #'eglot-signature-help
      :desc "Buffer diagnostics" "l f" #'flymake-show-buffer-diagnostics
      :desc "Format buffer" "f m" #'+format/buffer)


;;;; =========================================================================
;;;; Phase 3: Git, File Navigation, Terminal, Tmux
;;;; =========================================================================

;;; Tmux pane navigation (replaces vim-tmux-navigator)
;;; Seamless Ctrl-h/j/k/l across Emacs windows and tmux panes
(use-package! navigate
  :config
  (require 'navigate)
  ;; Ensure C-l isn't swallowed by Doom's global recenter binding
  ;; and that all four directions work in normal, motion, and visual states
  (map! :nvm "C-h" (cmd! (tmux-navigate "left"))
        :nvm "C-j" (cmd! (tmux-navigate "down"))
        :nvm "C-k" (cmd! (tmux-navigate "up"))
        :nvm "C-l" (cmd! (tmux-navigate "right"))))

;;; Git keybindings (extending magit + diff-hl from vc-gutter)
(map! :leader
      :desc "Blame line"  "g b" #'magit-blame-addition
      :desc "Next hunk"   "g h n" #'diff-hl-next-hunk
      :desc "Prev hunk"   "g h p" #'diff-hl-previous-hunk
      :desc "Revert hunk" "g h r" #'diff-hl-revert-hunk
      :desc "Diff hunk"   "g h d" #'diff-hl-diff-goto-hunk)

;;; File navigation keybinds (matching nvim telescope bindings)
(map! :leader
      :desc "Live grep (project)" "f w" #'+vertico/project-search
      :desc "Switch buffer" "f b" #'switch-to-buffer
      :desc "Recent files" "f o" #'recentf-open-files
      :desc "Search in buffer" "f z" #'+vertico/search-symbol)

;;; Terminal keybindings (replaces nvterm Alt-i/h/v toggles)
;; Alt-i: floating/popup terminal
(map! :ni "M-i" #'+vterm/toggle)
;; Alt-h: horizontal terminal (bottom split)
(map! :ni "M-h" (cmd! (split-window-below) (other-window 1) (+vterm/here nil)))
;; Alt-v: vertical terminal (right split)
(map! :ni "M-v" (cmd! (split-window-right) (other-window 1) (+vterm/here nil)))
(map! :leader
      :desc "Vterm popup"        "o t" #'+vterm/toggle
      :desc "Vterm here"         "o T" #'+vterm/here
      :desc "Browser (eww) here" "o B" #'zach/eww-here)

;;; Vterm TUI stability
(setq vterm-always-compile-module t)
(after! vterm
  ;; Process output faster — fewer visible intermediate redraw states
  (setq vterm-timer-delay 0.02))
(add-hook! 'vterm-mode-hook
  (display-line-numbers-mode -1))

;;; C-Escape: send a literal Escape to the active buffer's underlying
;;; program.  Useful inside vterm running nvim or a nested `emacs -nw`,
;;; or — when running under gowl — inside a gowl-embedded Wayland app.
;;; Plain Escape is intercepted by evil-mode (and, in the gowl embed
;;; case, by the embed itself, which uses Escape to return focus to
;;; Emacs).
(if IS-GOWL
    (defun +zach/send-escape ()
      "Send a literal Escape to the program backing the current buffer.
Gowl-aware variant: handles embedded Wayland clients via the
compositor seat."
      (interactive)
      (cond
       ;; gowl-embedded Wayland client: focus it, then inject KEY_ESC
       ;; (evdev keycode 1) press+release through the compositor seat.
       ((and (boundp 'gowl-embedded-client) gowl-embedded-client)
        (when (fboundp 'gowl-embed-focus)
          (gowl-embed-focus gowl-embedded-client))
        (gowl-send-key 1 t)
        (gowl-send-key 1 nil))
       ((derived-mode-p 'vterm-mode)
        (vterm-send-escape))
       ((derived-mode-p 'term-mode)
        (term-send-raw-string "\e"))
       ((derived-mode-p 'eat-mode)
        (eat-self-input 1 ?\e))
       (t
        (execute-kbd-macro (kbd "<escape>")))))
  (defun +zach/send-escape ()
    "Send a literal Escape to the program backing the current buffer."
    (interactive)
    (cond
     ((derived-mode-p 'vterm-mode)
      (vterm-send-escape))
     ((derived-mode-p 'term-mode)
      (term-send-raw-string "\e"))
     ((derived-mode-p 'eat-mode)
      (eat-self-input 1 ?\e))
     (t
      (execute-kbd-macro (kbd "<escape>"))))))
(map! :gnvime "C-<escape>" #'+zach/send-escape)

;;; Make target runner keybindings
(map! :leader
      :desc "Run make target" "c m" #'+make/run
      :desc "Run last target" "c M" #'+make/run-last)

;;; Window selection (ace-window, complements tmux-navigator)
(map! :leader
      :desc "Select window" "w w" #'ace-window)

;;; Justfile syntax highlighting (replaces vim-just)
(use-package! just-mode
  :mode ("justfile\\'" "Justfile\\'" "\\.just\\'"))


;;;; =========================================================================
;;;; Phase 4: Content, Notes, Media
;;;; =========================================================================

;;; RSS feeds (elfeed, replaces feed.nvim)
;;; All 14 feeds from nvim config ported with matching tags
(after! elfeed
  (setq elfeed-feeds
        '(;; Reddit feeds
          ("https://www.reddit.com/r/bash/.rss?sort=new" reddit tech)
          ("https://www.reddit.com/r/C_Programming/.rss?sort=new" reddit tech)
          ("https://www.reddit.com/r/Fedora/.rss?sort=new" reddit tech)
          ("https://www.reddit.com/r/Fire/.rss?sort=new" reddit finance)
          ("https://www.reddit.com/r/dividends/.rss?sort=new" reddit finance investing)
          ;; NewsMax feeds
          ("https://www.newsmax.com/rss/Newsfront/16/" news headlines)
          ("https://www.newsmax.com/rss/US/18/" news)
          ("https://www.newsmax.com/rss/Health-News/177/" news health)
          ("https://www.newsmax.com/rss/Companies/6/" news finance)
          ("https://www.newsmax.com/rss/InvestingAnalysis/17/" news finance investing)
          ("https://www.newsmax.com/rss/Economy/2/" news finance)
          ("https://www.newsmax.com/rss/FinanceNews/4/" news finance)
          ("https://www.newsmax.com/rss/Headline/76/" news finance headlines)
          ;; DailyWire
          ("https://www.dailywire.com/feeds/rss.xml" news))))

;; `o R' was previously bound to `elfeed-update' directly; clear it
;; with an explicit nil so Doom can rebuild it as a prefix.  Mirrors
;; the AI prefix pattern below (`SPC a' nil + flat "a c" sub-keys).
(map! :leader :desc "reddit/rss" "o R" nil)

(map! :leader
      :desc "Open RSS"         "o r"   #'elfeed
      :desc "Reddit main"      "o R r" #'reddigg-view-main
      :desc "Reddit subreddit" "o R s" #'reddigg-view-sub
      :desc "Reddit comments"  "o R c" #'reddigg-view-comments
      :desc "Update RSS"       "o R u" #'elfeed-update)

;;; Reddit (reddigg) -- org-mode-native browser
(use-package! reddigg
  :defer t
  :config
  (setq reddigg-subs
        '(bash C_Programming Fedora Fire dividends emacs))

  ;; Route reddit.com URLs from eww / elfeed / org links into reddigg
  ;; instead of rendering as a generic web page. Matches the existing
  ;; YouTube->mpv / PDF->pdf-tools pattern in eww.el.
  (defun zach/browse-url-reddit (url &rest _args)
    "Open reddit URL in reddigg.
Comment threads -> `reddigg-view-comments'; subreddit pages ->
`reddigg-view-sub'; anything else -> eww."
    (cond
     ((string-match "/r/[^/]+/comments/" url)
      (reddigg-view-comments url))
     ((string-match "/r/\\([^/?#]+\\)" url)
      (reddigg-view-sub (match-string 1 url)))
     (t (eww-browse-url url))))
  (function-put 'zach/browse-url-reddit 'browse-url-browser-kind 'internal)

  (add-to-list 'browse-url-handlers
               '("\\(www\\.\\|old\\.\\)?reddit\\.com"
                 . zach/browse-url-reddit)))

;;; AI/LLM (gptel, replaces avante.nvim)
;;; Three backends matching nvim config: ollama (default), openai, anthropic
(after! gptel
  ;; Default to Ollama with gemma3:12b
  (setq gptel-model 'gemma3:12b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "127.0.0.1:11434"
                        :models '(gemma3:12b)
                        :stream t))

  ;; OpenAI backend
  (gptel-make-openai "OpenAI"
    :key 'gptel-api-key
    :models '(gpt-4o)
    :stream t)

  ;; Anthropic backend
  (gptel-make-anthropic "Anthropic"
    :key 'gptel-api-key
    :models '(claude-sonnet-4-20250514)
    :stream t))

;;; AI prefix (SPC a)
(map! :leader :desc "AI" "a" nil)

;;; Claude Code CLI (runs `claude` as subprocess via vterm)
(use-package! claude-code
  :config
  (setq claude-code-terminal-backend 'vterm)
  (map! :leader
        :desc "Claude Code"        "a c" #'claude-code-start
        :desc "Claude send region" "a s" #'claude-code-send-region
        :desc "Claude send buffer" "a b" #'claude-code-send-buffer
        :desc "Claude fix error"   "a f" #'claude-code-send-flymake-to-claude))

;;; gptel rewrite & context (missing keybindings for existing features)
(map! :leader
      :desc "AI rewrite region" "a r" #'gptel-rewrite
      :desc "AI add context"    "a a" #'gptel-add
      :desc "AI menu"           "a m" #'gptel-menu
      :desc "AI chat"           "a g" #'gptel)

;;; Calendar keybind (calfw, replaces calendar.vim)
(map! :leader
      :desc "Calendar" "o c" #'+calendar/open-calendar)

;;; EPUB reader (nov.el, replaces epub.nvim)
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file
        (expand-file-name "nov-places" doom-profile-data-dir)))

;;; Debugger: GDB via dape for C development
(after! dape
  (map! :leader
        :desc "Debug"         "d d" #'dape
        :desc "Breakpoint"    "d b" #'dape-breakpoint-toggle
        :desc "Continue"      "d c" #'dape-continue
        :desc "Step over"     "d n" #'dape-next
        :desc "Step into"     "d i" #'dape-step-in
        :desc "Step out"      "d o" #'dape-step-out
        :desc "Quit debug"    "d q" #'dape-quit))

;;; Live markdown browser preview (replaces markdown-preview.nvim)
(use-package! grip-mode
  :after markdown-mode
  :config
  (setq grip-preview-use-webkit nil)
  (map! :map markdown-mode-map
        :localleader
        :desc "Live preview" "p" #'grip-mode))

;;; Code screenshots (replaces carbon-now.nvim)
(use-package! carbon-now-sh
  :defer t
  :config
  (map! :leader
        :desc "Carbon screenshot" "c s" #'carbon-now-sh))

;;; Pixel-perfect table alignment (replaces render-markdown.nvim table rendering)
;;; Aligns table columns visually using display properties — works even with
;;; variable-width fonts or CJK characters.
(use-package! valign
  :hook (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))  ;; render | as a continuous vertical bar

;;; Markdown rendering (replaces render-markdown.nvim + glow.nvim)
(after! markdown-mode
  ;; Display inline images on open (delayed to avoid breaking buffer display)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (run-with-idle-timer 0.5 nil #'markdown-display-inline-images)
              ;; Refresh images after save to pick up newly added image links
              (add-hook 'after-save-hook #'markdown-display-inline-images nil t)))
  (setq markdown-command "glow"
        ;; Scale headers like render-markdown.nvim
        markdown-header-scaling t
        markdown-header-scaling-values '(1.6 1.4 1.2 1.1 1.0 1.0)
        ;; Hide markup characters (* for bold, _ for italic, etc.)
        markdown-hide-markup t
        ;; Render bold/italic/code inline
        markdown-fontify-code-blocks-natively t
        ;; Enable wiki-style links
        markdown-enable-wiki-links t
        ;; Use checkboxes for lists
        markdown-list-indent-width 4
        ;; Display images inline
        markdown-display-remote-images t
        markdown-max-image-size '(800 . 600)
        ;; Render horizontal rules as a line across the buffer
        markdown-hr-display-char ?─)

  ;; Better header faces with catppuccin-friendly colors
  (custom-set-faces!
    '(markdown-header-face-1 :height 1.6 :weight bold)
    '(markdown-header-face-2 :height 1.4 :weight bold)
    '(markdown-header-face-3 :height 1.2 :weight bold)
    '(markdown-header-face-4 :height 1.1 :weight bold)
    ;; Style table faces to match catppuccin
    '(markdown-table-face :inherit fixed-pitch))

  ;; Handle #anchor fragment links (markdown-toc TOC entries)
  ;; markdown-mode silently drops fragment-only URLs, so intercept them
  ;; via the markdown-follow-link-functions hook before that happens.
  (defun zach-markdown-follow-anchor (url)
    "Jump to the heading matching a #fragment anchor URL.
Return t if handled, nil to fall through to default behaviour."
    (when (and url (string-prefix-p "#" url))
      (let* ((slug (substring url 1))
             (found nil))
        (save-excursion
          (goto-char (point-min))
          (while (and (not found)
                      (re-search-forward markdown-regex-header nil t))
            (let* ((heading (or (match-string-no-properties 1)
                                (match-string-no-properties 5)))
                   (heading-slug
                    (when heading
                      (thread-last heading
                        (downcase)
                        (replace-regexp-in-string "[^a-z0-9 -]" "")
                        (string-trim)
                        (replace-regexp-in-string " +" "-")))))
              (when (string= heading-slug slug)
                (setq found (point))))))
        (when found
          (goto-char found)
          (beginning-of-line)
          (recenter 4)
          t))))

  (add-hook 'markdown-follow-link-functions #'zach-markdown-follow-anchor)

  ;; RET follows links (TOC anchors, file links, URLs) in normal mode
  (map! :map markdown-mode-map
        :n "RET" #'markdown-follow-thing-at-point))


;;; EPUB reader (nov.el)
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))

;;; Calibre library browser
(use-package! calibredb
  :commands (calibredb)
  :config
  (setq calibredb-program "flatpak run --command=calibredb com.calibre_ebook.calibre"
        calibredb-root-dir "~/Documents/E-Books"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Documents/E-Books")))
  (map! :leader
        :desc "Calibre library" "o b" #'calibredb)
  (map! :map calibredb-search-mode-map
        :n "RET" #'calibredb-find-file
        :n "V"   #'calibredb-open-file-with-default-tool
        :n "v"   #'calibredb-view
        :n "?"   #'calibredb-dispatch
        :n "/"   #'calibredb-search-live-filter
        :n "r"   #'calibredb-search-refresh-and-clear-filter
        :n "m"   #'calibredb-mark-and-forward
        :n "u"   #'calibredb-unmark-and-forward
        :n "q"   #'calibredb-search-quit)
  (map! :map calibredb-show-mode-map
        :n "?"   #'calibredb-entry-dispatch
        :n "q"   #'calibredb-search-quit))


;;;; =========================================================================
;;;; Phase 6: Org-mode (full PARA knowledge base)
;;;; =========================================================================

;;; Doom capture file variables — org files live alongside PARA markdown
(setq +org-capture-todo-file "02_areas/org/todo.org"
      +org-capture-notes-file "02_areas/org/notes.org"
      +org-capture-journal-file "02_areas/org/journal.org"
      +org-capture-changelog-file "02_areas/org/changelog.org"
      +org-capture-projects-file "02_areas/org/projects.org")

(after! org
  ;; Agenda: recursively find .org files in inbox, projects, and areas
  ;; Cached at startup — use zach/refresh-org-agenda-files to rescan manually
  (defun zach/refresh-org-agenda-files ()
    "Recursively collect .org files from PARA agenda directories."
    (interactive)
    (setq org-agenda-files
          (apply #'append
                 (mapcar (lambda (d)
                           (let ((dir (expand-file-name d org-directory)))
                             (when (file-directory-p dir)
                               (directory-files-recursively dir "\\.org$"))))
                         '("00_inbox" "01_projects" "02_areas"))))
    (message "Agenda files: %d" (length org-agenda-files)))
  ;; Build cache once at startup (deferred so it doesn't block init)
  (run-with-idle-timer 2 nil #'zach/refresh-org-agenda-files)
  ;; Also refresh when saving any org file in the agenda dirs
  (add-hook 'after-save-hook
            (lambda ()
              (when (and (eq major-mode 'org-mode)
                         buffer-file-name
                         (string-prefix-p (expand-file-name org-directory)
                                          buffer-file-name)
                         (not (string-match-p "03_resources\\|04_archives"
                                              buffer-file-name)))
                (zach/refresh-org-agenda-files))))

  ;; Capture templates integrated with PARA
  (setq org-capture-templates
        '(("i" "Inbox" entry
           (file+headline "02_areas/org/todo.org" "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
           :prepend t)

          ("n" "Note" entry
           (file+headline "02_areas/org/notes.org" "Inbox")
           "* %U %?\n%i\n%a"
           :prepend t)

          ("m" "Meeting notes" entry
           (file+headline "02_areas/org/notes.org" "Meetings")
           "* %U Meeting: %?\n** Attendees\n- \n** Notes\n%i\n** Action Items\n- [ ] "
           :prepend t)

          ("c" "Code reference" entry
           (file+headline "02_areas/org/notes.org" "Code References")
           "* %?\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %a\n:END:\n#+begin_src %^{Language}\n%i\n#+end_src"
           :prepend t)

          ("p" "Project" entry
           (file+headline "02_areas/org/projects.org" "Active")
           "* PROJ %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
           :prepend t)))

  ;; Appearance: header scaling to match markdown rendering
  (custom-set-faces!
    '(org-level-1 :height 1.6 :weight bold)
    '(org-level-2 :height 1.4 :weight bold)
    '(org-level-3 :height 1.2 :weight bold)
    '(org-level-4 :height 1.1 :weight bold))

  ;; Indent
  (setq org-indent-indentation-per-level 4)

  ;; Tempo: <s TAB expands to src block, <q TAB to quote, etc.
  (require 'org-tempo)

  ;; Babel: languages relevant to the user's stack
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (C . t)
     (sql . t))))

;;; org-roam: bidirectional linking across entire PARA knowledge base
(after! org-roam
  (setq org-roam-directory (expand-file-name org-directory)
        org-roam-dailies-directory "02_areas/dailies/"
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t
        org-roam-file-exclude-regexp "03_resources/technical/docs/"))

;;; org-journal: replaces markdown journal at 02_areas/personal/journal/
(after! org-journal
  (setq org-journal-dir (expand-file-name "02_areas/personal/journal/" org-directory)
        org-journal-file-type 'daily
        org-journal-date-format "%Y-%m-%d"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-time-format "%H:%M"
        org-journal-carryover-items nil))

;;; org-noter: annotate PDFs/EPUBs with org notes
(after! org-noter
  (setq org-noter-notes-search-path
        (list (expand-file-name "02_areas/org/annotations/" org-directory))
        org-noter-auto-save-last-location t
        org-noter-separate-notes-from-heading t))

;;; org-super-agenda: group agenda views into sections
(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "In Progress"
           :todo "STRT"
           :order 1)
          (:name "Overdue"
           :deadline past
           :order 2)
          (:name "Due Today"
           :deadline today
           :order 3)
          (:name "Due Soon"
           :deadline future
           :order 4)
          (:name "Waiting"
           :todo "WAIT"
           :order 5)
          (:name "On Hold"
           :todo "HOLD"
           :order 6)
          (:name "Projects"
           :todo "PROJ"
           :order 7)
          (:name "Ideas"
           :todo "IDEA"
           :order 8)
          (:name "Backlog"
           :todo "TODO"
           :order 9)))
)

;;; org-ql: structured queries for org files
(use-package! org-ql
  :after org)

;;; org-transclusion: inline content from other org files
(use-package! org-transclusion
  :after org)

;;; org-timeblock: interactive multi-day timeblock view
(use-package! org-timeblock
  :after org
  :config
  (setq org-timeblock-scale-options nil)
  ;; Show DONE/KILL items so timeblock works like a full calendar.
  ;; The package hardcodes `org-entry-is-done-p' to skip done entries,
  ;; so we advise it to always return nil while gathering entries.
  (defadvice! +org-timeblock--show-done-a (orig-fn &rest args)
    :around #'org-timeblock-get-buffer-entries-all
    (cl-letf (((symbol-function 'org-entry-is-done-p) #'ignore))
      (apply orig-fn args)))
  ;; ─────────────────────────────────────────────────────────────────
  ;; Make the timeblock SVG respect the frame's `alpha-background'
  ;; ─────────────────────────────────────────────────────────────────
  ;;
  ;; Problem.  `org-timeblock-mode' renders its multi-day view as a
  ;; single inline SVG.  The rest of the frame is alpha-transparent
  ;; (see `alpha-background' setup earlier in this file), but the SVG
  ;; area shows up as an opaque matte rectangle — the image's
  ;; transparent pixels are being flattened against a solid colour
  ;; before display, so the compositor never gets per-pixel alpha to
  ;; show the desktop through.
  ;;
  ;; Why.  Emacs's C-level SVG loader (`svg_load_image' in image.c)
  ;; rasterises to a Cairo ARGB32 surface that is pre-filled with a
  ;; solid bg colour (from `:background' if supplied, else
  ;; `face-background', else white) before librsvg paints onto it.
  ;; There is no Lisp-reachable way to skip that pre-fill, so by the
  ;; time the pixmap reaches the compositor the alpha channel is
  ;; already baked opaque.  `alpha-background' only makes Emacs's own
  ;; bg draws transparent — it never touches image pixmaps.
  ;;
  ;; Workaround.  Use the display-time `:mask' property, which builds
  ;; a 1-bit clipping pixmap applied when the image is blit.  Where
  ;; mask = 0, Emacs doesn't write the image pixel — instead it leaves
  ;; whatever was drawn there (the line's face bg), which *is* subject
  ;; to `alpha-background' and therefore does composite through to the
  ;; desktop.  Three moving parts make this stable:
  ;;
  ;;   1. `svg-create' :filter-return — inject a theme-bg `<rect>' as
  ;;      the first child of every SVG built inside
  ;;      `org-timeblock-redraw-timeblocks', giving the rasterised
  ;;      pixmap a known uniform colour at every empty pixel.  Gated
  ;;      by a dynamic flag (see scope-injection below) so this only
  ;;      affects our SVG, not other callers of `svg-create'.
  ;;
  ;;   2. `org-timeblock-redraw-timeblocks' :around — binds the
  ;;      `zach/org-timeblock--injecting' dynamic flag while the
  ;;      redraw runs, so the svg-create filter knows to inject.
  ;;
  ;;   3. `svg-image' :around — adds `:mask (heuristic (R G B))' with
  ;;      the exact theme-bg colour to the image spec.  This is the
  ;;      single choke-point hit by BOTH the initial render (through
  ;;      `svg-insert-image') AND the cursor-move refresh (through
  ;;      `svg-possibly-update-image').  Advising here rather than at
  ;;      the redraw level is what makes the masking stable across
  ;;      cursor movement — a subtle trap discovered the hard way.
  ;;      Gated by identity check `(eq svg org-timeblock-svg)' so
  ;;      other SVGs in the frame are untouched.
  ;;
  ;; Caveat.  `:mask' is 1-bit — anti-aliased edges of geometry don't
  ;; exactly match theme-bg and stay opaque, giving a very faint halo
  ;; against dark themes.  Acceptable trade-off for the transparency.

  (defcustom zach/org-timeblock-theme-bg t
    "When non-nil, make `org-timeblock' SVG respect `alpha-background'.
Injects a theme-colored bg `<rect>' into the SVG and adds a
heuristic clipping mask at display time so transparent regions of
the grid pass the frame's background alpha through to the
compositor.  Toggle to nil to restore stock (opaque matte) rendering."
    :type 'boolean
    :group 'org-timeblock)

  (defvar zach/org-timeblock--injecting nil
    "Dynamic flag — non-nil only while `org-timeblock-redraw-timeblocks'
is running.  The `svg-create' :filter-return advice keys off this to
scope bg-rect injection to our SVG instead of every SVG Emacs ever
creates.")

  (defun zach/org-timeblock--theme-bg-color ()
    "Return the current theme background color string, e.g. \"#1e1e2e\".
Falls back to catppuccin-mocha base if the default face has no
explicit :background set."
    (or (face-attribute 'default :background nil t)
        "#1e1e2e"))

  (defun zach/org-timeblock--scope-injection (orig-fn &rest args)
    "Around-advice for `org-timeblock-redraw-timeblocks'.
Binds `zach/org-timeblock--injecting' while ORIG-FN runs so the
`svg-create' filter fires only for our SVG."
    (let ((zach/org-timeblock--injecting t))
      (apply orig-fn args)))

  (defun zach/org-timeblock--inject-bg-rect (svg)
    "Filter-return advice for `svg-create'.
When invoked inside `org-timeblock-redraw-timeblocks' (flag:
`zach/org-timeblock--injecting'), append a full-coverage theme-bg
`<rect>' as the first child of SVG so every pixel of the rasterized
image has a known uniform colour — the target colour for the
heuristic mask in step 3.  Returns SVG unchanged in every other
context."
    (when (and zach/org-timeblock--injecting
               zach/org-timeblock-theme-bg
               (consp svg))
      (dom-append-child
       svg
       (dom-node 'rect
                 `((x . "0") (y . "0")
                   (width  . "100%")
                   (height . "100%")
                   (fill   . ,(zach/org-timeblock--theme-bg-color))))))
    svg)

  (defun zach/org-timeblock--svg-image-with-mask (orig svg &rest props)
    "Around-advice for `svg-image'.
When SVG is our `org-timeblock-svg', inject
`:mask (heuristic (R G B))' with the theme-bg color into PROPS
before calling ORIG, so Emacs clips theme-bg pixels to transparent
at display time.  Gated by identity check on SVG so every other
caller of `svg-image' in the frame is untouched.  Fires on both
`svg-insert-image' (initial redraw) and `svg-possibly-update-image'
(cursor-move refresh) — advising at the redraw level alone would
leave cursor-move renders unmasked and cause the image to flash
back to matte on every movement."
    (if (and zach/org-timeblock-theme-bg
             (boundp 'org-timeblock-svg)
             org-timeblock-svg
             (eq svg org-timeblock-svg))
        (apply orig svg
               :mask (list 'heuristic
                           (color-values
                            (zach/org-timeblock--theme-bg-color)))
               props)
      (apply orig svg props)))

  (advice-add 'org-timeblock-redraw-timeblocks
              :around #'zach/org-timeblock--scope-injection)
  (advice-add 'svg-create
              :filter-return #'zach/org-timeblock--inject-bg-rect)
  (advice-add 'svg-image
              :around #'zach/org-timeblock--svg-image-with-mask)

  ;; ─────────────────────────────────────────────────────────────────
  ;; Kill the cursor in *org-timeblock*
  ;; ─────────────────────────────────────────────────────────────────
  ;;
  ;; Partner to the masking above.  Without this, the masked SVG
  ;; showed a bright-white accent when focused that vanished on blur.
  ;;
  ;; Why it happened.  `:mask' reveals whatever Emacs drew at that
  ;; pixel position *before* the image draw.  Normally that's the
  ;; line's face bg (alpha-transparent → desktop tint).  But when
  ;; point sits on the character carrying the SVG's `display'
  ;; property, Emacs additionally draws the text cursor AT FULL IMAGE
  ;; SIZE on top of the image, in the cursor face's bg color — a
  ;; rosewater `#f5e0dc' under catppuccin-mocha.  Via
  ;; `alpha-background' that reads as a bright-white accent.  Blur
  ;; stops the selected-window cursor draw, so the accent vanishes
  ;; and only the masked line bg is visible.
  ;;
  ;; Why the mode's own `(setq cursor-type nil)' wasn't enough.
  ;; `org-timeblock-mode' sets `cursor-type nil' in its body (line
  ;; 354 of the package).  But Evil's `evil-refresh-cursor' runs
  ;; right after the mode hook and unconditionally re-assigns
  ;; `cursor-type' from whichever `evil-<STATE>-state-cursor' matches
  ;; the buffer's current state.  For `special-mode'-derived buffers
  ;; Doom lands in `motion' state, whose default cursor is `hollow'.
  ;; So the mode's nil gets silently clobbered.
  ;;
  ;; Fix.  Zero every per-state cursor var buffer-locally and call
  ;; `evil-refresh-cursor' — that removes the rug Evil stands on,
  ;; not just the cursor that's currently on it.  Also defensively
  ;; turn off `hl-line-mode', which would leak its bg the same way
  ;; if some other config ever re-enables it in this buffer.

  (defun zach/org-timeblock--hide-cursor ()
    "Make no cursor of any kind draw in the current buffer.
Buffer-local: zeros `cursor-type' AND every `evil-<STATE>-state-cursor'
variable, then calls `evil-refresh-cursor' to apply immediately.
Also turns off `hl-line-mode' if active.  Required in *org-timeblock*
so Evil's cursor draw doesn't paint over the masked SVG with cursor
face bg; see the commentary above for the full rationale."
    (setq-local cursor-type nil)
    (dolist (var '(evil-normal-state-cursor
                   evil-motion-state-cursor
                   evil-insert-state-cursor
                   evil-visual-state-cursor
                   evil-replace-state-cursor
                   evil-operator-state-cursor
                   evil-emacs-state-cursor))
      (when (boundp var)
        (set (make-local-variable var) nil)))
    (when (fboundp 'evil-refresh-cursor)
      (evil-refresh-cursor))
    (when (bound-and-true-p hl-line-mode)
      (hl-line-mode -1)))

  (add-hook 'org-timeblock-mode-hook #'zach/org-timeblock--hide-cursor)

  ;; Mode hooks don't re-run on `SPC h r r' for already-live buffers,
  ;; so retroactively apply the fix to any *org-timeblock* that is
  ;; already open at config-load time.
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (with-current-buffer buf
                 (derived-mode-p 'org-timeblock-mode)))
      (with-current-buffer buf
        (zach/org-timeblock--hide-cursor))))
  ;; Help command: show keybinding cheatsheet in minibuffer
  (defun org-timeblock-help ()
    "Display org-timeblock Evil keybindings."
    (interactive)
    (let ((help-text
           (concat
            "org-timeblock keybindings:\n"
            "─── Navigation ───────────────────────────\n"
            "  j/k      block down/up    h/l    column left/right\n"
            "  H/L      prev/next day    J      jump to day\n"
            "─── Actions ──────────────────────────────\n"
            "  RET      go to task       go     go to (other window)\n"
            "  s        schedule         d      set duration\n"
            "  t        toggle TODO      a      new task\n"
            "  ci/co    clock in/out\n"
            "─── Marks ────────────────────────────────\n"
            "  m        mark block       u      unmark block\n"
            "  %        mark by regexp   U      unmark all\n"
            "─── View ─────────────────────────────────\n"
            "  v        switch scaling   V      change span\n"
            "  T        toggle list      gr     refresh\n"
            "  W        write/export     q      quit\n"
            "  C-s      save org files   ?      this help")))
      (with-current-buffer (get-buffer-create "*org-timeblock-help*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert help-text)
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer)
                        '(display-buffer-at-bottom . ((window-height . fit-window-to-buffer)))))))
  ;; Evil-friendly keybindings for the timeblock SVG view
  (evil-define-key* 'normal org-timeblock-mode-map
    ;; navigation: vim-style hjkl
    "j" #'org-timeblock-forward-block
    "k" #'org-timeblock-backward-block
    "h" #'org-timeblock-backward-column
    "l" #'org-timeblock-forward-column
    ;; day navigation
    "H" #'org-timeblock-day-earlier
    "L" #'org-timeblock-day-later
    ;; jump / goto
    "J" #'org-timeblock-jump-to-day
    (kbd "RET") #'org-timeblock-goto
    (kbd "TAB") #'org-timeblock-goto-other-window
    "go" #'org-timeblock-goto-other-window
    ;; actions
    "s" #'org-timeblock-schedule
    "d" #'org-timeblock-set-duration
    "t" #'org-timeblock-todo
    "a" #'org-timeblock-new-task
    ;; clock
    "ci" #'org-timeblock-clock-in
    "co" #'org-clock-out
    ;; marks
    "m" #'org-timeblock-mark-block
    "%" #'org-timeblock-mark-by-regexp
    "u" #'org-timeblock-unmark-block
    "U" #'org-timeblock-unmark-all-blocks
    ;; view
    "v" #'org-timeblock-switch-scaling
    "V" #'org-timeblock-change-span
    "T" #'org-timeblock-toggle-timeblock-list
    "gr" #'org-timeblock-redraw-buffers
    "W" #'org-timeblock-write
    "q" #'org-timeblock-quit
    "?" #'org-timeblock-help
    (kbd "C-s") #'org-save-all-org-buffers)
  ;; Evil-friendly keybindings for the timeblock list sidebar
  (evil-define-key* 'normal org-timeblock-list-mode-map
    "j" #'org-timeblock-list-next-line
    "k" #'org-timeblock-list-previous-line
    "H" #'org-timeblock-day-earlier
    "L" #'org-timeblock-day-later
    "J" #'org-timeblock-jump-to-day
    (kbd "RET") #'org-timeblock-list-goto
    (kbd "TAB") #'org-timeblock-list-goto-other-window
    "go" #'org-timeblock-list-goto-other-window
    "s" #'org-timeblock-list-schedule
    "d" #'org-timeblock-list-set-duration
    "t" #'org-timeblock-todo
    "a" #'org-timeblock-new-task
    "ci" #'org-timeblock-list-clock-in
    "co" #'org-clock-out
    "v" #'org-timeblock-switch-scaling
    "V" #'org-timeblock-change-span
    "T" #'org-timeblock-list-toggle-timeblock
    "gr" #'org-timeblock-redraw-buffers
    "q" #'org-timeblock-quit
    "?" #'org-timeblock-help
    (kbd "C-s") #'org-save-all-org-buffers))

;;; org-kanban: visual kanban board as org table
(use-package! org-kanban
  :after org)

;;; Org agenda: fix navigation on org-super-agenda header lines
;;; Super-agenda applies a text-property keymap on group headers that captures
;;; keys before Evil sees them. Override that keymap to use vim navigation.
(after! org-super-agenda
  (define-key org-super-agenda-header-map (kbd "j") #'org-agenda-next-line)
  (define-key org-super-agenda-header-map (kbd "k") #'org-agenda-previous-line)
  (define-key org-super-agenda-header-map (kbd "h") #'evil-backward-char)
  (define-key org-super-agenda-header-map (kbd "l") #'evil-forward-char))

;;; +org/insert-linked-note: Anytype-style create-and-link in PARA structure
(defun +org--create-and-link-note (dir)
  "Create a new org note in DIR and insert a link at point.
Auto-prefixes the filename with today's date when DIR contains
\"meeting\", \"journal\", or \"1on1\"."
  (let* ((title (read-string "Title: "))
         (slug (thread-last title
                 (downcase)
                 (replace-regexp-in-string "[^a-z0-9]+" "_")
                 (replace-regexp-in-string "\\`_\\|_\\'" "")))
         (date-prefix (format-time-string "%Y%m%d"))
         (auto-date-p (string-match-p "meeting\\|journal\\|1on1" (downcase dir)))
         (filename (concat (if auto-date-p (concat date-prefix "_") "") slug ".org"))
         (filepath (expand-file-name filename dir))
         (link-path (abbreviate-file-name filepath)))
    (when (file-exists-p filepath)
      (unless (y-or-n-p (format "%s already exists.  Link to it anyway?" filename))
        (user-error "Aborted")))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (file-exists-p filepath)
      (with-temp-file filepath
        (insert (format "#+title: %s\n#+author: %s\n#+startup: overview indent\n\n"
                        title user-full-name))))
    (insert (org-link-make-string (concat "file:" link-path) title))))

(defun +org/insert-linked-note ()
  "Create and link a note — browse PARA directories step by step."
  (interactive)
  (+org--create-and-link-note
   (read-directory-name "Location: " org-directory nil t)))

(defun +org/insert-linked-note-fuzzy ()
  "Create and link a note — fuzzy-search all PARA directories."
  (interactive)
  (let* ((root (expand-file-name org-directory))
         (dirs (mapcar
                (lambda (d) (file-relative-name d root))
                (seq-filter (lambda (d)
                              (and (file-directory-p d)
                                   (not (string-match-p "/\\.git\\(/\\|$\\)" d))))
                            (directory-files-recursively root "" t))))
         (pick (completing-read "Location (fuzzy): " (cons "./" dirs) nil nil)))
    (+org--create-and-link-note (expand-file-name pick root))))

;;; Org keybindings (extending Doom defaults)
;;; NOTE: org-transclusion bindings live in transclusion.el alongside markdown ones
(map! :leader
      :desc "Org QL search"       "n q" #'org-ql-search
      :desc "Org timeblock"       "n T" #'org-timeblock
      :desc "Org kanban"          "n k" #'org-kanban/initialize
      :desc "Insert linked note"  "n i" #'+org/insert-linked-note
      :desc "Insert linked (fuzzy)" "n I" #'+org/insert-linked-note-fuzzy)

;;; org-remark: universal annotation layer. `SPC n r' family covers
;;; every verb — mark (default + semantic pens), navigate, manage,
;;; open notes.  `g/R/b' are green/red/blue pens (confirmed / refute
;;; / question); uppercase R keeps lowercase `r' on "remove" which
;;; gets hit most often.  Module body lives in `org-remark.el'.
(map! :leader
      (:prefix ("n r" . "remark")
       :desc "Mark region (yellow)"   "m" #'org-remark-mark
       :desc "Mark whole line"        "l" #'org-remark-mark-line
       :desc "Mark green (confirmed)" "g" #'org-remark-mark-green
       :desc "Mark red (refute)"      "R" #'org-remark-mark-red
       :desc "Mark blue (question)"   "b" #'org-remark-mark-blue
       :desc "Mark important"         "!" #'org-remark-mark-important
       :desc "Open notes"             "o" #'org-remark-open
       :desc "View in sidebar"        "v" #'org-remark-view
       :desc "Next highlight"         "n" #'org-remark-view-next
       :desc "Prev highlight"         "p" #'org-remark-view-prev
       :desc "Remove (keep note)"     "r" #'org-remark-remove
       :desc "Delete (with note)"     "d" #'org-remark-delete
       :desc "Change pen"             "c" #'org-remark-change
       :desc "Toggle visibility"      "t" #'org-remark-toggle
       :desc "List all highlights"    "L" #'org-remark-list-highlights))


;;;; =========================================================================
;;;; Phase 5: Custom Modules
;;;; =========================================================================

;;; Load custom elisp modules (ported from nvim lua)
(load! "shell-runner")        ;; Shell command runner (from lua/custom/shell.lua)
(load! "transclusion")        ;; Transclusion system (from core/mappings.lua)
(load! "qbittorrent-webui")   ;; qBittorrent WebUI API client
(load! "qbittorrent-torrents");; qBittorrent torrents-list UI
(load! "jackett")             ;; Jackett torrent search client
(load! "arr")                 ;; Generic *arr REST client (Sonarr/Radarr/Lidarr/Readarr)
(load! "arr-queue")           ;; Unified *arr queue viewer
(load! "arr-search")          ;; Search-and-add across *arr services
(load! "eww")                 ;; Emacs-as-browser (eww + shr + URL routing)
(load! "org-remark")          ;; Universal annotation layer (all buffer types)

;;; Email: mu4e via Proton Mail Bridge (IMAP/SMTP on localhost)
;;; Maildir: ~/.local/share/mail/proton  (synced by mbsync)
;;; Sending: msmtp  (config at ~/.config/msmtp/config)
(when IS-MAC
  (when-let ((mu4e-dir (car (file-expand-wildcards "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"))))
    (add-to-list 'load-path mu4e-dir)))
(after! mu4e
  (setq mu4e-maildir (expand-file-name "~/.local/share/mail/proton")
        mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc proton"
        mu4e-update-interval (* 5 60)
        mu4e-change-filenames-when-moving t  ;; required for mbsync
        mu4e-use-fancy-chars t
        mu4e-view-show-images t
        mu4e-view-use-gnus t            ;; richer HTML rendering via gnus/shr
        shr-inhibit-images nil          ;; let shr fetch images
        gnus-blocked-images nil         ;; don't block remote images
        mu4e-view-show-addresses t
        mu4e-compose-format-flowed t
        mu4e-confirm-quit nil
        mu4e-attachment-dir "~/Downloads"
        ;; Identity
        user-mail-address "zach@podbielniak.com"
        user-full-name "Zach Podbielniak"
        ;; Folder mapping — Proton Bridge exposes standard IMAP folders
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent"
        mu4e-refile-folder "/Archive"
        mu4e-trash-folder  "/Trash"
        ;; Proton keeps a copy in Sent automatically — don't double-save
        mu4e-sent-messages-behavior 'delete
        ;; Sending via msmtp
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  ;; Quick bookmarks for mu4e main view
  (setq mu4e-bookmarks
        '((:name "Unread"    :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today"     :query "date:today..now"                  :key ?t)
          (:name "This week" :query "date:7d..now"                     :key ?w)
          (:name "Flagged"   :query "flag:flagged"                     :key ?f)))

  ;; Redefine the `trash` mark: move into the Trash maildir WITHOUT setting
  ;; the \Deleted (T) flag. mbsync's `Expunge Both` sees a new local file with
  ;; T set and no remote UID as "marked for deletion, never uploaded" and
  ;; silently expunges it locally — so the delete never reaches Proton. Moving
  ;; into /Trash is already the delete action on Proton; the T flag is
  ;; redundant and destructive here.
  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
          :prompt "dtrash"
          :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
          :action (lambda (docid msg target)
                    (mu4e--server-move docid
                                       (mu4e--mark-check-target target)
                                       "-N"))))

  ;; After executing marks (x), push the local changes back to the IMAP server
  ;; by running mbsync again. mbsync is bidirectional, so deletions/moves/flag
  ;; changes made locally propagate up to Proton.
  (advice-add 'mu4e-mark-execute-all :after
              (lambda (&rest _)
                (mu4e-update-mail-and-index t))))

;;; org-download: paste images from the Wayland clipboard.
;;; Default uses xclip which doesn't read Wayland clipboards — writes
;;; 0-byte files and you see a white box in place of the image.
(after! org-download
  (setq org-download-screenshot-method "wl-paste -t image/png > %s"))

;;; Ement.el: native Matrix client (E2EE via pantalaimon on localhost:8009)
(use-package! ement
  :config
  ;; setopt (not setq) so the defcustom :set hook adds kill-emacs-hook
  (setopt ement-save-sessions t)
  (setq plz-curl-program "/usr/bin/curl")
  ;; Guard against nil image metadata in encrypted rooms (pantalaimon
  ;; sometimes returns incomplete info dicts for media events)
  (defadvice! zach/ement--image-safe (fn event &rest args)
    :around #'ement-room--format-message-body
    (condition-case err
        (apply fn event args)
      (wrong-type-argument
       (propertize "[image unavailable]" 'face 'font-lock-comment-face))))
  ;; Connect through pantalaimon for E2EE support
  (defun zach/ement-connect ()
    "Connect to Matrix via pantalaimon proxy.
Restores saved session if available, otherwise prompts for login."
    (interactive)
    (if ement-sessions
        ;; Already connected — just reconnect the first session
        (ement-connect :session (cdar ement-sessions))
      ;; Try to restore saved session from disk
      (condition-case nil
          (let ((saved (ement--read-sessions)))
            (if saved
                (ement-connect :session (cdar saved))
              (error "No saved session")))
        (error
         ;; No saved session — fresh login through pantalaimon
         (let ((user-id (read-string "User ID: " nil 'ement-connect-user-id-history))
               (password (read-passwd "Password: ")))
           (ement-connect :uri-prefix "http://localhost:8009"
                          :user-id user-id
                          :password password))))))
  (map! :leader
        :desc "Matrix connect" "M M" #'zach/ement-connect
        :desc "Matrix rooms"   "M r" #'ement-room-list
        :desc "Matrix view"    "M t" #'ement-view-room
        :desc "Matrix disconnect" "M d" #'ement-disconnect))

;;; EMMS: MPD client for music playback
;;;
;;; MPD-only setup — no filesystem indexing.  `(emms-all)' would call
;;; `(emms-cache 1)' (on-disk cache + find-file hooks that read tags
;;; off the filesystem) and register `emms-info-native' /
;;; `emms-info-libtag' as info sources.  We skip both.  The in-memory
;;; emms-cache hash still exists — required for the browse-by-*
;;; commands — and can be populated from MPD on demand with
;;; `M-x emms-player-mpd-update-all-reset-cache' (no filesystem work:
;;; MPD sends its library metadata over the protocol).
(use-package! emms
  :config
  (emms-minimalistic)
  (require 'emms-playlist-mode)
  (require 'emms-info)
  (require 'emms-cache)
  (require 'emms-browser)
  (require 'emms-player-mpd)
  ;; Track-info + cache wiring (subset of what `emms-all' does).
  ;;
  ;; We DELIBERATELY DO NOT add `emms-info-initialize-track' to
  ;; `emms-track-initialize-functions'.  Upstream adds it, but for an
  ;; MPD-only setup it's a performance disaster:
  ;;   1. It calls `(file-attributes (emms-track-name track))' on
  ;;      every `emms-track' call — a filesystem stat per track,
  ;;      even though we want zero filesystem work.
  ;;   2. The stat usually returns nil (MPD's music_directory !=
  ;;      our path), so `info-mtime' never gets set, so on the next
  ;;      call it re-runs `emms-info-functions' (= `emms-info-mpd')
  ;;      without any pre-fetched info.  Every call to `emms-track'
  ;;      => a fresh MPD query.  For a large queue, that's thousands
  ;;      of redundant MPD round-trips on every playlist operation.
  ;;
  ;; Metadata is set explicitly by `emms-player-mpd-sync-from-mpd'
  ;; via `(emms-info-mpd track song-info)' WITH pre-fetched data, so
  ;; the init-hook re-query is pure waste.
  ;;
  ;; `(emms-cache 1)' enables the cache hash machinery (required by
  ;; `emms-cache-set-from-mpd-all').  It does NOT scan the
  ;; filesystem — only restores a previously saved snapshot and
  ;; persists on exit.
  (setq emms-playlist-default-major-mode #'emms-playlist-mode)
  (setq emms-track-description-function #'emms-info-track-description)
  (emms-cache 1)

  ;; Override `emms-player-mpd-start' so hitting RET on a track
  ;; simply tells MPD `play N' (N = that track's index in the
  ;; queue, counted from the EMMS playlist).  Upstream's
  ;; `start-and-sync-1' path decides, based on a
  ;; `buffer-modified-p' check that triggers on innocuous
  ;; interactions, to clear MPD's queue and re-add every EMMS
  ;; playlist track one by one via `sync-from-emms'.  For a queue
  ;; of any real size that's thousands of MPD commands plus
  ;; thousands of init-hook re-runs — the "spazz" the user sees.
  ;; We treat MPD's queue as authoritative (kept mirrored in the
  ;; EMMS playlist via `emms-player-mpd-connect'), so no sync is
  ;; needed on play.
  (defun zach/emms-mpd-start-by-position (_track)
    "Replacement for `emms-player-mpd-start'.
Play the currently-selected track by its position in the playlist,
without touching MPD's queue."
    (with-current-emms-playlist
      (save-excursion
        (goto-char (if (and emms-playlist-selected-marker
                            (marker-position emms-playlist-selected-marker))
                       emms-playlist-selected-marker
                     (point-min)))
        (let ((count 0))
          (condition-case nil
              (while t
                (emms-playlist-previous)
                (setq count (1+ count)))
            (error nil))
          (emms-player-mpd-play count)))))
  (advice-add 'emms-player-mpd-start
              :override #'zach/emms-mpd-start-by-position)
  ;; Disable modeline track display — causes UI lag with MPD polling
  (emms-mode-line-mode -1)
  (emms-playing-time-display-mode -1)
  ;; EMMS uses non-derived modes so Evil doesn't auto-detect them.
  ;; Bind directly on the mode keymaps with vim-style navigation.
  (evil-set-initial-state 'emms-browser-mode 'normal)
  (evil-set-initial-state 'emms-playlist-mode 'normal)

  (evil-define-key* 'normal emms-browser-mode-map
    "j"        #'next-line
    "k"        #'previous-line
    "h"        #'evil-backward-char
    "l"        #'evil-forward-char
    "q"        #'emms-browser-bury-buffer
    (kbd "RET") #'emms-browser-add-tracks-and-play
    (kbd "TAB") #'emms-browser-toggle-subitems
    (kbd "SPC") #'emms-browser-toggle-subitems
    "x"        #'emms-pause
    "X"        #'emms-stop
    "+"        #'emms-volume-raise
    "-"        #'emms-volume-lower
    "/"        #'emms-isearch-buffer
    "d"        #'emms-browser-view-in-dired
    "D"        #'emms-browser-delete-files
    "C"        #'emms-browser-clear-playlist
    "ga"       #'emms-browse-by-artist
    "gA"       #'emms-browse-by-album
    "gb"       #'emms-browse-by-genre
    "gy"       #'emms-browse-by-year)

  (evil-define-key* 'normal emms-playlist-mode-map
    "j"        #'next-line
    "k"        #'previous-line
    "h"        #'evil-backward-char
    "l"        #'evil-forward-char
    "q"        #'quit-window
    (kbd "RET") #'emms-playlist-mode-play-smart
    "d"        #'emms-playlist-mode-kill-track
    "x"        #'emms-pause
    "X"        #'emms-stop
    "+"        #'emms-volume-raise
    "-"        #'emms-volume-lower
    "r"        #'emms-random
    "s"        #'emms-shuffle)
  (setq emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info-mpd)
        emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory "~/Music"
        ;; Disable the 1 Hz MPD poll: the timer calls
        ;; `emms-player-mpd-detect-song-change' which (via its sync /
        ;; select-song paths) does `goto-char' + playlist rewrites
        ;; inside *EMMS Playlist*, making the buffer unusable while
        ;; a track is playing.  Trade-off: EMMS's "currently playing"
        ;; highlight won't auto-advance when MPD moves to the next
        ;; track — hit `SPC z l' (or any emms command) and it
        ;; re-reads MPD state, or set up mpc/mpris elsewhere.
        emms-player-mpd-check-interval nil)
  ;; Keep the original after-started hook as a safety net for
  ;; reconnects (idempotent).
  (add-hook 'emms-player-started-hook #'emms-player-mpd-connect)

  ;; One-shot initial connect.  Syncs MPD's current queue into the
  ;; EMMS playlist so `SPC z p' has a track to start with.  Runs on
  ;; a 2 s idle timer so Emacs startup isn't held if MPD is slow or
  ;; unreachable; errors are caught so the rest of config survives.
  ;;
  ;; We deliberately do NOT call `emms-cache-set-from-mpd-all' here:
  ;; it iterates every track in MPD's library (for a big library,
  ;; thousands of messages floods the echo area and triggers heavy
  ;; redisplay).  The browser works without a primed cache — the
  ;; first time it's opened after a fresh Emacs, run
  ;; `M-x emms-cache-set-from-mpd-all' manually, or bind it to a
  ;; key (e.g. `SPC z B' below for "Browser prime").
  (run-with-idle-timer
   2 nil
   (lambda ()
     (condition-case err
         (emms-player-mpd-connect)
       (error (message "EMMS: MPD initial connect failed: %s"
                       (error-message-string err))))))

  (map! :leader
        (:prefix ("z" . "music")
         :desc "Play/pause"     "p" #'emms-pause
         :desc "Stop"           "s" #'emms-stop
         :desc "Next track"     "n" #'emms-next
         :desc "Previous track" "N" #'emms-previous
         :desc "Playlist"       "l" #'emms-playlist-mode-go
         :desc "Browser"        "b" #'emms-browser
         :desc "Browser: prime from MPD" "B" #'emms-cache-set-from-mpd-all
         :desc "Volume up"      "+" #'emms-volume-raise
         :desc "Volume down"    "-" #'emms-volume-lower
         :desc "Shuffle"        "S" #'emms-shuffle
         :desc "Now playing"    "i" #'emms-show)))

;;; Jira issue tracker (jira.el) — requires Emacs 30+ (rx compat)
;;; Auth: store credentials in ~/.authinfo.gpg:
;;;   machine <instance>.atlassian.net login <email> port https password <api-token>
(use-package! jira
  :when (>= emacs-major-version 30)
  :config
  (setq jira-base-url "https://dt-rnd.atlassian.net"
        jira-api-version 3
        auth-sources '("~/.authinfo"))
  (map! :leader
        :desc "Jira issues" "J" #'jira-issues)
  ;; Evil overrides in jira-issues-mode
  (evil-define-key* 'normal jira-issues-mode-map
    (kbd "RET") (lambda () (interactive)
                  (jira-detail-show-issue (jira-utils-marked-item)))
    "go"        (lambda () (interactive)
                  (jira-actions-open-issue (jira-utils-marked-item)))
    "gf"        (lambda () (interactive)
                  (jira-detail-find-issue-by-key))
    "gc"        (lambda () (interactive)
                  (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item)))
    "gC"        #'jira-actions-change-issue-menu
    "gw"        #'jira-actions-add-worklog-menu
    "ge"        #'jira-export-menu
    "gl"        #'jira-issues-menu
    "gH"        #'jira-issues--switch-host-and-refresh
    "?"         (lambda () (interactive)
                  (with-current-buffer (get-buffer-create "*jira-keys*")
                    (erase-buffer)
                    (insert "Jira Issues Keybindings\n"
                            "=======================\n\n"
                            "RET  Open issue details\n"
                            "gl   Filter/search menu\n"
                            "gf   Find issue by key\n"
                            "go   Open in browser\n"
                            "gc   Copy issue ID\n"
                            "gC   Change issue status\n"
                            "gw   Add worklog\n"
                            "ge   Export menu\n"
                            "gH   Switch Jira host\n"
                            "?    This help\n")
                    (goto-char (point-min))
                    (special-mode))
                  (pop-to-buffer "*jira-keys*")))
  ;; Evil overrides in jira-detail-mode
  (evil-define-key* 'normal jira-detail-mode-map
    "+"         (lambda () (interactive) (jira-detail--add-comment))
    "-"         (lambda () (interactive) (jira-detail--remove-comment-at-point))
    "ge"        (lambda () (interactive) (jira-detail--edit-comment-at-point))
    "gC"        #'jira-actions-change-issue-menu
    "go"        (lambda () (interactive)
                  (jira-actions-open-issue jira-detail--current-key))
    "gU"        (lambda () (interactive) (jira-detail--update-field))
    "gf"        (lambda () (interactive) (jira-detail-find-issue-by-key))
    "gw"        (lambda () (interactive) (jira-detail--watchers-menu))
    "gP"        (lambda () (interactive) (jira-detail--show-parent-issue))
    "gS"        (lambda () (interactive) (jira-detail--create-subtask))
    "gc"        (lambda () (interactive)
                  (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key))
    "gr"        (lambda () (interactive)
                  (jira-detail-show-issue jira-detail--current-key))
    "?"         (lambda () (interactive)
                  (with-current-buffer (get-buffer-create "*jira-keys*")
                    (erase-buffer)
                    (insert "Jira Detail Keybindings\n"
                            "=======================\n\n"
                            "+    Add comment\n"
                            "-    Remove comment\n"
                            "ge   Edit comment\n"
                            "gC   Change status\n"
                            "gU   Update field\n"
                            "gf   Find issue by key\n"
                            "go   Open in browser\n"
                            "gc   Copy issue ID\n"
                            "gP   Show parent issue\n"
                            "gS   Create subtask\n"
                            "gr   Refresh\n"
                            "?    This help\n")
                    (goto-char (point-min))
                    (special-mode))
                  (pop-to-buffer "*jira-keys*"))))

;;; Torrenting — qbittorrent-transient + Jackett search
;;; Auth: store Jackett API key in ~/.authinfo (plain, not gpg):
;;;   machine nas-main_jackett login jackett password <api-key>
(use-package! qbittorrent-transient
  :commands (qbittorrent-transient
             qbittorrent-transient-url
             qbittorrent-transient-filepath
             qbittorrent-transient-dired)
  :init
  (map! :leader :desc "Torrents" "T" nil)
  (map! :leader
        :desc "qBittorrent menu"   "T t" #'qbittorrent-transient
        :desc "Add from Dired"     "T m" #'qbittorrent-transient-dired
        :desc "Open qBittorrent"   "T o" #'qbittorrent-transient-open))

;; Jackett search — defined in jackett.el, loaded above via (load! "jackett").
;; Configure host/port here (credentials stay in authinfo).
(with-eval-after-load 'jackett
  (setq jackett-host "nas-main"
        jackett-auth-host "nas-main_jackett"
        jackett-port 9117
        jackett-scheme "http"
        jackett-add-backend 'webui)
  (map! :leader
        :desc "Search Jackett"     "T s" #'jackett-search)
  ;; Evil normal-state overrides — otherwise RET/a/d/w just move the cursor.
  (evil-define-key* 'normal jackett-results-mode-map
    (kbd "RET") #'jackett-add-to-qbittorrent
    "a"         #'jackett-add-to-qbittorrent
    "d"         #'jackett-download-torrent-file
    "w"         #'jackett-copy-magnet
    "g"         #'jackett-refresh
    "q"         #'quit-window))

;; qBittorrent WebUI — points at the container on nas-main.
;; Authinfo: machine nas-main_qbittorrent login <user> password <pw>
(with-eval-after-load 'qbittorrent-webui
  (setq qbittorrent-webui-host "nas-main"
        qbittorrent-webui-auth-host "nas-main_qbittorrent"
        qbittorrent-webui-port 8580
        qbittorrent-webui-scheme "http"
        qbittorrent-webui-user "admin")
  (map! :leader
        :desc "qBittorrent login"  "T l" #'qbittorrent-webui-login
        :desc "Add magnet/URL"     "T u" #'qbittorrent-webui-add-url
        :desc "Add .torrent file"  "T f" #'qbittorrent-webui-add-file))

;;; *arr services (Sonarr/Radarr/Lidarr/Readarr) — see arr.el + arr-queue.el
;;; Auth: store each service's API key in ~/.authinfo:
;;;   machine <auth-host> login arr password <api-key>
;;; Add Readarr by appending another plist once configured.
(with-eval-after-load 'arr
  (setq arr-services
        '((:name sonarr :label "TV"
           :host "nas-main" :port 8989
           :auth-host "nas-main_sonarr"
           :api-path "/api/v3" :resource series)
          (:name radarr :label "Mov"
           :host "nas-main" :port 7878
           :auth-host "nas-main_radarr"
           :api-path "/api/v3" :resource movie)
          (:name lidarr :label "Msc"
           :host "nas-main" :port 8686
           :auth-host "nas-main_lidarr"
           :api-path "/api/v1" :resource artist)
          ;; Readarr — uncomment when configured:
          ;; (:name readarr :label "Bk"
          ;;  :host "nas-main" :port 8787
          ;;  :auth-host "nas-main_readarr"
          ;;  :api-path "/api/v1" :resource book)
          )))

;; SPC A prefix for *arr commands.
(map! :leader :desc "*arr" "A" nil)

(with-eval-after-load 'arr-queue
  (map! :leader
        :desc "Queue (all *arr)"   "A q" #'arr-queue
        :desc "Ping *arr"          "A p" #'arr-ping)
  ;; Evil normal-state overrides — otherwise letter keys just move the cursor.
  (evil-define-key* 'normal arr-queue-mode-map
    "g" #'arr-queue-refresh
    "d" #'arr-queue-remove-at-point
    "D" #'arr-queue-remove-and-blocklist-at-point
    "o" #'arr-queue-open-webui-at-point
    "w" #'arr-queue-copy-title
    "q" #'quit-window
    "?" #'arr-queue-show-keys))

(with-eval-after-load 'arr-search
  (map! :leader
        :desc "Search *arr"        "A s" #'arr-search)
  (evil-define-key* 'normal arr-search-mode-map
    "a" #'arr-search-add-at-point
    "g" #'arr-search-refresh
    "q" #'quit-window
    "?" #'arr-search-show-keys))

;;; Home Assistant (purplg/hass)
;;; Uses the $HASS_SERVER (URL or host:port) and $HASS_TOKEN environment
;;; variables.  Make sure both are exported in the Emacs process's env
;;; (e.g. in ~/.profile or the Doom launcher).

(defun zach/hass-parse-server (server)
  "Parse SERVER into a list (HOST PORT INSECURE-P).
Accepts \"host\", \"host:port\", \"http://host[:port]\", or
\"https://host[:port]\".  When no port is specified, defaults to 443
for https and 8123 for http (HA's native port)."
  (let (scheme host port)
    (cond
     ((string-match "^\\(https?\\)://\\([^:/]+\\)\\(?::\\([0-9]+\\)\\)?/?" server)
      (setq scheme (match-string 1 server)
            host   (match-string 2 server)
            port   (match-string 3 server)))
     ((string-match "^\\([^:/]+\\):\\([0-9]+\\)$" server)
      (setq scheme "http"
            host   (match-string 1 server)
            port   (match-string 2 server)))
     (t (setq scheme "http" host server)))
    (list host
          (cond
           (port                    (string-to-number port))
           ((equal scheme "https")  443)
           (t                       8123))
          (equal scheme "http"))))

(use-package! hass
  :defer t
  :init
  (let* ((server (or (getenv "HASS_SERVER") "localhost:8123"))
         (parsed (zach/hass-parse-server server)))
    (setq hass-host     (nth 0 parsed)
          hass-port     (nth 1 parsed)
          hass-insecure (nth 2 parsed)
          hass-apikey   (lambda () (getenv "HASS_TOKEN"))))
  ;; Dashboards — generated from /api/template scrape of entities+areas.
  ;; Two layouts:
  ;;   default — everything worth controlling, grouped by room
  ;;   quick   — flat list of the most-used lamps/lights only
  (setq hass-dash-layouts
        '((default .
            ((hass-dash-group
              :title "Home"
              :format "%t\n\n%v"

              (hass-dash-group
               :title "Basement — Lamps & Fans"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.b4_b0_24_0e_09_83" :label "Lamp 01")
               (hass-dash-toggle :entity-id "switch.b4_b0_24_0e_0d_c5" :label "Lamp 02")
               (hass-dash-toggle :entity-id "switch.54_af_97_f4_25_28" :label "Lamp 03")
               (hass-dash-toggle :entity-id "switch.28_87_ba_6a_2a_ef" :label "Lamp 04")
               (hass-dash-toggle :entity-id "switch.b4_b0_24_29_80_df" :label "Fan 01")
               (hass-dash-toggle :entity-id "switch.b4_b0_24_29_c2_2c" :label "Fan 02"))

              (hass-dash-group
               :title "Basement — Lights"
               :title-face outline-2
               (hass-dash-toggle :entity-id "light.1c_61_b4_64_ea_42" :label "Can (Left)")
               (hass-dash-toggle :entity-id "light.30_de_4b_77_19_54" :label "Can (Right)"))

              (hass-dash-group
               :title "Basement — Rooms"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.6c_5a_b0_9b_38_ae" :label "Storage Room")
               (hass-dash-toggle :entity-id "switch.d8_47_32_9e_e9_15" :label "Server Room"
                                 :confirm t))

              (hass-dash-group
               :title "Basement — Media"
               :title-face outline-2
               (hass-dash-state  :entity-id "media_player.fire_tv_10_0_4_141" :label "Fire TV")
               (hass-dash-state  :entity-id "media_player.kodi_186ab93026605a815ffe66304538e245" :label "Kodi")
               (hass-dash-state  :entity-id "media_player.zach_s_tv"   :label "TV")
               (hass-dash-state  :entity-id "media_player.zach_s_tv_2" :label "TV 2"))

              (hass-dash-group
               :title "Garage — Freezers"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.e4_fa_c4_9e_5b_5c"
                                 :label "Freezer Plug Group" :confirm t)
               (hass-dash-toggle :entity-id "switch.e4_fa_c4_9e_5b_5c_e4_fa_c4_9e_5b_5c_1"
                                 :label "Freezer 1" :confirm t)
               (hass-dash-toggle :entity-id "switch.e4_fa_c4_9e_5b_5c_e4_fa_c4_9e_5b_5c_2"
                                 :label "Freezer 2" :confirm t))

              (hass-dash-group
               :title "Cameras — Driveway"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.bullet_driveway_detections_motion" :label "Motion")
               (hass-dash-toggle :entity-id "switch.bullet_driveway_privacy_mode"      :label "Privacy"))

              (hass-dash-group
               :title "Cameras — Front Yard"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.bullet_frontyard_detections_motion" :label "Motion")
               (hass-dash-toggle :entity-id "switch.bullet_frontyard_privacy_mode"      :label "Privacy"))

              (hass-dash-group
               :title "Virtual"
               :title-face outline-2
               (hass-dash-toggle :entity-id "input_boolean.power_status" :label "Power Status")))))

          (quick .
            ((hass-dash-toggle :entity-id "switch.b4_b0_24_0e_09_83" :label "Lamp 01")
             (hass-dash-toggle :entity-id "switch.b4_b0_24_0e_0d_c5" :label "Lamp 02")
             (hass-dash-toggle :entity-id "switch.54_af_97_f4_25_28" :label "Lamp 03")
             (hass-dash-toggle :entity-id "switch.28_87_ba_6a_2a_ef" :label "Lamp 04")
             (hass-dash-toggle :entity-id "light.1c_61_b4_64_ea_42"  :label "Can (Left)")
             (hass-dash-toggle :entity-id "light.30_de_4b_77_19_54"  :label "Can (Right)")))))
  (map! :leader :desc "Home Assistant" "H" nil)
  (map! :leader
        :desc "HA dashboard"         "H d" #'hass-dash-open
        :desc "HA call service"      "H c" #'hass-call-service
        :desc "HA service+payload"   "H p" #'hass-call-service-with-payload
        :desc "HA ensure connection" "H e" #'hass-ensure))

;;; YouTube front-end (yeetube): search + play via mpv, download via yt-dlp.
(defun zach/yeetube-show-keys ()
  "Display a help buffer listing `yeetube-mode' keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*yeetube-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "yeetube Keybindings\n"
              "===================\n\n"
              "Playback:\n"
              "  RET    Play video at point (mpv)\n"
              "  r      Replay last video\n"
              "  p      Toggle mpv pause\n"
              "  v      Toggle video display in mpv\n"
              "  V      Toggle --no-video flag\n"
              "  C-q    Change mpv video quality\n\n"
              "Navigation / search:\n"
              "  M-RET  New search\n"
              "  C-RET  Open video/playlist page\n"
              "  b      Browse URL in external browser\n"
              "  c      Channel videos (for channel at point)\n"
              "  L      Channel live streams\n\n"
              "Saved / clipboard:\n"
              "  s      Save video to persistent list\n"
              "  P      Play a saved video\n"
              "  C      Copy URL to kill ring\n\n"
              "Download:\n"
              "  d      Download video (yt-dlp)\n"
              "  D      Change download directory\n"
              "  a      Change audio format\n\n"
              "Misc:\n"
              "  T      Toggle torsocks routing\n"
              "  S      Sort by column at point (tabulated-list)\n"
              "  q      Quit window\n"
              "  ?      This help\n"))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*yeetube-keys*"))

(use-package! yeetube
  :defer t
  :init
  (setq yeetube-results-limit        20
        yeetube-enable-emojis        nil
        yeetube-display-thumbnails-p t)
  (map! :leader :desc "YouTube" "Y" nil)
  (map! :leader
        :desc "Search YouTube"  "Y s" #'yeetube-search
        :desc "Saved videos"    "Y l" #'yeetube-play-saved-video
        :desc "Download dir"    "Y d" #'yeetube-download-change-directory)
  :config
  (keymap-set yeetube-mode-map "?" #'zach/yeetube-show-keys)
  ;; Evil normal-state overrides — otherwise RET / letter keys just move point.
  (evil-define-key* 'normal yeetube-mode-map
    (kbd "RET")   #'yeetube-play
    (kbd "M-RET") #'yeetube-search
    (kbd "C-<return>") #'yeetube-video-or-playlist-page
    (kbd "C-q")   #'yeetube-mpv-change-video-quality
    "b"  #'yeetube-browse-url
    "c"  #'yeetube-channel-videos
    "C"  #'yeetube-copy-url
    "d"  #'yeetube-download-video
    "D"  #'yeetube-download-change-directory
    "a"  #'yeetube-download-change-audio-format
    "p"  #'yeetube-mpv-toggle-pause
    "v"  #'yeetube-mpv-toggle-video
    "V"  #'yeetube-mpv-toggle-no-video-flag
    "s"  #'yeetube-save-video
    "L"  #'yeetube-channel-streams
    "P"  #'yeetube-play-saved-video
    "r"  #'yeetube-replay
    "T"  #'yeetube-mpv-toggle-torsocks
    "q"  #'quit-window
    "?"  #'zach/yeetube-show-keys))

;;; Jellyfin (emacs-os/jellyfin-emms-mpv.el): browse + play through EMMS/mpv.
;;; Server URL comes from $JELLYFIN_URL.
;;; Auth lives in ~/.authinfo keyed by the hostname inside $JELLYFIN_URL:
;;;   machine <host> login <user> password <pw>
;;; (If you use a non-443/80 port, add `port NNNN' to the machine line.)
(use-package! jellyfin-emms-mpv
  :defer t
  :init
  (setq jellyfin-server-url
          (let ((u (getenv "JELLYFIN_URL")))
            (and u (string-trim-right u "/")))
        jellyfin-completing-read-preview   t
        jellyfin-preferred-language        "eng"
        jellyfin-subtitles                 nil
        jellyfin-emms-cover-art            t
        jellyfin-elcava-emms-experimental  nil)
  (map! :leader :desc "Video (Jellyfin)" "V" nil)
  (map! :leader
        :desc "Movies"              "V m" #'jellyfin-browse-movies
        :desc "Movies gallery"      "V M" #'jellyfin-browse-movies-gallery
        :desc "TV shows"            "V t" #'jellyfin-browse-shows
        :desc "TV shows gallery"    "V T" #'jellyfin-browse-shows-gallery
        :desc "Continue watching"   "V c" #'jellyfin-browse-continue-watching
        :desc "Albums"              "V a" #'jellyfin-browse-albums
        :desc "Playlists"           "V p" #'jellyfin-browse-playlists
        :desc "Songs"               "V s" #'jellyfin-browse-songs))

;; Torrents list UI — live-updating table of torrents with actions.
(with-eval-after-load 'qbittorrent-torrents
  (map! :leader
        :desc "List torrents"      "T L" #'qbittorrent-torrents)
  ;; Evil normal-state overrides — otherwise letter keys just move the cursor.
  (evil-define-key* 'normal qbittorrent-torrents-mode-map
    "p" #'qbittorrent-torrents-pause-at-point
    "r" #'qbittorrent-torrents-resume-at-point
    "d" #'qbittorrent-torrents-delete-at-point
    "D" #'qbittorrent-torrents-delete-with-files-at-point
    "c" #'qbittorrent-torrents-recheck-at-point
    "w" #'qbittorrent-torrents-copy-hash
    "o" #'qbittorrent-torrents-open-webui
    "g" #'qbittorrent-torrents-refresh
    "q" #'quit-window
    "?" #'qbittorrent-torrents-show-keys))

;;; Git forge management (port of gitctl-nvim; backend: gitctl)
(use-package! gitctl-emacs
  :config
  (setq gitctl-cmd "gitctl"
        gitctl-confirm-actions t)
  (map! :leader
        :desc "Git forge"        "g G" #'gitctl-repos
        :desc "Git forge browse" "g B" #'gitctl-browse))

;;; Kanban/ticket management (port of vimban-nvim; replaces local vimban.el)
(use-package! vimban-emacs
  :config
  (setq vimban-cmd       "vimban"
        vimban-directory "~/Documents/notes")
  (map! :leader
        :desc "Tickets"       "v v" #'vimban-open
        :desc "Kanban board"  "v k" #'vimban-kanban
        :desc "New ticket"    "v n" #'vimban-new
        :desc "Search"        "v s" #'vimban-search
        :desc "Dashboard"     "v d" #'vimban-dashboard
        :desc "People"        "v p" #'vimban-people))

;;; Personal inventory tracker (port of possessions-nvim; backend: possessions)
(use-package! possessions-emacs
  :config
  (setq possessions-cmd "possessions")
  (map! :leader
        :desc "Possessions"  "P P" #'possessions-open
        :desc "Add item"     "P a" #'possessions-add
        :desc "Search items" "P s" #'possessions-search))

;;; GLib/GObject C language support (port of crispy-nvim; backend: crispy-language-server)
(use-package! crispy-emacs
  :config
  (setq crispy-auto-start t)
  ;; Auto-enable crispy-mode for C files in GLib projects (has Makefile/config.mk)
  (crispy-setup-auto-detect))

;;; Bacon .strip file support (port of bacon-nvim; backend: bacon-language-server)
(use-package! bacon-emacs
  :config
  (setq bacon-auto-start t))

;;; Podomation .pod DSL support (port of podomation-nvim; backend: podomation-language-server)
(use-package! podomation-emacs
  :config
  (setq podomation-auto-start t
        podomation-cmd '("podomation-language-server"
                         "--modules-path"
                         "/var/home/zach/source/projects/podomation/build/debug/modules")))

;;; monday.com client (GraphQL API)
(use-package! monday
  :commands (monday monday-boards)
  :config
  (map! :leader
        :desc "monday.com" "M" #'monday))


