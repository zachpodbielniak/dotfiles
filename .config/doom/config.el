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

;;; TRAMP performance / safety guards (see +tramp-perf.el)
(load! "+tramp-perf")

;;; Eshell performance tuning & TRAMP integration (see eshell.el)
(load! "eshell")

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
        :n "f"   #'dirvish-fd
        :n "N"   #'dired-create-empty-file
        :n "?"   #'dirvish-dispatch))

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

;;; Background handling (replaces transparent.nvim)
;;; GUI frames get a translucent background (alpha).  TTY frames instead
;;; clear their background faces so Emacs inherits the *terminal emulator's*
;;; own background.  Our terminal is already Catppuccin Mocha, and `-nw'
;;; Emacs can only 256-color-approximate the theme's 24-bit hex — the dark
;;; mocha base (#1e1e2e) collapses to a muddy dark blue.  Letting the real
;;; terminal background show through is an exact match for free.
;;;
;;; This MUST run per-frame, not just on `doom-load-theme-hook'.  We run
;;; `emacs --daemon' + `emacsclient -nw', so the theme loads exactly once
;;; (against a non-graphic pseudo-frame) and never reloads for the TTY
;;; frames created later — a load-time-only hook silently misses every
;;; `-nw' client.  Hence `after-make-frame-functions'.  And use the magic
;;; "unspecified-bg" TTY color, NOT nil: nil leaves `default' falling back
;;; to the (global) theme background instead of clearing it.
(defun zach/tty-inherit-terminal-bg (&optional frame)
  "Clear theme backgrounds on a TTY FRAME so it inherits the terminal's own.
Every face below carries a subtle Catppuccin *surface* color that `-nw'
Emacs can only 256-approximate — and they all collapse to the same muddy
blue.  Giving each the magic \"unspecified-bg\" value lets the real terminal
background (already Catppuccin Mocha) show through instead.

Frame-local, so GUI frames served by the same daemon keep the theme's
backgrounds.  No-op on graphic frames.  Callable interactively (`M-x') to
re-apply to the current frame after editing the face list / reloading.

Trim the list to keep any element's tint — e.g. drop `hl-line' if you'd
rather still see the current line highlighted (clearing it removes the
highlight entirely under `-nw', it doesn't soften it)."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (unless (display-graphic-p frame)
      (dolist (face '(;; editing surface (+ Doom's solaire variants for files)
                      default fringe
                      solaire-default-face solaire-fringe-face
                      solaire-hl-line-face
                      ;; line-number gutter
                      line-number line-number-current-line
                      ;; current-line highlight (clearing = no highlight)
                      hl-line
                      ;; the tab bar at the top.  `gt'/`gT' cycle the *native*
                      ;; tab-bar (we use `tab-bar-new-tab' in shell-runner.el);
                      ;; centaur-tabs faces are kept too in case `:ui tabs'
                      ;; renders its own bar — clearing unused faces is a no-op.
                      tab-bar tab-bar-tab tab-bar-tab-inactive
                      tab-bar-tab-group-current tab-bar-tab-group-inactive
                      tab-bar-tab-ungrouped
                      centaur-tabs-default
                      centaur-tabs-unselected centaur-tabs-selected
                      centaur-tabs-unselected-modified
                      centaur-tabs-selected-modified
                      centaur-tabs-close-unselected centaur-tabs-close-selected
                      centaur-tabs-modified-marker-unselected
                      centaur-tabs-modified-marker-selected
                      centaur-tabs-active-bar-face
                      ;; the statusbar (Doom `:ui modeline' = doom-modeline)
                      mode-line mode-line-active mode-line-inactive
                      doom-modeline-bar doom-modeline-bar-inactive
                      ;; org / markdown code + quote blocks
                      org-block org-block-begin-line org-block-end-line
                      org-quote org-code org-verbatim
                      markdown-code-face markdown-pre-face
                      markdown-inline-code-face markdown-blockquote-face
                      ;; org headings: indentation, folded-ellipsis, and tags
                      ;; (org-tag also covers tags in the agenda).  NB: this
                      ;; only helps if the blue is a *background*; if a tag/
                      ;; ellipsis is blue *text*, see the foreground note below.
                      org-indent org-hide org-ellipsis org-tag))
        (when (facep face)
          (set-face-background face "unspecified-bg" frame))))))

(add-hook 'doom-load-theme-hook       #'zach/tty-inherit-terminal-bg)
(add-hook 'after-make-frame-functions #'zach/tty-inherit-terminal-bg)
(add-hook 'window-setup-hook          #'zach/tty-inherit-terminal-bg)

;;; GUI: translucent frame background (alpha).  No effect on TTY frames.
(if (eq system-type 'darwin)
    ;; macOS NS port: alpha-background is not supported, use alpha instead.
    (progn
      (when (display-graphic-p) (set-frame-parameter nil 'alpha '(95 . 85)))
      (add-to-list 'default-frame-alist '(alpha . (85 . 75))))
  (when (display-graphic-p) (set-frame-parameter nil 'alpha-background 95))
  (add-to-list 'default-frame-alist '(alpha-background . 85)))

;; macOS native fullscreen creates a separate Space and drops transparency.
;; Use non-native fullscreen (maximized within current Space) instead.
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;;; Gowl compositor modules (only when running as Wayland compositor).
;;; IS-GOWL must precede `(load! "+gowl")' since the body of that file
;;; is gated on this constant.
(defconst IS-GOWL (and (fboundp 'gowl-running-p) (gowl-running-p)))
(load! "+gowl")

;;; CMacs feature detection — non-nil only on a cmacs build with the
;;; inline video (GStreamer) overlay feature compiled in.  Used to
;;; gate `(load! "cmacs")' and any future cmacs-only modules.
;;; `cmacs-video-open' is a C-level DEFUN present only when cmacs is
;;; built with the video feature, so its fboundp is the cleanest probe.
(defconst IS-CMACS (fboundp 'cmacs-video-open))

;;; Modeline (tmux-style status bar with catppuccin colors and icons)
(load! "+modeline")

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

;;; pgtk + Wayland clipboard: let wl-copy own CLIPBOARD, not Emacs.
;;;
;;; Two failure modes when Emacs (pgtk) owns CLIPBOARD itself:
;;;
;;;   1. Stale reads. The pgtk daemon doesn't always honor
;;;      `wl_data_source.cancelled' when another app copies, so Emacs
;;;      thinks it still owns CLIPBOARD and `gui-selection-value' returns
;;;      its own stale kill-ring head.
;;;
;;;   2. Self-deadlock. With `save-interprogram-paste-before-kill' on,
;;;      every kill triggers a clipboard READ. If Emacs is the current
;;;      owner, a synchronous `wl-paste' call hangs: the compositor
;;;      relays `wl_data_source.send' back to Emacs while Emacs's main
;;;      thread is blocked in read() waiting for wl-paste. The second
;;;      `x' wedges the editor. (Confirmed via gdb thread dump.)
;;;
;;; Fix: take Emacs out of the owner role. `select-enable-clipboard nil'
;;; stops `gui-select-text' from claiming CLIPBOARD; we then bridge
;;; explicitly via wl-copy/wl-paste, which daemonize and hold ownership
;;; out-of-process. wl-paste can always read because the owner is a
;;; separate process, not us.
;;;
;;; Writeup: ~/Documents/notes/03_resources/technical/emacs/clipboard_paste_broken_after_kill.org
(when (and (eq window-system 'pgtk)
           (executable-find "wl-copy")
           (executable-find "wl-paste"))
  (setq select-enable-clipboard nil
        select-enable-primary   nil
        save-interprogram-paste-before-kill t
        interprogram-cut-function
        (lambda (text)
          (let ((proc (make-process :name "wl-copy"
                                    :buffer nil
                                    :command '("wl-copy")
                                    :connection-type 'pipe
                                    :noquery t)))
            (process-send-string proc text)
            (process-send-eof proc)))
        interprogram-paste-function
        (lambda ()
          (with-temp-buffer
            (when (zerop (call-process "wl-paste" nil t nil "-n"))
              (let ((s (buffer-string)))
                (unless (string-empty-p s) s)))))))

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

;;; Tmux pane navigation (replaces vim-tmux-navigator).
;;; Seamless Ctrl-h/j/k/l across Emacs windows and tmux panes.
;;; Deferred — `tmux-navigate' is autoloaded; first C-h/j/k/l press loads it.
(use-package! navigate
  :defer t
  :commands (tmux-navigate))

(map! :nvm "C-h" (cmd! (tmux-navigate "left"))
      :nvm "C-j" (cmd! (tmux-navigate "down"))
      :nvm "C-k" (cmd! (tmux-navigate "up"))
      :nvm "C-l" (cmd! (tmux-navigate "right")))

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
      :desc "Browser (eww) here" "o B" #'zach/eww-here
      :desc "TRAMP dashboard"    "o D" #'tramp-dashboard)

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

(load! "+rss")            ;; elfeed feeds + reddigg URL handler
(load! "+ai")             ;; gptel + claude-code
(load! "+reading")        ;; calendar, EPUB, dape, grip, markdown, calibredb


;;;; =========================================================================
;;;; Phase 6: Org-mode (full PARA knowledge base)
;;;; =========================================================================

(load! "+org")            ;; capture / roam / journal / agenda / kanban / remark
(load! "+org-timeblock")  ;; multi-day timeblock view (depends on +org)


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
(load! "tramp-dashboard")     ;; Org-style dashboard for entering TRAMP systems
(load! "sf")                  ;; Salesforce Service Cloud client (case triage)
(load! "art-of-war")          ;; Art of War daily-study client (SPC s a)
(load! "container-registry-browse") ;; Container registry search + tag browse (SPC s c)
(load! "+games")              ;; Games launcher + built-in/3rd-party game bundle (SPC G)
(load! "emacslife/emacslife") ;; EmacsLife — BitLife clone (SPC G L)
(when IS-CMACS (load! "cmacs")) ;; RTSP/RTSPS camera dashboard (cmacs builds only; SPC o v)
(when IS-CMACS (load! "+cmacs-ai")) ;; per-provider model overrides (ollama → gemma4:26b)


;;;; =========================================================================
;;;; Apps / messaging / media
;;;; =========================================================================

;;; Email: mu4e via Proton Mail Bridge (IMAP/SMTP on localhost)
;;; Maildir: ~/.local/share/mail/proton  (synced by mbsync)
;;; Sending: msmtp  (config at ~/.config/msmtp/config)
;;;
;;; macOS-only load-path tweak — must run before mu4e is loaded by
;;; Doom's `:email mu4e' module.  Body lives in `+mu4e.el'.
(when IS-MAC
  (when-let ((mu4e-dir (car (file-expand-wildcards "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"))))
    (add-to-list 'load-path mu4e-dir)))
(load! "+mu4e")

(load! "+ement")          ;; Matrix client
(load! "+emms")           ;; EMMS / MPD music client
(load! "+jira")           ;; Jira issues + detail views


;;;; =========================================================================
;;;; Torrenting — qbittorrent-transient + Jackett + *arr glue
;;;; =========================================================================
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


;;;; =========================================================================
;;;; Smart home / video / music apps
;;;; =========================================================================

(load! "+home-assistant")     ;; Home Assistant (purplg/hass)
(load! "+yeetube")            ;; YouTube (yeetube + mpv + yt-dlp)

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


;;;; =========================================================================
;;;; Forge / kanban / inventory / language ports
;;;; =========================================================================

;;; Git forge management (port of gitctl-nvim; backend: gitctl).
;;; Deferred — loads on first `SPC g G' / `SPC g B'.
(use-package! gitctl-emacs
  :defer t
  :commands (gitctl-repos gitctl-browse)
  :init
  (setq gitctl-cmd "gitctl"
        gitctl-confirm-actions t))

(map! :leader
      :desc "Git forge"        "g G" #'gitctl-repos
      :desc "Git forge browse" "g B" #'gitctl-browse)

;;; Kanban/ticket management (port of vimban-nvim; replaces local vimban.el).
;;; Deferred — loads on first `SPC v X'.
(use-package! vimban-emacs
  :defer t
  :commands (vimban-open vimban-kanban vimban-new
             vimban-search vimban-dashboard vimban-people)
  :init
  (setq vimban-cmd       "vimban"
        vimban-directory "~/Documents/notes"))

(map! :leader
      :desc "Tickets"       "v v" #'vimban-open
      :desc "Kanban board"  "v k" #'vimban-kanban
      :desc "New ticket"    "v n" #'vimban-new
      :desc "Search"        "v s" #'vimban-search
      :desc "Dashboard"     "v d" #'vimban-dashboard
      :desc "People"        "v p" #'vimban-people)

;;; Personal inventory tracker (port of possessions-nvim; backend: possessions).
;;; Deferred — loads on first `SPC P X'.
(use-package! possessions-emacs
  :defer t
  :commands (possessions-open possessions-add possessions-search)
  :init
  (setq possessions-cmd "possessions"))

(map! :leader
      :desc "Possessions"  "P P" #'possessions-open
      :desc "Add item"     "P a" #'possessions-add
      :desc "Search items" "P s" #'possessions-search)

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

;;; monday.com client (GraphQL API).  Lives under `SPC o' ("open")
;;; next to jira and the other launch-style commands, freeing `SPC M'
;;; for the ement Matrix prefix.  Deferred via `:commands' — first
;;; `SPC o M' press loads the package.
(use-package! monday
  :defer t
  :commands (monday monday-boards))

(map! :leader
      :desc "monday.com" "o M" #'monday)

;;; ──────────────────────────────────────────────────────────────────
;;; Prefer fresh .el over stale .elc on load
;;; ──────────────────────────────────────────────────────────────────
;;;
;;; Default Emacs behaviour: when both `foo.el' and `foo.elc' exist,
;;; ALWAYS load `foo.elc' regardless of mtime.  That makes editing
;;; lisp/cmacs/*.el a foot-gun — Doom auto-byte-compiles in the
;;; background, your edits land in the .el, and you can spend an
;;; hour wondering why the new defcustom is "void variable" while
;;; the stale .elc shadows your fresh source.
;;;
;;; Flipping this makes Emacs compare mtimes; the .el wins when it's
;;; newer.  Cost: a stat() on each file load — invisible.
(setq load-prefer-newer t)

;;; ──────────────────────────────────────────────────────────────────
;;; cmacs dev tree takes precedence over system install
;;; ──────────────────────────────────────────────────────────────────
;;;
;;; The `make install' from 2026-04-26 dropped a system copy of
;;; cmacs's lisp/ into /usr/share/emacs/31.0.50/lisp/cmacs/.  That
;;; path is on `load-path' BEFORE the dev tree at
;;; ~/source/projects/cmacs/lisp/cmacs/, so every (require ...) and
;;; autoload pulls the months-old system version instead of whatever
;;; you're editing right now.  Symptom: edits to lisp/cmacs/*.el
;;; never take effect — even after wiping every .elc/.eln you can
;;; find — because the SYSTEM .elc is what's being loaded.
;;;
;;; Unconditionally move the dev tree to the front of load-path.
;;; `add-to-list' alone would be a no-op when the dir is already
;;; present further down the list, so we delete + cons.
(let ((dev "/var/home/zach/source/projects/cmacs/lisp/cmacs"))
  (when (file-directory-p dev)
    (setq load-path (cons dev (delete dev load-path)))))
