;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "Zach Podbielniak")


;;;; =========================================================================
;;;; Phase 1: Foundation — Theme, UI, Core Editor
;;;; =========================================================================

;;; Font: Hack Nerd Font Mono (matching gst terminal config)
;;; Nerd Font covers modeline icons, Noto Color Emoji for emoji
(setq doom-font (font-spec :family "Hack Nerd Font Mono" :size 18)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :size 18)
      doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 24)
      doom-symbol-font (font-spec :family "Symbols Nerd Font Mono" :size 18)
      doom-emoji-font (font-spec :family "Noto Color Emoji" :size 18))

;;; Theme: Catppuccin Mocha (replaces doom-one, matches nvim catppuccin)
(setq doom-theme 'catppuccin
      catppuccin-flavor 'mocha)

;;; Line numbers
(setq display-line-numbers-type t)

;;; Org directory
(setq org-directory "~/org/")

;;; Dired: show dotfiles and parent directory (..)
(setq dired-listing-switches "-ahl")


;;; Indentation: tabs, 4 spaces width (matching nvim config)
(setq-default indent-tabs-mode t
              tab-width 4
              c-basic-offset 4)

;;; Transparent background (replaces transparent.nvim)
;;; GUI: set frame alpha; Terminal: clear face backgrounds
(if (display-graphic-p)
    (set-frame-parameter nil 'alpha-background 95)
  (add-hook 'doom-load-theme-hook
            (lambda ()
              (set-face-background 'default nil)
              (set-face-background 'fringe nil)
              (when (facep 'solaire-default-face)
                (set-face-background 'solaire-default-face nil)))))
;; Apply alpha to new frames too
(add-to-list 'default-frame-alist '(alpha-background . 85))

;;; Modeline (tmux-style status bar with catppuccin colors and icons)
(after! doom-modeline
  ;; Enable battery and time display
  (display-battery-mode 1)
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
        doom-modeline-time-live-icon t
        doom-modeline-battery t
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
  (setq zach-modeline--pomo-timer
        (run-with-timer 5 5 #'zach-modeline--update-pomo))

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

;;; Indent guides: highlight current depth (replaces snacks.nvim indent)
(after! indent-bars
  (setq indent-bars-highlight-current-depth t))

;;; Emoji picker keybind (replaces telescope_emoji.lua)
(map! :leader
      :desc "Insert emoji" "i e" #'emoji-search)

;;; Tab/Shift-Tab to cycle buffers (matching nvim tabufline)
(map! :n "TAB" #'next-buffer
      :n [backtab] #'previous-buffer)

;;; Clipboard paste (GTK GUI)
(map! "C-S-v" #'clipboard-yank)

;;; Sync all Evil yank/delete operations to system clipboard
(after! evil
  (setq evil-want-clipboard t))

;;; Fix "wrong type argument: plistp, t" on SPC p p project switch
;;; The workspace switch function hits a plistp error on first attempt when
;;; persp-mode returns t instead of a perspective struct. Wrap with retry.
(defadvice! +workspaces-switch-to-project-retry-a (fn &rest args)
  "Catch plistp error on first project switch and retry."
  :around #'+workspaces-switch-to-project-h
  (condition-case _err
      (apply fn args)
    (wrong-type-argument (apply fn args))))


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
  (require 'navigate))

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
      :desc "Vterm popup" "o t" #'+vterm/toggle
      :desc "Vterm here" "o T" #'+vterm/here)

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

(map! :leader
      :desc "Open RSS" "o r" #'elfeed
      :desc "Update RSS" "o R" #'elfeed-update)

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


;;;; =========================================================================
;;;; Phase 5: Custom Modules
;;;; =========================================================================

;;; Load custom elisp modules (ported from nvim lua)
(load! "shell-runner")  ;; Shell command runner (from lua/custom/shell.lua)
(load! "transclusion")  ;; Transclusion system (from core/mappings.lua)

;;; Email client (port of email-nvim; backend: email.c)
(use-package! email-emacs
  :config
  (setq email-cmd          "email.c"
        email-from         nil     ;; set to your address for reply-all filtering
        email-confirm-send   t
        email-confirm-delete t)
  (map! :leader
        :desc "Email inbox"   "m m" #'email-open
        :desc "Email compose" "m c" #'email-compose
        :desc "Email folders" "m f" #'email-folders
        :desc "Email search"  "m s" #'email-search))

;;; Matrix chat (port of matrix-nvim; backend: matrixctl)
(use-package! matrix-emacs
  :config
  (setq matrix-cmd "matrixctl")
  (map! :leader
        :desc "Matrix open"  "M M" #'matrix-open
        :desc "Matrix rooms" "M r" #'matrix-rooms
        :desc "Matrix tail"  "M t" #'matrix-tail))

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
