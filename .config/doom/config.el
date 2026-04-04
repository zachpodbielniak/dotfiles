;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "Zach Podbielniak")


;;;; =========================================================================
;;;; Phase 1: Foundation — Theme, UI, Core Editor
;;;; =========================================================================

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

;;; Org directory: point at PARA knowledge base root
(setq org-directory "~/Documents/notes/")

;;; Dired: show dotfiles and parent directory (..)
(setq dired-listing-switches "-ahl")

;;; wdired: bulk rename by editing filenames in dired
(after! dired
  (setq wdired-allow-to-change-permissions t
        wdired-allow-to-redirect-links t)
  (map! :map dired-mode-map
        :localleader
        "w" #'wdired-change-to-wdired-mode))


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
        doom-modeline-time-live-icon t
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
      :desc "Vterm popup" "o t" #'+vterm/toggle
      :desc "Vterm here" "o T" #'+vterm/here)

;;; Vterm TUI stability
(setq vterm-always-compile-module t)
(after! vterm
  ;; Process output faster — fewer visible intermediate redraw states
  (setq vterm-timer-delay 0.02))
(add-hook! 'vterm-mode-hook
  (display-line-numbers-mode -1))

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
        org-roam-dailies-directory "02_areas/personal/journal/"
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
  :after org)

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

;;; Org keybindings (extending Doom defaults)
;;; NOTE: org-transclusion bindings live in transclusion.el alongside markdown ones
(map! :leader
      :desc "Org QL search"       "n q" #'org-ql-search
      :desc "Org timeblock"       "n T" #'org-timeblock
      :desc "Org kanban"          "n k" #'org-kanban/initialize)


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
(use-package! emms
  :config
  (emms-all)
  (emms-default-players)
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
        emms-player-mpd-music-directory "~/Music")
  ;; Connect to MPD lazily — only when first used
  (add-hook 'emms-player-started-hook #'emms-player-mpd-connect)

  (map! :leader
        (:prefix ("z" . "music")
         :desc "Play/pause"     "p" #'emms-pause
         :desc "Stop"           "s" #'emms-stop
         :desc "Next track"     "n" #'emms-next
         :desc "Previous track" "N" #'emms-previous
         :desc "Playlist"       "l" #'emms-playlist-mode-go
         :desc "Browser"        "b" #'emms-browser
         :desc "Volume up"      "+" #'emms-volume-raise
         :desc "Volume down"    "-" #'emms-volume-lower
         :desc "Shuffle"        "S" #'emms-shuffle
         :desc "Now playing"    "i" #'emms-show)))

;;; Jira issue tracker (jira.el)
;;; Auth: store credentials in ~/.authinfo.gpg:
;;;   machine <instance>.atlassian.net login <email> port https password <api-token>
(use-package! jira
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


