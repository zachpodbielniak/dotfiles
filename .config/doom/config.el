;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Identity
(setq user-full-name "Zach Podbielniak")


;;;; =========================================================================
;;;; Phase 1: Foundation — Theme, UI, Core Editor
;;;; =========================================================================

;;; Theme: Catppuccin Mocha (replaces doom-one, matches nvim catppuccin)
(setq doom-theme 'catppuccin
      catppuccin-flavor 'mocha)

;;; Line numbers
(setq display-line-numbers-type t)

;;; Org directory
(setq org-directory "~/org/")


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
                           (other . "gnu"))))

;;; LSP keybindings (most already mapped by Doom Evil+eglot defaults)
(map! :leader
      :desc "Signature help" "l s" #'eglot-signature-help
      :desc "Buffer diagnostics" "l f" #'flymake-show-buffer-diagnostics)


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

;;; Calendar keybind (calfw, replaces calendar.vim)
(map! :leader
      :desc "Calendar" "o c" #'+calendar/open-calendar)

;;; EPUB reader (nov.el, replaces epub.nvim)
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file
        (expand-file-name "nov-places" doom-profile-data-dir)))

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
        markdown-max-image-size '(800 . 600))

  ;; Better header faces with catppuccin-friendly colors
  (custom-set-faces!
    '(markdown-header-face-1 :height 1.6 :weight bold)
    '(markdown-header-face-2 :height 1.4 :weight bold)
    '(markdown-header-face-3 :height 1.2 :weight bold)
    '(markdown-header-face-4 :height 1.1 :weight bold)))


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
