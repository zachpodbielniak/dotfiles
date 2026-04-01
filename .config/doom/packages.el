;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package:
;;
;;   1. Declare them here in a `package!' statement,
;;   2. Run 'doom sync' in the shell,
;;   3. Restart Emacs.
;;
;; Use 'C-h f package\!' to look up documentation for the `package!' macro.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)


;;; --- Phase 1: Theme & UI ---

;; Catppuccin theme (standalone MELPA package, not in doom-themes)
(package! catppuccin-theme)

;; Color code highlighting (replaces nvim-colorizer.lua)
(package! rainbow-mode)


;;; --- Phase 3: Navigation & Tools ---

;; Seamless Ctrl-h/j/k/l navigation between Emacs and tmux panes
;; (replaces christoomey/vim-tmux-navigator)
(package! navigate
  :recipe (:host github :repo "keith/evil-tmux-navigator"))

;; Justfile syntax highlighting (replaces vim-just)
(package! just-mode)


;;; --- Phase 4: Content & Media ---

;; EPUB reader (replaces epub.nvim)
(package! nov)

;; Live markdown browser preview (replaces markdown-preview.nvim)
(package! grip-mode)

;; Table of contents generation for markdown
(package! markdown-toc)

;; Pixel-perfect table alignment in-buffer (renders tables cleanly)
(package! valign)

;; Claude Code CLI integration (runs `claude` as subprocess in Emacs)
(package! claude-code)

;;; --- Phase 6: Org-mode ---

;; Group agenda items into named sections
(package! org-super-agenda)

;; Structured query language for org headings
(package! org-ql)

;; Inline content from other org files (replaces transclusion.el)
(package! org-transclusion)

;; Interactive multi-day timeblock view for scheduled tasks
(package! org-timeblock)

;; Visual kanban board rendered as org table
(package! org-kanban)

;; Code screenshots (replaces carbon-now.nvim)
(package! carbon-now-sh)


;;; --- Phase 5: Email ---

;; email-emacs: buffer-based email client using email.c backend (port of email-nvim)
(package! email-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/email-emacs"))


;;; --- Phase 6: Custom Emacs ports of nvim plugins ---

;; matrix-emacs: Matrix chat client (port of matrix-nvim)
(package! matrix-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/matrix-emacs"))

;; gitctl-emacs: Git forge management UI (port of gitctl-nvim)
(package! gitctl-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/gitctl-emacs"))

;; vimban-emacs: Kanban/ticket management (port of vimban-nvim)
(package! vimban-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/vimban-emacs"))

;; possessions-emacs: Personal inventory tracker (port of possessions-nvim)
(package! possessions-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/possessions-emacs"))

;; crispy-emacs: GLib/GObject C language support (port of crispy-nvim)
(package! crispy-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/crispy-emacs"))

;; bacon-emacs: Bacon .strip file major mode + LSP (port of bacon-nvim)
(package! bacon-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/bacon-emacs"))

;; podomation-emacs: Podomation .pod DSL major mode + LSP (port of podomation-nvim)
(package! podomation-emacs
  :recipe (:type git
           :host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/podomation-emacs"))
