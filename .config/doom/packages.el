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


;;; --- Dependencies ---

;; Emacs 29.x ships an older transient that lacks transient--set-layout.
;; Packages like claude-code and jira.el need the MELPA version.
(package! transient)


;;; --- Phase 1: Theme & UI ---

;; Catppuccin theme (standalone MELPA package, not in doom-themes)
(package! catppuccin-theme)

;; Color code highlighting (replaces nvim-colorizer.lua)
(package! rainbow-mode)

;; Per-column coloring for csv-mode buffers
(package! rainbow-csv
  :recipe (:host github :repo "emacs-vs/rainbow-csv"))

;; Browse kill-ring in a dedicated buffer
(package! browse-kill-ring)


;;; --- Phase 3: Navigation & Tools ---

;; Enhanced dired file manager with preview and multi-column layout
(package! dirvish)

;; Seamless Ctrl-h/j/k/l navigation between Emacs and tmux panes
;; (replaces christoomey/vim-tmux-navigator)
(package! navigate
  :recipe (:host github :repo "keith/evil-tmux-navigator"))

;; Justfile syntax highlighting (replaces vim-just)
(package! just-mode)


;;; --- Phase 4: Content & Media ---

;; EPUB reader (replaces epub.nvim)
(package! nov)

;; Calibre library browser
(package! calibredb)

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

;; Universal annotation layer: highlight regions in any buffer (text,
;; code, org, eww, nov, Info) and persist notes into aggregating .org
;; files under the PARA tree.
(package! org-remark)

;; Matrix chat client (native Emacs, E2EE via pantalaimon)
(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))

;; MPD client (music player daemon)
(package! emms)

;; Code screenshots (replaces carbon-now.nvim)
(package! carbon-now-sh)


;;; --- Jira ---

;; Jira issue tracker interface (list, view, edit, export issues)
;; jira.el uses rx syntax that requires Emacs 30+
(when (>= emacs-major-version 30)
  (package! jira
    :recipe (:host github :repo "unmonoqueteclea/jira.el")))


;;; --- Phase 6: Custom Emacs ports of nvim plugins ---

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

;; monday: monday.com client (GraphQL API)
(package! monday
  :recipe (:host nil
           :repo "https://git.podbielniak.com/zachpodbielniak/monday-emacs"))

;;; --- Torrenting ---

;; Transient menu wrapper around the qbittorrent CLI. Not on MELPA.
(package! qbittorrent-transient
  :recipe (:host github :repo "theobori/qbittorrent-transient"))

;;; --- Home Assistant ---

;; REST + WebSocket client for Home Assistant. On MELPA.
(package! hass)

;;; --- Media ---

;; YouTube front-end: search, play via mpv, download via yt-dlp. On MELPA.
(package! yeetube)

;; Jellyfin client via EMMS + mpv: browse movies, shows, music, playlists.
;; Not on MELPA — install direct from GitHub.
(package! jellyfin-emms-mpv
  :recipe (:host github :repo "emacs-os/jellyfin-emms-mpv.el"))
