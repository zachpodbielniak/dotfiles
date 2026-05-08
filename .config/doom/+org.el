;;; +org.el -*- lexical-binding: t; -*-
;;
;; +org.el - Org-mode core: capture / roam / journal / agenda / kanban / remark
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
;; Phase 6 of the Doom config — the PARA knowledge-base subsystem.
;;
;; What this file holds:
;;   - Doom org-capture file variables (PARA paths)
;;   - `(after! org …)' core: agenda-files cache + auto-refresh,
;;     capture templates, header-face scaling, indent, tempo + babel.
;;   - `(after! org-roam …)' — point at ~/Documents/notes
;;   - `(after! org-journal …)' — daily files in 02_areas/personal/journal
;;   - `(after! org-noter …)' — annotations directory
;;   - `org-super-agenda' grouping
;;   - `org-ql' / `org-transclusion' / `org-kanban' deferred loaders
;;   - `org-super-agenda-header-map' Evil-style nav (j/k/h/l)
;;   - `+org/insert-linked-note' (Anytype-style create-and-link)
;;   - SPC n keybinds (search / timeblock / kanban / linked notes)
;;   - SPC n r org-remark prefix
;;   - org-download Wayland clipboard fix
;;
;; org-timeblock has its own file (`+org-timeblock.el') and must be
;; loaded after this one.

;;; Code:

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

;;; org-download: paste images from the Wayland clipboard.
;;; Default uses xclip which doesn't read Wayland clipboards — writes
;;; 0-byte files and you see a white box in place of the image.
(after! org-download
  (setq org-download-screenshot-method "wl-paste -t image/png > %s"))

(provide '+org)
;;; +org.el ends here
