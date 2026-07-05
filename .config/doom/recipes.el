;;; recipes.el --- Recipe manager on top of org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zach Podbielniak
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A small recipe-management client for the PARA notes tree.  Recipes are
;; plain org files living (recursively, in whatever subfolder hierarchy you
;; like) under `recipes-subdir' of `recipes-notes-dir' — by default
;; ~/Documents/notes/03_resources/food_and_health/recipes/.
;;
;; Each recipe is also an org-roam node: it carries an `:ID:' plus a
;; `#+title:' and `#+filetags:' (always including `:recipe:', with diet and
;; category tags alongside).  So they surface in `org-roam-node-find', gain
;; backlinks, and are queryable — while this module adds a purpose-built
;; browser and a transient capture form on top.
;;
;; Entry points:
;;   recipes           — the tabulated-list browser (SPC o k)
;;   recipes-capture   — transient "new recipe" form (SPC o K, and `c' in the
;;                       browser)
;;   recipes-find-roam — org-roam-node-find filtered to the :recipe: tag
;;
;; In the browser (`recipes-mode') — `?' opens a transient with everything:
;;   RET open · v cook-view · c new · I AI-import · g refresh
;;   f filter-tag · V views · L sort-by-last-made · / search
;;   R rate · X made-today · N AI-macros · p photo · B backlinks
;;   m/u/U mark · b shopping-list · s schedule · e export · a assistant · q quit
;;
;; Metadata lives in the file-level property drawer (SERVINGS, PREP, COOK,
;; RATING, SOURCE, LAST_MADE) and in `#+filetags'.  Files that predate this
;; schema still list — title falls back to the first headline or the
;; filename, and missing metadata renders blank.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'org)
(require 'org-id)
(require 'transient)

;; Forward declarations to silence byte-compile free-variable/function
;; warnings for symbols owned by packages loaded lazily.
(declare-function org-roam-node-find "org-roam-node")
(declare-function org-roam-node-tags "org-roam-node")
(declare-function org-roam-db-update-file "org-roam-db")
(declare-function consult-ripgrep "consult")

(defgroup recipes nil
  "Recipe manager on top of org-roam."
  :group 'applications
  :prefix "recipes-")

(defcustom recipes-notes-dir (expand-file-name "~/Documents/notes")
  "Root of the PARA notes tree.
The recipe collection lives at `recipes-subdir' under this directory.
`~' is expanded via `expand-file-name', so this follows $HOME on any
machine without hardcoding an absolute path."
  :type 'directory
  :group 'recipes)

(defcustom recipes-subdir "03_resources/food_and_health/recipes/"
  "Recipe collection directory, relative to `recipes-notes-dir'.
Searched recursively, so recipes may be organised into arbitrary
subfolders (e.g. `desserts/', `carnivore/breakfast/')."
  :type 'string
  :group 'recipes)

(defcustom recipes-index-regexp "\\(?:^\\|/\\)0*_?index\\.org\\'"
  "Files matching this regexp are treated as section indexes, not recipes."
  :type 'regexp
  :group 'recipes)

;;; ------------------------------------------------------ faces (catppuccin-mocha)

(defface recipes-rating-face
  '((t (:foreground "#f9e2af")))
  "Face for the star rating column."
  :group 'recipes)

(defface recipes-tag-face
  '((t (:foreground "#cba6f7")))
  "Face for the tags column."
  :group 'recipes)

(defface recipes-folder-face
  '((t (:foreground "#6c7086" :slant italic)))
  "Face for the subfolder column."
  :group 'recipes)

(defface recipes-cook-title-face
  '((t (:foreground "#cba6f7" :weight bold :height 1.7)))
  "Face for the recipe title in cook mode."
  :group 'recipes)

(defface recipes-cook-heading-face
  '((t (:foreground "#f9e2af" :weight bold :height 1.25)))
  "Face for section headings in cook mode."
  :group 'recipes)

(defface recipes-cook-text-face
  '((t (:foreground "#cdd6f4" :height 1.15)))
  "Face for body text in cook mode."
  :group 'recipes)

;;; ------------------------------------------------------ paths

(defun recipes--dir ()
  "Absolute path of the recipe collection directory."
  (file-name-as-directory (expand-file-name recipes-subdir recipes-notes-dir)))

;;; ------------------------------------------------------ strings

(defun recipes--humanize (slug)
  "Turn a file SLUG (e.g. \"carnivore_cheesecake\") into a Title Case string."
  (capitalize (replace-regexp-in-string "[_-]+" " " slug)))

(defun recipes--slugify (title)
  "Turn TITLE into a snake_case filename base."
  (let* ((s (downcase (string-trim title)))
         (s (replace-regexp-in-string "['\"]" "" s))
         (s (replace-regexp-in-string "[^a-z0-9]+" "_" s))
         (s (replace-regexp-in-string "\\`_+\\|_+\\'" "" s)))
    (if (string-empty-p s) "recipe" s)))

(defun recipes--stars (rating)
  "Render RATING (a string like \"4\", or nil) as filled/empty stars."
  (let ((n (and rating (ignore-errors (round (string-to-number rating))))))
    (if (and n (> n 0))
        (let ((n (min 5 (max 0 n))))
          (concat (make-string n ?★) (make-string (- 5 n) ?☆)))
      "")))

(defun recipes--count-char (s ch)
  "Count occurrences of character CH in string S."
  (let ((c 0))
    (mapc (lambda (x) (when (eq x ch) (setq c (1+ c)))) (append s nil))
    c))

;;; ------------------------------------------------------ discovery + parsing

(defun recipes--files ()
  "List every recipe .org file under `recipes--dir', recursively.
Section-index files (see `recipes-index-regexp') are excluded."
  (let ((root (recipes--dir)))
    (when (file-directory-p root)
      (seq-remove (lambda (f) (string-match-p recipes-index-regexp f))
                  (directory-files-recursively root "\\.org\\'")))))

(defun recipes--subdirs ()
  "List subfolders under `recipes--dir' as paths relative to it."
  (let ((root (recipes--dir)))
    (when (file-directory-p root)
      (mapcar (lambda (d) (file-relative-name d root))
              (seq-filter #'file-directory-p
                          (directory-files-recursively root "" t))))))

(defun recipes--file-metadata (file)
  "Parse FILE and return a plist describing the recipe.
Keys: :file :title :tags :rating :servings :prep :cook :last-made :folder.
Tolerant of files that predate the metadata schema."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((case-fold-search t)
           (limit (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward "^\\*+ " nil t)
                        (match-beginning 0)
                      (point-max))))
           (kw (lambda (name)
                 (goto-char (point-min))
                 (when (re-search-forward
                        (format "^#\\+%s:[ \t]*\\(.*\\)$" name) nil t)
                   (let ((v (string-trim (match-string 1))))
                     (unless (string-empty-p v) v)))))
           (prop (lambda (name)
                   (goto-char (point-min))
                   (when (re-search-forward
                          (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" name) limit t)
                     (let ((v (string-trim (match-string 1))))
                       (unless (string-empty-p v) v)))))
           (title (or (funcall kw "title")
                      (save-excursion
                        (goto-char (point-min))
                        (when (re-search-forward "^\\*+ +\\(.*\\)$" nil t)
                          (string-trim (match-string 1))))
                      (recipes--humanize (file-name-base file))))
           (ftags (funcall kw "filetags"))
           (tags (when ftags
                   (seq-remove (lambda (s)
                                 (or (string-empty-p s) (string= s "recipe")))
                               (split-string ftags ":" t "[ \t]*"))))
           (rel (file-relative-name (file-name-directory file) (recipes--dir)))
           (folder (if (member rel '("./" "." "")) ""
                     (directory-file-name rel))))
      (list :file file
            :title title
            :tags tags
            :rating (funcall prop "RATING")
            :servings (funcall prop "SERVINGS")
            :prep (funcall prop "PREP")
            :cook (funcall prop "COOK")
            :last-made (funcall prop "LAST_MADE")
            :folder folder))))

(defun recipes--file-prop (file key)
  "Read a single file-level property KEY from FILE, or nil."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((limit (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "^\\*+ " nil t)
                       (match-beginning 0)
                     (point-max)))))
      (goto-char (point-min))
      (when (re-search-forward
             (format "^[ \t]*:%s:[ \t]*\\(.*\\)$" (regexp-quote key)) limit t)
        (let ((v (string-trim (match-string 1))))
          (unless (string-empty-p v) v))))))

(defun recipes--all-tags ()
  "Sorted, de-duplicated list of every tag used across all recipes."
  (seq-sort #'string<
            (seq-uniq
             (seq-mapcat (lambda (f) (plist-get (recipes--file-metadata f) :tags))
                         (recipes--files)))))

;;; ------------------------------------------------------ writing metadata

(defun recipes--set-file-property (file key value)
  "Set file-level property KEY to VALUE in FILE's top property drawer.
Creates the drawer at the very top of the file if none exists (which is
where org-roam expects the file-level drawer to live, before `#+title')."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (save-excursion
        (let* ((key  (upcase key))
               (line (format ":%s: %s" key value))
               (limit (save-excursion
                        (goto-char (point-min))
                        (if (re-search-forward "^\\*+ " nil t)
                            (match-beginning 0)
                          (point-max)))))
          (goto-char (point-min))
          (cond
           ;; Existing top-level property drawer.
           ((re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" limit t)
            (let ((drawer-end (save-excursion
                                (and (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)
                                     (match-beginning 0)))))
              (unless drawer-end
                (user-error "Malformed property drawer in %s" file))
              (forward-line 1)
              (if (re-search-forward
                   (format "^[ \t]*:%s:.*$" (regexp-quote key)) drawer-end t)
                  (replace-match line t t)
                (goto-char drawer-end)
                (beginning-of-line)
                (insert line "\n"))))
           ;; No drawer: create one at the top of the file.
           (t
            (goto-char (point-min))
            (insert ":PROPERTIES:\n" line "\n:END:\n"))))))
    (save-buffer)))

(defun recipes--set-filetags (file tags)
  "Rewrite FILE's `#+filetags:' line to TAGS (a list of strings).
`recipe' is always included first.  If no `#+filetags:' line exists, one is
inserted after `#+title:' (or at the top of the file)."
  (with-current-buffer (find-file-noselect file)
    (save-restriction
      (widen)
      (save-excursion
        (let* ((case-fold-search t)
               (clean (seq-remove (lambda (x) (or (string-empty-p x)
                                                  (string= (downcase x) "recipe")))
                                  tags))
               (all (cons "recipe"
                          (mapcar (lambda (s) (replace-regexp-in-string "[ \t]+" "_" s))
                                  clean)))
               (line (format "#+filetags: :%s:" (mapconcat #'identity all ":"))))
          (goto-char (point-min))
          (if (re-search-forward "^#\\+filetags:.*$" nil t)
              (replace-match line t t)
            (goto-char (point-min))
            (if (re-search-forward "^#\\+title:.*$" nil t)
                (progn (end-of-line) (insert "\n" line))
              (insert line "\n"))))))
    (save-buffer)))

;;; ------------------------------------------------------ browser mode

(defvar-local recipes--tag-filter nil
  "When non-nil, the browser shows only recipes carrying this tag.")

(defvar-local recipes--filter-fn nil
  "Optional extra predicate on a recipe metadata plist for view filters.")

(defvar-local recipes--filter-label nil
  "Human-readable label for the active `recipes--filter-fn' view.")

(defvar-local recipes--marked nil
  "List of file-path ids marked for batch actions in the browser.")

(defun recipes--entry-vector (m)
  "Build the tabulated-list column vector for metadata plist M."
  (vector
   (or (plist-get m :title) "")
   (propertize (recipes--stars (plist-get m :rating)) 'face 'recipes-rating-face)
   (propertize (mapconcat #'identity (plist-get m :tags) " ") 'face 'recipes-tag-face)
   (or (plist-get m :servings) "")
   (or (plist-get m :cook) "")
   (or (plist-get m :last-made) "")
   (propertize (or (plist-get m :folder) "") 'face 'recipes-folder-face)))

(defun recipes--collect-entries ()
  "Return tabulated-list entries for every recipe (honouring the tag filter)."
  (let ((filter recipes--tag-filter))
    (delq nil
          (mapcar
           (lambda (f)
             (let* ((m (recipes--file-metadata f))
                    (tags (plist-get m :tags)))
               (when (and (or (null filter) (member filter tags))
                          (or (null recipes--filter-fn)
                              (funcall recipes--filter-fn m)))
                 (list f (recipes--entry-vector m)))))
           (recipes--files)))))

(defun recipes--rating-cmp (a b)
  "Numeric comparator on the star-rating column of entries A and B."
  (< (recipes--count-char (aref (cadr a) 1) ?★)
     (recipes--count-char (aref (cadr b) 1) ?★)))

(defconst recipes--column-headers
  ["Name" "Rating" "Tags" "Serves" "Cook" "Last Made" "Folder"]
  "Column header labels, in display order.  Indices match the entry vector.")

(defun recipes--dynamic-format (entries)
  "Return a `tabulated-list-format' vector auto-sized to ENTRIES' contents.
Each column is widened to the larger of its header label and its widest
cell (so titles/tags are never truncated), plus a little right padding on
every column but the last."
  (let* ((headers recipes--column-headers)
         (sorters (vector t #'recipes--rating-cmp t t t t t))
         (n (length headers))
         (widths (make-vector n 0)))
    (dotimes (i n)
      (aset widths i (string-width (aref headers i))))
    (dolist (e entries)
      (let ((vec (cadr e)))
        (dotimes (i n)
          (aset widths i (max (aref widths i)
                              (string-width (or (aref vec i) "")))))))
    (apply #'vector
           (cl-loop for i below n
                    for last = (= i (1- n))
                    collect (list (aref headers i)
                                  (if last (aref widths i) (+ 2 (aref widths i)))
                                  (aref sorters i))))))

(defvar recipes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")      #'recipes-open)
    (define-key map (kbd "<return>") #'recipes-open)
    (define-key map "v" #'recipes-cook)
    (define-key map "c" #'recipes-capture)
    (define-key map "g" #'recipes-refresh)
    (define-key map "f" #'recipes-filter-tag)
    (define-key map "/" #'recipes-search)
    (define-key map "R" #'recipes-set-rating)
    (define-key map "X" #'recipes-mark-made)
    (define-key map "m" #'recipes-mark)
    (define-key map "u" #'recipes-unmark)
    (define-key map "U" #'recipes-unmark-all)
    (define-key map "B" #'recipes-show-backlinks)
    (define-key map "p" #'recipes-add-photo)
    (define-key map "L" #'recipes-sort-by-last-made)
    (define-key map "V" #'recipes-views)
    (define-key map "?" #'recipes-dispatch)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `recipes-mode'.
Additional keys are contributed by the recipes-plan / recipes-export /
recipes-ai modules when they load.")

;; Evil intercepts single-key normal-state bindings (c=change, /=search,
;; m=set-mark, R/g/…) before they reach the major-mode map, so re-bind them
;; in evil's normal state.  `evil-define-key*' is the immediate, non-deferred
;; variant — reloads take effect without killing the buffer.
(with-eval-after-load 'evil
  (evil-define-key* 'normal recipes-mode-map
    (kbd "RET")      #'recipes-open
    (kbd "<return>") #'recipes-open
    "v" #'recipes-cook
    "c" #'recipes-capture
    "g" #'recipes-refresh
    "f" #'recipes-filter-tag
    "/" #'recipes-search
    "R" #'recipes-set-rating
    "X" #'recipes-mark-made
    "m" #'recipes-mark
    "u" #'recipes-unmark
    "U" #'recipes-unmark-all
    "B" #'recipes-show-backlinks
    "p" #'recipes-add-photo
    "L" #'recipes-sort-by-last-made
    "V" #'recipes-views
    "?" #'recipes-dispatch
    "q" #'quit-window))

(define-derived-mode recipes-mode tabulated-list-mode "Recipes"
  "Major mode for browsing the recipe collection."
  (setq tabulated-list-format
        `[("Name"      32 t)
          ("Rating"     7 ,#'recipes--rating-cmp)
          ("Tags"      22 t)
          ("Serves"     6 t)
          ("Cook"       8 t)
          ("Last Made" 12 t)
          ("Folder"    16 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun recipes-refresh ()
  "Re-scan the collection and redraw the browser."
  (interactive)
  (let ((entries (recipes--collect-entries)))
    (setq tabulated-list-entries entries)
    (setq tabulated-list-format (recipes--dynamic-format entries))
    (setq mode-line-process
          (let ((parts (delq nil (list (and recipes--tag-filter
                                            (format "#%s" recipes--tag-filter))
                                       recipes--filter-label))))
            (and parts (concat " [" (string-join parts " ") "]"))))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (recipes--repaint-marks)))

(defun recipes-open ()
  "Open the recipe at point."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (find-file file)))

(defun recipes--current-recipe ()
  "Return the recipe file for the current context.
Works from the browser (row at point) or while visiting a recipe file."
  (or (and (derived-mode-p 'recipes-mode) (tabulated-list-get-id))
      (and buffer-file-name
           (string-prefix-p (recipes--dir) (expand-file-name buffer-file-name))
           buffer-file-name)
      (user-error "Not on a recipe")))

(defun recipes-filter-tag (tag)
  "Filter the browser to recipes tagged TAG.  Empty input clears the filter."
  (interactive
   (list (completing-read "Filter by tag (empty to clear): "
                          (recipes--all-tags) nil nil)))
  (setq recipes--tag-filter (unless (string-empty-p (string-trim tag))
                              (string-trim tag)))
  (recipes-refresh)
  (message (if recipes--tag-filter
               (format "Filtering by #%s" recipes--tag-filter)
             "Filter cleared")))

(defun recipes-set-rating (rating)
  "Set the RATING (1-5) of the recipe at point."
  (interactive
   (list (read-number
          "Rating (1-5): "
          (let ((cur (recipes--file-prop (tabulated-list-get-id) "RATING")))
            (if cur (string-to-number cur) 4)))))
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (unless (<= 1 rating 5) (user-error "Rating must be between 1 and 5"))
    (recipes--set-file-property file "RATING" (number-to-string rating))
    (recipes-refresh)
    (message "Rated %s: %s" (file-name-base file)
             (recipes--stars (number-to-string rating)))))

(defun recipes-mark-made ()
  "Stamp the recipe at point as made today (sets LAST_MADE)."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (recipes--set-file-property
     file "LAST_MADE" (format-time-string "[%Y-%m-%d]"))
    (recipes-refresh)
    (message "Marked made today: %s" (file-name-base file))))

(defun recipes-search ()
  "Full-text search across the recipe collection."
  (interactive)
  (let ((dir (recipes--dir)))
    (cond
     ((fboundp 'consult-ripgrep) (consult-ripgrep dir))
     ((fboundp '+default/search-project)
      (let ((default-directory dir)) (call-interactively #'+default/search-project)))
     (t (let ((default-directory dir)) (call-interactively #'rgrep))))))

(defun recipes-show-keys ()
  "Echo the `recipes-mode' keymap."
  (interactive)
  (message
   "RET open · v cook · c new · I import · m/u/U mark · b basket · s schedule · e export · ? menu · q quit"))

;;;###autoload
(defun recipes ()
  "Open the recipe browser."
  (interactive)
  (let ((buf (get-buffer-create "*recipes*")))
    (with-current-buffer buf
      (recipes-mode)
      (recipes-refresh)
      (when (null tabulated-list-entries)
        (message "No recipes found under %s — press `c' to create one."
                 (recipes--dir))))
    (switch-to-buffer buf)))

;;; ------------------------------------------------------ capture (transient)

(defun recipes--read-tags (prompt &optional initial-input history)
  "Transient reader: read a comma-joined tag list with completion."
  (mapconcat #'identity
             (completing-read-multiple prompt (recipes--all-tags)
                                       nil nil initial-input history)
             ","))

(defun recipes--read-subdir (prompt &optional initial-input history)
  "Transient reader: read a subfolder, completing over existing ones."
  (completing-read prompt (recipes--subdirs) nil nil initial-input history))

(transient-define-infix recipes--infix-title ()
  :class 'transient-option
  :argument "--title="
  :prompt "Title: "
  :always-read t)

(transient-define-infix recipes--infix-tags ()
  :class 'transient-option
  :argument "--tags="
  :prompt "Tags: "
  :reader #'recipes--read-tags)

(transient-define-infix recipes--infix-subdir ()
  :class 'transient-option
  :argument "--subdir="
  :prompt "Subfolder: "
  :reader #'recipes--read-subdir)

(transient-define-infix recipes--infix-servings ()
  :class 'transient-option
  :argument "--servings="
  :prompt "Servings: "
  :always-read t)

(transient-define-infix recipes--infix-prep ()
  :class 'transient-option
  :argument "--prep="
  :prompt "Prep time (e.g. 20m): "
  :always-read t)

(transient-define-infix recipes--infix-cook ()
  :class 'transient-option
  :argument "--cook="
  :prompt "Cook time (e.g. 1h30m): "
  :always-read t)

(transient-define-infix recipes--infix-rating ()
  :class 'transient-option
  :argument "--rating="
  :prompt "Rating (1-5): "
  :choices '("1" "2" "3" "4" "5"))

(transient-define-infix recipes--infix-source ()
  :class 'transient-option
  :argument "--source="
  :prompt "Source URL: "
  :always-read t)

(defun recipes--arg (args flag)
  "Return the value of FLAG (e.g. \"--title=\") in transient ARGS, or nil."
  (let ((v (cl-some (lambda (s)
                      (and (string-prefix-p flag s) (substring s (length flag))))
                    args)))
    (and v (not (string-empty-p (string-trim v))) (string-trim v))))

(defun recipes--filetags (tags-string)
  "Build an org `#+filetags' value from TAGS-STRING (comma separated).
Always includes `recipe' first."
  (let* ((raw (when tags-string (split-string tags-string "[,]" t "[ \t]*")))
         (clean (seq-remove (lambda (x) (string= (downcase x) "recipe")) raw))
         (all (cons "recipe" (mapcar (lambda (s) (replace-regexp-in-string "[ \t]+" "_" s))
                                     clean))))
    (concat ":" (mapconcat #'identity all ":") ":")))

(defun recipes--skeleton (id title filetags servings prep cook rating source)
  "Return the org text for a new recipe file."
  (concat
   ":PROPERTIES:\n"
   (format ":ID:        %s\n" id)
   (format ":SERVINGS:  %s\n" (or servings ""))
   (format ":PREP:      %s\n" (or prep ""))
   (format ":COOK:      %s\n" (or cook ""))
   (format ":RATING:    %s\n" (or rating ""))
   (format ":SOURCE:    %s\n" (or source ""))
   ":LAST_MADE: \n"
   ":END:\n"
   (format "#+title: %s\n" title)
   (format "#+filetags: %s\n\n" filetags)
   "* Ingredients\n- \n\n"
   "* Instructions\n1. \n\n"
   "* Notes\n\n"
   "* Equipment\n- \n"))

(transient-define-suffix recipes-capture-create (&optional args)
  "Create a recipe file from the transient form ARGS and open it."
  (interactive (list (transient-args 'recipes-capture)))
  (let* ((title (recipes--arg args "--title="))
         (subdir (recipes--arg args "--subdir="))
         (tags (recipes--arg args "--tags="))
         (servings (recipes--arg args "--servings="))
         (prep (recipes--arg args "--prep="))
         (cook (recipes--arg args "--cook="))
         (rating (recipes--arg args "--rating="))
         (source (recipes--arg args "--source=")))
    (unless title
      (user-error "A title is required — press `t' to set one"))
    (let* ((dir (if subdir (expand-file-name subdir (recipes--dir)) (recipes--dir)))
           (path (expand-file-name (concat (recipes--slugify title) ".org") dir)))
      (make-directory dir t)
      (when (file-exists-p path)
        (user-error "Recipe already exists: %s" path))
      (with-temp-file path
        (insert (recipes--skeleton (org-id-new) title
                                   (recipes--filetags tags)
                                   servings prep cook rating source)))
      (when (fboundp 'org-roam-db-update-file)
        (ignore-errors (org-roam-db-update-file path)))
      (when (get-buffer "*recipes*")
        (with-current-buffer "*recipes*" (recipes-refresh)))
      (find-file path)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Ingredients$" nil t)
        (forward-line 1)
        (end-of-line))
      (message "Created %s" (file-relative-name path (recipes--dir))))))

;;;###autoload (autoload 'recipes-capture "recipes" nil t)
(transient-define-prefix recipes-capture ()
  "Capture a new recipe."
  ["New recipe"
   ["Required"
    ("t" "Title"      recipes--infix-title)]
   ["Metadata"
    ("g" "Tags"       recipes--infix-tags)
    ("d" "Subfolder"  recipes--infix-subdir)
    ("s" "Servings"   recipes--infix-servings)
    ("p" "Prep time"  recipes--infix-prep)
    ("k" "Cook time"  recipes--infix-cook)
    ("r" "Rating"     recipes--infix-rating)
    ("u" "Source URL" recipes--infix-source)]]
  ["Actions"
   ("c" "Create recipe" recipes-capture-create)
   ("q" "Cancel"        transient-quit-one)])

;;; ------------------------------------------------------ org-roam convenience

;;;###autoload
(defun recipes-find-roam ()
  "Find a recipe via `org-roam-node-find', filtered to the :recipe: tag."
  (interactive)
  (unless (fboundp 'org-roam-node-find)
    (user-error "org-roam is not available"))
  (org-roam-node-find
   nil nil
   (lambda (node) (member "recipe" (org-roam-node-tags node)))))

;;; ------------------------------------------------------ marking (multi-select)

(defun recipes--repaint-marks ()
  "Repaint the `*' glyph for every marked recipe currently visible."
  (when recipes--marked
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((id (tabulated-list-get-id)))
          (when (and id (member id recipes--marked))
            (tabulated-list-put-tag "*")))
        (forward-line 1)))))

(defun recipes--selected-files ()
  "Return the marked recipes, or the recipe at point if none are marked."
  (or (reverse recipes--marked)
      (let ((id (tabulated-list-get-id)))
        (and id (list id)))))

(defun recipes-mark ()
  "Mark the recipe at point for a batch action and move to the next line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (cl-pushnew id recipes--marked :test #'equal)
      (tabulated-list-put-tag "*" t))))

(defun recipes-unmark ()
  "Unmark the recipe at point and move to the next line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (setq recipes--marked (delete id recipes--marked))
      (tabulated-list-put-tag " " t))))

(defun recipes-unmark-all ()
  "Clear all marks."
  (interactive)
  (setq recipes--marked nil)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag " ")
      (forward-line 1)))
  (message "Marks cleared"))

;;; ------------------------------------------------------ views (filters)

(defun recipes--last-made-days (lm)
  "Return the absolute day number encoded in LAST_MADE string LM, or nil."
  (when (and lm (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" lm))
    (ignore-errors
      (time-to-days (date-to-time (concat (match-string 1 lm) " 00:00"))))))

(defun recipes-view-unrated ()
  "Show only recipes that have no rating."
  (interactive)
  (setq recipes--filter-fn (lambda (m) (null (plist-get m :rating)))
        recipes--filter-label "unrated")
  (recipes-refresh)
  (message "View: unrated"))

(defun recipes-view-stale (days)
  "Show only recipes not made within DAYS (never-made counts as stale)."
  (interactive (list (read-number "Not made in how many days: " 60)))
  (let ((cutoff (- (time-to-days (current-time)) days)))
    (setq recipes--filter-fn
          (lambda (m)
            (let ((d (recipes--last-made-days (plist-get m :last-made))))
              (or (null d) (< d cutoff))))
          recipes--filter-label (format "stale>%dd" days)))
  (recipes-refresh)
  (message "View: not made in %d days" days))

(defun recipes-view-clear ()
  "Clear the active view filter (the tag filter is left untouched)."
  (interactive)
  (setq recipes--filter-fn nil recipes--filter-label nil)
  (recipes-refresh)
  (message "View cleared"))

(defun recipes-sort-by-last-made ()
  "Sort by Last Made so never/least-recently made recipes float to the top."
  (interactive)
  (setq tabulated-list-sort-key '("Last Made" . nil))
  (tabulated-list-print t)
  (recipes--repaint-marks))

(transient-define-prefix recipes-views ()
  "Filtered views over the recipe collection."
  ["Views"
   ("u" "unrated"            recipes-view-unrated)
   ("s" "not made in N days" recipes-view-stale)
   ("t" "by tag"             recipes-filter-tag)
   ("c" "clear view"         recipes-view-clear)])

;;; ------------------------------------------------------ cook mode

(defvar-local recipes--cook-file nil
  "Recipe file backing the current `recipes-cook-mode' buffer.")

(defun recipes--body-lines (file)
  "Return FILE's recipe body as lines, with the drawer and leading keywords stripped."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (looking-at "[ \t]*:PROPERTIES:")
      (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
        (delete-region (point-min) (min (point-max) (1+ (line-end-position))))))
    (goto-char (point-min))
    (while (and (not (eobp)) (looking-at "^\\(#\\+\\|[ \t]*$\\)"))
      (forward-line 1))
    (split-string (buffer-substring-no-properties (point) (point-max)) "\n")))

(defun recipes-cook-edit ()
  "Open the underlying recipe file for editing."
  (interactive)
  (when recipes--cook-file (find-file recipes--cook-file)))

(defvar recipes-cook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    (define-key map "e" #'recipes-cook-edit)
    map)
  "Keymap for `recipes-cook-mode'.")

(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) recipes-cook-mode-map
    "q" #'quit-window
    "e" #'recipes-cook-edit)
  (evil-set-initial-state 'recipes-cook-mode 'motion))

(define-derived-mode recipes-cook-mode special-mode "Recipe-Cook"
  "Distraction-free, large-type reader for cooking a recipe."
  (setq buffer-read-only t)
  (setq cursor-type nil)
  (setq-local line-spacing 0.25))

(defun recipes--cook-render (file m)
  "Render recipe FILE (metadata plist M) into the current cook buffer."
  (insert "\n  ")
  (insert (propertize (or (plist-get m :title) (file-name-base file))
                      'face 'recipes-cook-title-face))
  (insert "\n\n")
  (let ((meta (string-join
               (delq nil
                     (list (when (plist-get m :servings) (concat "Serves " (plist-get m :servings)))
                           (when (plist-get m :prep)     (concat "Prep "   (plist-get m :prep)))
                           (when (plist-get m :cook)     (concat "Cook "   (plist-get m :cook)))))
               "    ·    ")))
    (unless (string-empty-p meta)
      (insert "  " (propertize meta 'face 'recipes-folder-face) "\n\n")))
  (dolist (line (recipes--body-lines file))
    (cond
     ((string-match "^\\*+ +\\(.*\\)$" line)
      (insert "\n  " (propertize (match-string 1 line) 'face 'recipes-cook-heading-face) "\n"))
     ((string-match-p "^[ \t]*:[A-Za-z_]+:" line) nil)
     (t (insert "  " (propertize line 'face 'recipes-cook-text-face) "\n")))))

(defun recipes-cook ()
  "Open a distraction-free cooking view of the recipe at point."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (let ((buf (get-buffer-create "*recipe-cook*"))
          (m (recipes--file-metadata file)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (recipes-cook-mode)
          (setq recipes--cook-file file)
          (recipes--cook-render file m)
          (goto-char (point-min))))
      (switch-to-buffer buf))))

;;; ------------------------------------------------------ backlinks + photo

(declare-function org-roam-buffer-toggle "org-roam-mode")
(declare-function org-download-clipboard "org-download")

(defun recipes-show-backlinks ()
  "Open the recipe at point and toggle the org-roam backlinks buffer."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (unless (fboundp 'org-roam-buffer-toggle)
      (user-error "org-roam is not available"))
    (find-file file)
    (org-roam-buffer-toggle)))

(defun recipes-add-photo ()
  "Open the recipe at point and paste a clipboard image under a Photos heading."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (unless (fboundp 'org-download-clipboard)
      (user-error "org-download is not available"))
    (find-file file)
    (unless (save-excursion (goto-char (point-min))
                            (re-search-forward "^\\* Photos[ \t]*$" nil t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n* Photos\n"))
    (goto-char (point-max))
    (org-download-clipboard)))

;;; ------------------------------------------------------ dispatcher

(defun recipes--ai-available-p ()
  "Non-nil when the recipes-ai module is loaded."
  (fboundp 'recipes-ai-import))

(declare-function recipes-ai-import "recipes-ai")
(declare-function recipes-ai-extract-macros "recipes-ai")
(declare-function recipes-ai-suggest-tags "recipes-ai")
(declare-function recipes-ai-assistant "recipes-ai")
(declare-function recipes-ai-scale "recipes-ai")
(declare-function recipes-shopping-list "recipes-plan")
(declare-function recipes-schedule "recipes-plan")
(declare-function recipes-export "recipes-export")

(transient-define-prefix recipes-dispatch ()
  "All recipe-browser actions."
  [["Open"
    ("RET" "open"       recipes-open)
    ("v"   "cook view"  recipes-cook)
    ("B"   "backlinks"  recipes-show-backlinks)]
   ["New / Import"
    ("c"   "capture"    recipes-capture)
    ("I"   "AI import"  recipes-ai-import :if recipes--ai-available-p)]
   ["Metadata"
    ("R"   "rate"       recipes-set-rating)
    ("X"   "made today" recipes-mark-made)
    ("p"   "add photo"  recipes-add-photo)
    ("N"   "AI macros"  recipes-ai-extract-macros :if recipes--ai-available-p)
    ("T"   "AI tags"    recipes-ai-suggest-tags   :if recipes--ai-available-p)]]
  [["Marks"
    ("m"   "mark"          recipes-mark)
    ("u"   "unmark"        recipes-unmark)
    ("U"   "unmark all"    recipes-unmark-all)]
   ["Batch / Plan"
    ("b"   "shopping list" recipes-shopping-list)
    ("s"   "schedule"      recipes-schedule)
    ("a"   "AI assistant"  recipes-ai-assistant :if recipes--ai-available-p)
    ("S"   "AI scale"      recipes-ai-scale     :if recipes--ai-available-p)]
   ["View / Export"
    ("f"   "filter tag"        recipes-filter-tag)
    ("V"   "views"             recipes-views)
    ("L"   "sort by last-made" recipes-sort-by-last-made)
    ("e"   "export"            recipes-export)
    ("g"   "refresh"           recipes-refresh)]])

;;; ------------------------------------------------------ keybindings

(map! :leader
      :desc "Recipes browser" "o k" #'recipes
      :desc "New recipe"      "o K" #'recipes-capture)

(provide 'recipes)
;;; recipes.el ends here
