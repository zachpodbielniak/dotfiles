;;; recipes-plan.el --- Shopping lists + meal planning for recipes -*- lexical-binding: t; -*-

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

;; Planning layer on top of `recipes.el':
;;
;;   recipes-shopping-list (b)  — aggregate the Ingredients of the marked
;;                                recipes (or the one at point) into an org
;;                                checkbox buffer, one section per recipe.
;;   recipes-schedule      (s)  — mirror a scheduled TODO for the recipe into
;;                                `recipes-meals-file' (02_areas/org/todo.org),
;;                                so it shows in org-agenda and org-timeblock.
;;   recipes-meal-plan          — schedule the marked recipes across days.
;;
;; Meals are mirrored (rather than scheduling the recipe file itself) because
;; the recipe collection lives under 03_resources, which `org-agenda-files'
;; deliberately excludes; the meals file lives under 02_areas, which is scanned.

;;; Code:

(require 'org)
(require 'subr-x)

;; Provided by recipes.el (loaded first via config.el).
(declare-function recipes--selected-files "recipes")
(declare-function recipes--file-metadata "recipes")
(declare-function recipes--file-prop "recipes")
(defvar recipes-notes-dir)
(defvar recipes-mode-map)

;;; ------------------------------------------------------ shopping list

(defun recipes--ingredients (file)
  "Return the ingredient strings from FILE across any \"Ingredients\" heading.
Collects bullet-list items and the first column of any ingredient table, so
it works for flat recipes, multi-component recipes, and table-based ones."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((items '())
          (case-fold-search t))
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) +.*ingredients.*$" nil t)
        (let* ((level (length (match-string 1)))
               (end (save-excursion
                      (if (re-search-forward (format "^\\*\\{1,%d\\} " level) nil t)
                          (match-beginning 0)
                        (point-max)))))
          (save-excursion
            (forward-line 1)
            (while (< (point) end)
              (let ((line (string-trim (buffer-substring-no-properties
                                        (line-beginning-position) (line-end-position)))))
                (cond
                 ;; bullet list item
                 ((string-match "^[-+] +\\(.*\\)$" line)
                  (push (string-trim (match-string 1 line)) items))
                 ;; table data row (skip separators and the header row)
                 ((and (string-prefix-p "|" line)
                       (not (string-match-p "^|[-+| ]*$" line)))
                  (let ((cell (string-trim (car (split-string (substring line 1) "|")))))
                    (unless (or (string-empty-p cell)
                                (member (downcase cell)
                                        '("ingredient" "ingredients" "item")))
                      (push cell items))))))
              (forward-line 1)))))
      (nreverse items))))

(defun recipes-shopping-list ()
  "Build a shopping list from the marked recipes (or the recipe at point).
Produces an org buffer with a checkbox list per recipe."
  (interactive)
  (let ((files (recipes--selected-files)))
    (unless files (user-error "No recipes selected"))
    (let ((buf (get-buffer-create "*recipe shopping list*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (org-mode)
          (insert "#+title: Shopping List\n\n")
          (dolist (file files)
            (let* ((m (recipes--file-metadata file))
                   (items (recipes--ingredients file)))
              (insert (format "* %s\n" (or (plist-get m :title) (file-name-base file))))
              (if items
                  (dolist (it items) (insert (format "- [ ] %s\n" it)))
                (insert "- [ ] (no ingredients found)\n"))
              (insert "\n")))
          (goto-char (point-min))))
      (pop-to-buffer buf))))

;;; ------------------------------------------------------ meal planning

(defcustom recipes-meals-file "02_areas/org/todo.org"
  "Org file (relative to `recipes-notes-dir') that scheduled meals mirror into.
Kept under 02_areas so the entries are inside `org-agenda-files'."
  :type 'string
  :group 'recipes)

(defcustom recipes-meals-heading "Meals"
  "Top-level heading in `recipes-meals-file' under which meals are filed."
  :type 'string
  :group 'recipes)

(defun recipes--meals-file ()
  "Absolute path of the meals file."
  (expand-file-name recipes-meals-file recipes-notes-dir))

(defun recipes--org-ts (time)
  "Format TIME as an active org timestamp with a time-of-day."
  (format-time-string "<%Y-%m-%d %a %H:%M>" time))

(defun recipes--schedule-file (file time)
  "Append a scheduled TODO for recipe FILE at TIME to the meals file."
  (let* ((m (recipes--file-metadata file))
         (title (or (plist-get m :title) (file-name-base file)))
         (id (recipes--file-prop file "ID"))
         (link (if id (format "[[id:%s][%s]]" id title) title))
         (heading-re (format "^\\* %s[ \t]*$" (regexp-quote recipes-meals-heading))))
    (with-current-buffer (find-file-noselect (recipes--meals-file))
      (save-restriction
        (widen)
        (save-excursion
          ;; Ensure the Meals heading exists.
          (goto-char (point-min))
          (unless (re-search-forward heading-re nil t)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (format "* %s\n" recipes-meals-heading)))
          ;; Jump to the end of the Meals subtree and append the entry.
          (goto-char (point-min))
          (re-search-forward heading-re nil t)
          (let ((end (save-excursion
                       (if (re-search-forward "^\\* " nil t)
                           (match-beginning 0)
                         (point-max)))))
            (goto-char end)
            (unless (bolp) (insert "\n"))
            (insert (format "** TODO Make %s\nSCHEDULED: %s\n"
                            link (recipes--org-ts time))))))
      (save-buffer))))

(defun recipes-schedule ()
  "Schedule the recipe at point as a meal (mirrors a TODO into the meals file)."
  (interactive)
  (let ((file (tabulated-list-get-id)))
    (unless file (user-error "No recipe at point"))
    (recipes--schedule-file file (org-read-date t t nil "Cook when (include a time): "))
    (message "Scheduled: %s" (file-name-base file))))

(defun recipes-meal-plan ()
  "Schedule the marked recipes (or the one at point) on successive days."
  (interactive)
  (let ((files (recipes--selected-files)))
    (unless files (user-error "No recipes selected"))
    (let ((start (org-read-date t t nil "First meal date/time: "))
          (day 0))
      (dolist (file files)
        (recipes--schedule-file file (time-add start (days-to-time day)))
        (setq day (1+ day)))
      (message "Scheduled %d meal(s)" (length files)))))

;;; ------------------------------------------------------ keybindings

(define-key recipes-mode-map "b" #'recipes-shopping-list)
(define-key recipes-mode-map "s" #'recipes-schedule)
(define-key recipes-mode-map "P" #'recipes-meal-plan)

(with-eval-after-load 'evil
  (evil-define-key* 'normal recipes-mode-map
    "b" #'recipes-shopping-list
    "s" #'recipes-schedule
    "P" #'recipes-meal-plan))

(provide 'recipes-plan)
;;; recipes-plan.el ends here
