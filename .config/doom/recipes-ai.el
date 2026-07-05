;;; recipes-ai.el --- AI import + utilities for recipes (cmacs-ai) -*- lexical-binding: t; -*-

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

;; AI layer for the recipe manager, driven by the `cmacs-ai' C module
;; (`cmacs-ai-prompt-sync', synchronous, returns text) with grok-4.3 by
;; default.  Everything is Emacs-native — no external CLI, no sbi.  Loaded
;; only on cmacs builds (see config.el `(when IS-CMACS ...)`); commands
;; degrade to a friendly error elsewhere.
;;
;;   recipes-ai-import          (I)  — draft a recipe from a URL / clipboard /
;;                                     region / eww page into a review buffer
;;   recipes-ai-extract-macros  (N)  — estimate per-serving macros -> :NUTRITION:
;;   recipes-ai-suggest-tags    (T)  — propose #+filetags (reconciled w/ known tags)
;;   recipes-ai-scale           (S)  — scale ingredient quantities (popup)
;;   recipes-ai-assistant       (a)  — ask a question about the recipe (popup)
;;   recipes-ai-meal-plan            — pick N varied meals + shopping list
;;
;; Structured tasks demand strict JSON and parse it with `json-parse-string'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'org-id)

;; Provided by recipes.el / recipes-plan.el (loaded first via config.el).
(declare-function recipes--all-tags "recipes")
(declare-function recipes--current-recipe "recipes")
(declare-function recipes--file-metadata "recipes")
(declare-function recipes--files "recipes")
(declare-function recipes--filetags "recipes")
(declare-function recipes--set-file-property "recipes")
(declare-function recipes--set-filetags "recipes")
(declare-function recipes--selected-files "recipes")
(declare-function recipes--slugify "recipes")
(declare-function recipes--dir "recipes")
(declare-function recipes--body-lines "recipes")
(declare-function recipes-refresh "recipes")
(declare-function recipes--ingredients "recipes-plan")
;; Provided by the cmacs-ai C module (only present on cmacs builds).
(declare-function cmacs-ai-prompt-sync "cmacs-ai")
(defvar recipes-mode-map)

(defcustom recipes-ai-provider 'grok
  "AI provider symbol passed to `cmacs-ai-prompt-sync'."
  :type 'symbol
  :group 'recipes)

(defcustom recipes-ai-model "grok-4.3"
  "Model name passed to `cmacs-ai-prompt-sync' (nil = provider default)."
  :type '(choice (const :tag "Provider default" nil) string)
  :group 'recipes)

(defcustom recipes-ai-max-source-chars 20000
  "Maximum characters of source text sent to the model on import."
  :type 'integer
  :group 'recipes)

;;; ------------------------------------------------------ AI foundation

(defun recipes-ai--available-p ()
  "Non-nil when the cmacs-ai module can run a prompt."
  (fboundp 'cmacs-ai-prompt-sync))

(defun recipes-ai--ensure ()
  "Signal a friendly error unless cmacs-ai is available."
  (unless (recipes-ai--available-p)
    (user-error "cmacs-ai is not available on this build")))

(defun recipes-ai--call (system prompt)
  "Run PROMPT with SYSTEM prompt through cmacs-ai; return the text."
  (recipes-ai--ensure)
  (condition-case err
      (cmacs-ai-prompt-sync prompt recipes-ai-provider system recipes-ai-model)
    (error (user-error "cmacs-ai request failed: %s" (error-message-string err)))))

(defun recipes-ai--extract-json (s)
  "Pull a JSON object/array string out of S (handles code fences and prose)."
  (let ((s (string-trim s)))
    (cond
     ((string-match "```\\(?:json\\)?[ \t\r\n]*\\(\\(?:.\\|\n\\)*?\\)[ \t\r\n]*```" s)
      (string-trim (match-string 1 s)))
     ((string-match "\\`[{[]" s) s)
     ((string-match "[{[]\\(?:.\\|\n\\)*[]}]" s) (match-string 0 s))
     (t s))))

(defun recipes-ai--parse (raw)
  "Parse RAW model output into an Elisp value (plist objects, list arrays)."
  (let ((json (recipes-ai--extract-json raw)))
    (condition-case err
        (json-parse-string json :object-type 'plist :array-type 'list
                           :null-object nil :false-object nil)
      (error
       (recipes-ai--popup "*recipe AI raw*" raw)
       (user-error "AI did not return valid JSON: %s" (error-message-string err))))))

(defun recipes-ai--str (v)
  "Coerce JSON value V to a non-empty string, or nil."
  (cond
   ((stringp v) (let ((s (string-trim v))) (unless (string-empty-p s) s)))
   ((numberp v) (number-to-string v))
   (t nil)))

(defun recipes-ai--strlist (v)
  "Coerce JSON array/value V to a list of non-empty strings."
  (delq nil (mapcar #'recipes-ai--str
                    (cond ((listp v) v)
                          ((vectorp v) (append v nil))
                          (v (list v))
                          (t nil)))))

(defun recipes-ai--popup (name text)
  "Show TEXT in a read-only popup buffer named NAME."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

;;; ------------------------------------------------------ import

(defun recipes-ai--clipboard ()
  "Return the system clipboard / last kill as a string."
  (or (ignore-errors (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
      (ignore-errors (gui-get-selection 'CLIPBOARD))
      (ignore-errors (current-kill 0 t))
      ""))

(defun recipes-ai--fetch-url (url)
  "Fetch URL and return its main text, rendered natively via shr."
  (require 'url)
  (require 'shr)
  (let ((buf (url-retrieve-synchronously url t t 30)))
    (unless buf (user-error "Failed to fetch %s" url))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t)
          (let ((dom (libxml-parse-html-region (point) (point-max))))
            (with-temp-buffer
              (shr-insert-document dom)
              (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer buf))))

(defun recipes-ai--gather-source ()
  "Prompt for and return the raw source text to import from."
  (pcase (completing-read "Import recipe from: "
                          '("URL" "clipboard" "region" "eww page") nil t)
    ("URL"       (recipes-ai--fetch-url (read-string "URL: ")))
    ("clipboard" (recipes-ai--clipboard))
    ("region"    (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (user-error "No active region")))
    ("eww page"  (if (derived-mode-p 'eww-mode)
                     (buffer-substring-no-properties (point-min) (point-max))
                   (user-error "Not in an eww buffer")))))

(defconst recipes-ai--import-system
  "You are a careful recipe parser. Extract ONE recipe from the provided text.
Return ONLY valid minified JSON, no markdown fences and no prose, with keys:
title (string), tags (array of lowercase strings), servings (string),
prep (string like \"20m\"), cook (string like \"1h30m\"), source (string URL or \"\"),
ingredients (array of strings), instructions (array of strings),
notes (array of strings), equipment (array of strings).
Do not include the word 'recipe' in tags. Prefer these known tags when they fit: %s.
Use \"\" or [] for unknown fields. Keep ingredient and instruction wording faithful."
  "System prompt for recipe import; %s is filled with the known tag vocabulary.")

(defun recipes-ai--bullets (items)
  "Render ITEMS as an org bullet list (or an empty bullet)."
  (if items (mapconcat (lambda (x) (format "- %s\n" x)) items "") "- \n"))

(defun recipes-ai--numbers (items)
  "Render ITEMS as an org numbered list (or an empty item)."
  (if items
      (let ((i 0))
        (mapconcat (lambda (x) (setq i (1+ i)) (format "%d. %s\n" i x)) items ""))
    "1. \n"))

(defun recipes-ai--render (title tags data)
  "Build the org text for a recipe from TITLE, TAGS and parsed DATA plist."
  (concat
   ":PROPERTIES:\n"
   (format ":ID:        %s\n" (org-id-new))
   (format ":SERVINGS:  %s\n" (or (recipes-ai--str (plist-get data :servings)) ""))
   (format ":PREP:      %s\n" (or (recipes-ai--str (plist-get data :prep)) ""))
   (format ":COOK:      %s\n" (or (recipes-ai--str (plist-get data :cook)) ""))
   ":RATING:    \n"
   (format ":SOURCE:    %s\n" (or (recipes-ai--str (plist-get data :source)) ""))
   ":LAST_MADE: \n"
   ":END:\n"
   (format "#+title: %s\n" title)
   (format "#+filetags: %s\n\n" (recipes--filetags (mapconcat #'identity tags ",")))
   "* Ingredients\n" (recipes-ai--bullets (recipes-ai--strlist (plist-get data :ingredients))) "\n"
   "* Instructions\n" (recipes-ai--numbers (recipes-ai--strlist (plist-get data :instructions))) "\n"
   "* Notes\n" (recipes-ai--bullets (recipes-ai--strlist (plist-get data :notes))) "\n"
   "* Equipment\n" (recipes-ai--bullets (recipes-ai--strlist (plist-get data :equipment)))))

(defun recipes-ai--draft-path (title)
  "Return a fresh (non-existent) recipe file path for TITLE under the collection."
  (let* ((dir (recipes--dir))
         (base (recipes--slugify title))
         (path (expand-file-name (concat base ".org") dir))
         (n 2))
    (while (file-exists-p path)
      (setq path (expand-file-name (format "%s_%d.org" base n) dir))
      (setq n (1+ n)))
    path))

(defun recipes-ai--arm-save-hook ()
  "Register the draft's roam/refresh update to run when the buffer is saved."
  (let ((file buffer-file-name))
    (add-hook 'after-save-hook
              (lambda ()
                (when (fboundp 'org-roam-db-update-file)
                  (ignore-errors (org-roam-db-update-file file)))
                (when (get-buffer "*recipes*")
                  (with-current-buffer "*recipes*" (recipes-refresh))))
              nil t)))

;;;###autoload
(defun recipes-ai-import ()
  "Import a recipe via AI into an unsaved draft buffer for review.
Save (\\[save-buffer]) to commit; discard the buffer to cancel."
  (interactive)
  (recipes-ai--ensure)
  (let* ((src (recipes-ai--gather-source))
         (content (string-trim (or src ""))))
    (when (string-empty-p content) (user-error "No source content"))
    (when (> (length content) recipes-ai-max-source-chars)
      (setq content (substring content 0 recipes-ai-max-source-chars)))
    (message "Asking %s to parse the recipe…" recipes-ai-provider)
    (let* ((system (format recipes-ai--import-system
                           (mapconcat #'identity (recipes--all-tags) ", ")))
           (data (recipes-ai--parse (recipes-ai--call system content)))
           (title (or (recipes-ai--str (plist-get data :title)) "Untitled Recipe"))
           (tags (recipes-ai--strlist (plist-get data :tags)))
           (path (recipes-ai--draft-path title)))
      (find-file path)
      (erase-buffer)
      (insert (recipes-ai--render title tags data))
      (goto-char (point-min))
      (set-buffer-modified-p t)
      (recipes-ai--arm-save-hook)
      (message "Draft ready — review, then %s to save (kill buffer to discard)"
               (substitute-command-keys "\\[save-buffer]")))))

;;; ------------------------------------------------------ utilities

(defun recipes-ai--ingredient-block (file)
  "Return FILE's ingredients as a plain bullet block for prompting."
  (mapconcat (lambda (x) (concat "- " x)) (recipes--ingredients file) "\n"))

;;;###autoload
(defun recipes-ai-extract-macros ()
  "Estimate per-serving macros for the current recipe and store :NUTRITION:."
  (interactive)
  (recipes-ai--ensure)
  (let* ((file (recipes--current-recipe))
         (m (recipes--file-metadata file))
         (ingredients (recipes--ingredients file)))
    (unless ingredients (user-error "No ingredients found in %s" (file-name-base file)))
    (let* ((system "You are a nutrition estimator. Given a recipe's ingredients and \
serving count, estimate PER-SERVING macros. Return ONLY JSON, no prose: \
{\"calories\":N,\"protein_g\":N,\"fat_g\":N,\"carbs_g\":N,\"net_carbs_g\":N}. Numbers only.")
           (prompt (format "Servings: %s\nIngredients:\n%s"
                           (or (plist-get m :servings) "unknown")
                           (recipes-ai--ingredient-block file)))
           (data (recipes-ai--parse (recipes-ai--call system prompt)))
           (v (lambda (k) (or (recipes-ai--str (plist-get data k)) "?")))
           (nutrition (format "%s kcal · %sp · %sf · %sc (net %s)"
                              (funcall v :calories) (funcall v :protein_g)
                              (funcall v :fat_g) (funcall v :carbs_g)
                              (funcall v :net_carbs_g))))
      (recipes--set-file-property file "NUTRITION" nutrition)
      (when (get-buffer "*recipes*")
        (with-current-buffer "*recipes*" (recipes-refresh)))
      (message "Nutrition (per serving): %s" nutrition))))

;;;###autoload
(defun recipes-ai-suggest-tags ()
  "Propose and apply #+filetags for the marked recipes (or the one at point)."
  (interactive)
  (recipes-ai--ensure)
  (let ((files (recipes--selected-files))
        (known (mapconcat #'identity (recipes--all-tags) ", ")))
    (unless files (user-error "No recipes selected"))
    (dolist (file files)
      (let* ((m (recipes--file-metadata file))
             (system (format "Suggest 2-5 lowercase category/diet tags for a recipe. \
Reuse these known tags when they fit: %s. Return ONLY a JSON array of strings, \
excluding the word 'recipe'." known))
             (prompt (format "Title: %s\nIngredients:\n%s"
                             (or (plist-get m :title) "") (recipes-ai--ingredient-block file)))
             (tags (recipes-ai--strlist (recipes-ai--parse (recipes-ai--call system prompt)))))
        (when tags (recipes--set-filetags file tags))))
    (when (get-buffer "*recipes*")
      (with-current-buffer "*recipes*" (recipes-refresh)))
    (message "Tags updated for %d recipe(s)" (length files))))

;;;###autoload
(defun recipes-ai-scale (servings)
  "Scale the current recipe's ingredients to SERVINGS and show them in a popup."
  (interactive (list (read-string "Scale to how many servings: ")))
  (recipes-ai--ensure)
  (let* ((file (recipes--current-recipe))
         (m (recipes--file-metadata file))
         (system "Scale the recipe's ingredient quantities to the requested serving \
count. Return ONLY a JSON array of strings (the scaled ingredient lines). Keep \
ingredient names, adjust only amounts.")
         (prompt (format "Current servings: %s\nTarget servings: %s\nIngredients:\n%s"
                         (or (plist-get m :servings) "unknown") servings
                         (recipes-ai--ingredient-block file)))
         (scaled (recipes-ai--strlist (recipes-ai--parse (recipes-ai--call system prompt)))))
    (recipes-ai--popup
     (format "*scaled: %s*" (or (plist-get m :title) "recipe"))
     (concat (format "%s — scaled to %s servings\n\n"
                     (or (plist-get m :title) "Recipe") servings)
             (mapconcat (lambda (x) (concat "- " x)) scaled "\n")))))

;;;###autoload
(defun recipes-ai-assistant (question)
  "Ask QUESTION about the current recipe and show the answer in a popup."
  (interactive (list (read-string "Ask about this recipe: ")))
  (recipes-ai--ensure)
  (let* ((file (recipes--current-recipe))
         (m (recipes--file-metadata file))
         (body (string-join (recipes--body-lines file) "\n"))
         (system "You are a helpful cooking assistant. Answer the question about the \
given recipe concisely and practically.")
         (prompt (format "Recipe: %s\n\n%s\n\nQuestion: %s"
                         (or (plist-get m :title) "") body question)))
    (recipes-ai--popup "*recipe assistant*" (recipes-ai--call system prompt))))

;;;###autoload
(defun recipes-ai-meal-plan (count)
  "Have the AI pick COUNT varied meals and show them with a combined shopping list."
  (interactive (list (read-number "How many meals to plan: " 5)))
  (recipes-ai--ensure)
  (let* ((files (recipes--files))
         (summary (mapconcat
                   (lambda (f)
                     (let ((m (recipes--file-metadata f)))
                       (format "- %s | tags: %s | last made: %s"
                               (plist-get m :title)
                               (mapconcat #'identity (plist-get m :tags) ",")
                               (or (plist-get m :last-made) "never"))))
                   files "\n"))
         (system (format "Plan %d varied meals chosen from the list below. Favor a \
diversity of tags and recipes not made recently. Return ONLY a JSON array of the \
chosen recipe titles, each an exact string from the list." count))
         (chosen (recipes-ai--strlist (recipes-ai--parse (recipes-ai--call system summary))))
         (chosen-files (delq nil (mapcar #'recipes-ai--file-for-title chosen))))
    (recipes-ai--popup
     "*meal plan*"
     (concat
      "#+title: Meal Plan\n\n* Meals\n"
      (mapconcat (lambda (f)
                   (let ((m (recipes--file-metadata f)))
                     (format "- %s" (or (plist-get m :title) (file-name-base f)))))
                 chosen-files "\n")
      "\n\n* Shopping List\n"
      (mapconcat (lambda (it) (format "- [ ] %s" it))
                 (seq-uniq (seq-mapcat #'recipes--ingredients chosen-files))
                 "\n")))))

(defun recipes-ai--file-for-title (title)
  "Return the recipe file whose title matches TITLE, or nil."
  (let ((needle (downcase (string-trim (or title "")))))
    (seq-find (lambda (f)
                (string= needle
                         (downcase (string-trim
                                    (or (plist-get (recipes--file-metadata f) :title) "")))))
              (recipes--files))))

;;; ------------------------------------------------------ keybindings

(define-key recipes-mode-map "I" #'recipes-ai-import)
(define-key recipes-mode-map "N" #'recipes-ai-extract-macros)
(define-key recipes-mode-map "T" #'recipes-ai-suggest-tags)
(define-key recipes-mode-map "S" #'recipes-ai-scale)
(define-key recipes-mode-map "a" #'recipes-ai-assistant)

(with-eval-after-load 'evil
  (evil-define-key* 'normal recipes-mode-map
    "I" #'recipes-ai-import
    "N" #'recipes-ai-extract-macros
    "T" #'recipes-ai-suggest-tags
    "S" #'recipes-ai-scale
    "a" #'recipes-ai-assistant))

(provide 'recipes-ai)
;;; recipes-ai.el ends here
