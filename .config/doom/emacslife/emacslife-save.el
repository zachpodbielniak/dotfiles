;;; emacslife-save.el --- EmacsLife: multi-slot save/load + recap export -*- lexical-binding: t; -*-
;;
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
;; Multi-slot save format: one `slot-N.eld' file per slot in
;; `emacslife-save-dir'.  Contents are a `prin1'-serialized plist
;; holding the whole `emacslife-character' struct.
;;
;; Also: end-of-life recap export — writes a polished org file to
;; `emacslife-recap-export-dir' (typically inside the user's PARA
;; notes tree).

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-family)
(require 'emacslife-activities)

(defun emacslife--ensure-dir (dir)
  "Create DIR if missing."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun emacslife--slot-file (n)
  (expand-file-name (format "slot-%d.eld" n) emacslife-save-dir))

(defun emacslife-save (&optional slot)
  "Save the current life to SLOT (default `emacslife--current-slot')."
  (interactive)
  (emacslife-with-state char
    (let* ((n (or slot emacslife--current-slot 1))
           (file (emacslife--slot-file n))
           (payload (list :version emacslife-save-format-version
                          :saved-at (emacslife-iso-date)
                          :character char)))
      (emacslife--ensure-dir emacslife-save-dir)
      (with-temp-file file
        (let ((print-circle t)
              (print-length nil)
              (print-level nil))
          (prin1 payload (current-buffer))))
      (setq emacslife--current-slot n)
      (message "Saved to slot %d (%s)" n file))))

(defun emacslife-load (slot)
  "Load life from SLOT into `emacslife--state'."
  (let* ((file (emacslife--slot-file slot)))
    (unless (file-exists-p file)
      (user-error "No save file at %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let* ((payload (read (current-buffer)))
             (char (plist-get payload :character)))
        (unless (emacslife-character-p char)
          (user-error "Invalid save file: %s" file))
        (setq emacslife--state char
              emacslife--current-slot slot)
        (message "Loaded slot %d (%s, age %d)"
                 slot
                 (emacslife-character-full-name char)
                 (emacslife-character-age char))))))

(defun emacslife-list-slots ()
  "Return a list of plists describing existing slots."
  (when (file-directory-p emacslife-save-dir)
    (cl-loop for n from 1 to emacslife-max-slots
             for file = (emacslife--slot-file n)
             when (file-exists-p file)
             collect (list :slot n :file file
                           :summary (emacslife--slot-summary file)))))

(defun emacslife--slot-summary (file)
  "Read FILE and return a one-line summary, or nil on error."
  (condition-case _
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let* ((payload (read (current-buffer)))
               (char (plist-get payload :character)))
          (format "%s, age %d, %s, %s%s"
                  (emacslife-character-full-name char)
                  (emacslife-character-age char)
                  (emacslife-country-name char)
                  (emacslife-format-money (emacslife-character-cash char))
                  (if (emacslife-character-alive char) ""
                    " — DECEASED"))))
    (error nil)))

(defun emacslife-delete-slot (slot)
  "Delete SLOT after confirmation."
  (interactive)
  (let ((file (emacslife--slot-file slot)))
    (when (and (file-exists-p file)
               (yes-or-no-p (format "Permanently delete slot %d? " slot)))
      (delete-file file)
      (message "Slot %d deleted." slot))))

(defun emacslife-autosave-maybe ()
  "If autosave is on and we have a slot, save."
  (when (and emacslife-autosave emacslife--state)
    (let ((slot (or emacslife--current-slot 1)))
      (setq emacslife--current-slot slot)
      (ignore-errors (emacslife-save slot)))))

;;; -----------------------------------------------------------------
;;; Slot picker (used by the top-level emacslife entry point)

(defun emacslife-pick-slot-and-load ()
  "Show slot picker.  Either load a slot or start a new life."
  (let* ((slots (emacslife-list-slots))
         (rows (append
                (list (cons "[ New Life — random ]" 'new-random)
                      (cons "[ New Life — custom ]" 'new-custom))
                (mapcar
                 (lambda (s)
                   (cons (format "Slot %d: %s"
                                 (plist-get s :slot)
                                 (or (plist-get s :summary) "<empty>"))
                         (plist-get s :slot)))
                 slots)
                (when (< (length slots) emacslife-max-slots)
                  (cl-loop for n from 1 to emacslife-max-slots
                           unless (cl-some (lambda (s) (= (plist-get s :slot) n))
                                           slots)
                           collect (cons (format "[ Empty slot %d ]" n)
                                         (intern (format "empty-%d" n)))
                           into out
                           and return out))))
         (choice (completing-read "Life: " (mapcar #'car rows) nil t))
         (action (cdr (assoc choice rows))))
    (cond
     ((eq action 'new-random)
      (setq emacslife--state (emacslife-create-random-character))
      (emacslife-generate-family emacslife--state)
      (setq emacslife--current-slot
            (or (cl-loop for n from 1 to emacslife-max-slots
                         unless (file-exists-p (emacslife--slot-file n))
                         return n)
                1))
      (emacslife-log emacslife--state
                     (format "Born in %s. Welcome to the world."
                             (emacslife-country-name emacslife--state))
                     :good)
      (emacslife-autosave-maybe))
     ((eq action 'new-custom)
      (setq emacslife--state (emacslife-create-custom-character))
      (emacslife-generate-family emacslife--state)
      (setq emacslife--current-slot
            (or (cl-loop for n from 1 to emacslife-max-slots
                         unless (file-exists-p (emacslife--slot-file n))
                         return n)
                1))
      (emacslife-log emacslife--state
                     (format "Born in %s (custom). Welcome to the world."
                             (emacslife-country-name emacslife--state))
                     :good)
      (emacslife-autosave-maybe))
     ((symbolp action)
      ;; empty-N
      (let* ((s (symbol-name action))
             (n (string-to-number (substring s 6))))
        (setq emacslife--state (emacslife-create-random-character))
        (emacslife-generate-family emacslife--state)
        (setq emacslife--current-slot n)
        (emacslife-log emacslife--state
                       (format "Born in %s. Welcome to the world."
                               (emacslife-country-name emacslife--state))
                       :good)
        (emacslife-autosave-maybe)))
     ((integerp action)
      (emacslife-load action)))))

;;; -----------------------------------------------------------------
;;; Recap export

(defun emacslife-export-recap ()
  "Write an org-mode obituary/recap of the current (dead) character."
  (interactive)
  (emacslife-with-state char
    (unless emacslife-recap-export-dir
      (user-error "Recap export disabled (emacslife-recap-export-dir is nil)"))
    (emacslife--ensure-dir emacslife-recap-export-dir)
    (let* ((slug (replace-regexp-in-string
                  "[^A-Za-z0-9-]" "-"
                  (downcase (emacslife-character-full-name char))))
           (file (expand-file-name
                  (format "recap-%s-%s.org"
                          slug
                          (format-time-string "%Y%m%d-%H%M%S"))
                  emacslife-recap-export-dir)))
      (with-temp-file file
        (insert "#+title: " (emacslife-character-full-name char)
                " — Life Recap\n")
        (insert "#+date: " (emacslife-iso-date) "\n")
        (insert "#+filetags: :emacslife:recap:\n\n")
        (insert "* Obituary\n\n")
        (insert (format "%s was born in %s (%s) and lived to age %d.\n"
                        (emacslife-character-full-name char)
                        (emacslife-character-city char)
                        (emacslife-country-name char)
                        (emacslife-character-age char)))
        (when-let* ((cause (emacslife-character-death-cause char)))
          (insert (format "Cause of death: %s.\n\n" cause)))
        (insert (format "Net worth at death: %s.\n"
                        (emacslife-format-money
                         (emacslife-character-cash char))))
        (insert (format "Ribbons awarded: %s.\n\n"
                        (or (mapconcat #'symbol-name
                                       (emacslife-character-ribbons char) ", ")
                            "none")))
        (insert "* Final Stats\n")
        (insert (format "- Happiness: %d\n" (emacslife-character-happiness char)))
        (insert (format "- Health:    %d\n" (emacslife-character-health char)))
        (insert (format "- Smarts:    %d\n" (emacslife-character-smarts char)))
        (insert (format "- Looks:     %d\n" (emacslife-character-looks char)))
        (insert (format "- Karma:     %d\n" (emacslife-character-karma char)))
        (insert (format "- Fame:      %d\n\n" (emacslife-character-fame char)))
        (insert "* Education\n")
        (insert (format "- Stage at death: %s\n"
                        (emacslife-education-stage-name
                         (emacslife-character-education-stage char))))
        (insert (format "- Degrees:        %s\n"
                        (or (mapconcat #'symbol-name
                                       (emacslife-character-degrees char) ", ")
                            "none")))
        (when (emacslife-character-major char)
          (insert (format "- Major:          %s\n" (emacslife-character-major char))))
        (insert "\n* Career History\n")
        (dolist (j (cons (emacslife-character-job char)
                         (emacslife-character-job-history char)))
          (when j
            (insert (format "- %s ($%s/yr)%s\n"
                            (plist-get j :title)
                            (emacslife-format-money (plist-get j :salary))
                            (cond ((plist-get j :fired)   " — fired")
                                  ((plist-get j :retired) " — retired")
                                  (t ""))))))
        (insert "\n* Timeline\n")
        (dolist (entry (emacslife-character-timeline char))
          (insert (format "** Age %d (%d)\n   %s\n"
                          (car entry)
                          (plist-get (cdr entry) :year)
                          (plist-get (cdr entry) :text)))))
      (message "Recap saved to %s" file)
      file)))

(provide 'emacslife-save)
;;; emacslife-save.el ends here
