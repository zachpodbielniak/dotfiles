;;; emacslife-core.el --- EmacsLife: core defcustoms, faces, state -*- lexical-binding: t; -*-
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
;; Core defcustoms (save dir, recap export dir, slot count), faces
;; (catppuccin-mocha palette), the global `emacslife--state' variable,
;; small utility helpers (clamp, money formatting, RNG), and the
;; central `emacslife-do' action dispatcher.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst emacslife-version "1.0.0"
  "Current EmacsLife version.")

(defconst emacslife-save-format-version 1
  "On-disk save-format version; bump when the character struct grows.")

;;; -----------------------------------------------------------------
;;; Customization

(defgroup emacslife nil
  "EmacsLife — a BitLife-style life simulator inside Emacs."
  :group 'games
  :prefix "emacslife-")

(defcustom emacslife-save-dir
  (expand-file-name "emacslife/"
                    (or (getenv "XDG_DATA_HOME")
                        (expand-file-name "~/.local/share/doom/")))
  "Directory for EmacsLife save slots (one slot-N.eld per slot)."
  :type 'directory :group 'emacslife)

(defcustom emacslife-recap-export-dir
  (expand-file-name "02_areas/games/emacslife/"
                    (or (and (boundp 'org-directory) org-directory)
                        "~/Documents/notes/"))
  "Directory where end-of-life org recaps are exported.
Set to nil to disable recap export."
  :type '(choice (const :tag "Disabled" nil) directory)
  :group 'emacslife)

(defcustom emacslife-max-slots 5
  "Maximum number of save slots."
  :type 'integer :group 'emacslife)

(defcustom emacslife-autosave t
  "If non-nil, autosave the current life after every age-up and major action."
  :type 'boolean :group 'emacslife)

(defcustom emacslife-event-cooldown-years 8
  "Minimum years before a fired random event may repeat.
Tracked per-life in `emacslife-character-metadata' under :event-last-year.
Increase for rarer / more memorable events; decrease for more chaos."
  :type 'integer :group 'emacslife)

(defcustom emacslife-event-chance-by-age
  '((0  . 0.20)   ; 0-4   — very rare; a child's life is mostly school
    (5  . 0.40)   ; 5-12  — light sprinkle of school events
    (13 . 0.70)   ; 13-17 — teen drama starts up
    (18 . 0.80)   ; 18-29 — peak random-event chaos
    (30 . 0.65)   ; 30-49 — settled life
    (50 . 0.65)   ; 50-64 — career/family events
    (65 . 0.70)   ; 65-79 — health events ramp up
    (80 . 0.80)) ; 80+   — health/family attrition
  "Alist (MIN-AGE . CHANCE) — probability that *any* event fires at age-up.
Lookup uses the highest MIN-AGE not exceeding the character's age."
  :type '(alist :key-type integer :value-type float) :group 'emacslife)

(defcustom emacslife-event-max-per-year 2
  "Maximum number of distinct events that may fire in a single age-up."
  :type 'integer :group 'emacslife)

(defcustom emacslife-god-mode nil
  "If non-nil, expose God Mode actions (edit stats, resurrect, force events)."
  :type 'boolean :group 'emacslife)

(defcustom emacslife-annual-col-raise 0.02
  "Base annual cost-of-living raise (e.g. 0.02 = +2%/yr regardless of perf)."
  :type 'float :group 'emacslife)

(defcustom emacslife-annual-merit-raise-max 0.04
  "Max annual merit raise on top of the COL raise.
Actual merit raise scales linearly with the current :performance:
  merit = MAX * (performance / 100)
So at performance 50 the merit bump is ~half of MAX, and a perfect 100
gets the full bonus.  Default 0.04 → 0–4% on top of COL."
  :type 'float :group 'emacslife)

;;; -----------------------------------------------------------------
;;; Faces — catppuccin-mocha palette

(defface emacslife-face-title
  '((t (:foreground "#cba6f7" :weight bold :height 1.4)))
  "Face for the main EmacsLife title."
  :group 'emacslife)

(defface emacslife-face-name
  '((t (:foreground "#f5c2e7" :weight bold)))
  "Face for the character's name."
  :group 'emacslife)

(defface emacslife-face-age
  '((t (:foreground "#fab387" :weight bold)))
  "Face for age display."
  :group 'emacslife)

(defface emacslife-face-stat-happiness
  '((t (:foreground "#f9e2af")))
  "Face for Happiness stat bars."
  :group 'emacslife)

(defface emacslife-face-stat-health
  '((t (:foreground "#a6e3a1")))
  "Face for Health stat bars."
  :group 'emacslife)

(defface emacslife-face-stat-smarts
  '((t (:foreground "#89b4fa")))
  "Face for Smarts stat bars."
  :group 'emacslife)

(defface emacslife-face-stat-looks
  '((t (:foreground "#f38ba8")))
  "Face for Looks stat bars."
  :group 'emacslife)

(defface emacslife-face-money
  '((t (:foreground "#a6e3a1" :weight bold)))
  "Face for monetary amounts."
  :group 'emacslife)

(defface emacslife-face-debt
  '((t (:foreground "#f38ba8" :weight bold)))
  "Face for negative balances / debt."
  :group 'emacslife)

(defface emacslife-face-tab
  '((t (:foreground "#94e2d5" :underline t)))
  "Face for clickable dashboard tab links."
  :group 'emacslife)

(defface emacslife-face-year
  '((t (:foreground "#cba6f7" :weight bold)))
  "Face for year/age headings in the timeline."
  :group 'emacslife)

(defface emacslife-face-event-good
  '((t (:foreground "#a6e3a1")))
  "Face for positive timeline events."
  :group 'emacslife)

(defface emacslife-face-event-bad
  '((t (:foreground "#f38ba8")))
  "Face for negative timeline events."
  :group 'emacslife)

(defface emacslife-face-event-neutral
  '((t (:foreground "#cdd6f4")))
  "Face for neutral timeline events."
  :group 'emacslife)

(defface emacslife-face-event-funny
  '((t (:foreground "#fab387" :slant italic)))
  "Face for absurd/comedic timeline events."
  :group 'emacslife)

(defface emacslife-face-ribbon
  '((t (:foreground "#f9e2af" :weight bold :box t)))
  "Face for awarded ribbons."
  :group 'emacslife)

(defface emacslife-face-section
  '((t (:foreground "#94e2d5" :weight bold)))
  "Face for section headers."
  :group 'emacslife)

;;; -----------------------------------------------------------------
;;; Global state

(defvar emacslife--state nil
  "The currently-loaded character (an `emacslife-character' struct, or nil).")

(defvar emacslife--current-slot nil
  "Slot number for the currently-loaded life (integer, or nil).")

(defvar emacslife--registry-loaded nil
  "Set non-nil after all content registries have populated themselves.")

;;; -----------------------------------------------------------------
;;; Utility helpers

(defun emacslife-clamp (n &optional lo hi)
  "Clamp N to [LO, HI] (default 0..100)."
  (max (or lo 0) (min (or hi 100) n)))

(defun emacslife-roll (chance)
  "Return non-nil with probability CHANCE (0.0..1.0)."
  (< (random 1000) (round (* 1000.0 chance))))

(defun emacslife-pick (list)
  "Return a random element of LIST, or nil if empty."
  (when list (nth (random (length list)) list)))

(defun emacslife-weighted-pick (alist)
  "Pick a key from ALIST where each cell is (KEY . WEIGHT)."
  (let* ((total (apply #'+ (mapcar #'cdr alist)))
         (r (random (max 1 total)))
         (sum 0)
         choice)
    (cl-dolist (cell alist choice)
      (cl-incf sum (cdr cell))
      (when (and (null choice) (< r sum))
        (setq choice (car cell))))))

(defun emacslife-format-money (n)
  "Pretty-print N as a dollar amount."
  (let* ((neg (< n 0))
         (abs (abs n))
         (s (cond
             ((>= abs 1000000000) (format "$%.2fB" (/ abs 1e9)))
             ((>= abs 1000000)    (format "$%.2fM" (/ abs 1e6)))
             ((>= abs 1000)       (format "$%.1fK" (/ abs 1e3)))
             (t                   (format "$%d" abs)))))
    (if neg (concat "-" s) s)))

(defun emacslife-bar (value &optional width face)
  "Render VALUE (0..100) as a Unicode bar of WIDTH chars (default 10)."
  (let* ((w (or width 10))
         (filled (round (* w (/ (float (max 0 (min 100 value))) 100.0))))
         (empty (- w filled))
         (s (concat (make-string filled ?█) (make-string empty ?░))))
    (if face (propertize s 'face face) s)))

(defun emacslife-pluralize (n word)
  "Pluralize WORD based on N (simple s-suffix)."
  (format "%d %s%s" n word (if (= n 1) "" "s")))

(defun emacslife-iso-date ()
  "Return current time as ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

;;; -----------------------------------------------------------------
;;; State accessors (light wrappers, defined here so all modules
;;; can call them without re-requiring character)

(defmacro emacslife-with-state (var &rest body)
  "Bind VAR to the current `emacslife--state' and run BODY.
Errors if no life is loaded."
  (declare (indent 1))
  `(let ((,var (or emacslife--state
                   (user-error "No life loaded.  Start one with M-x emacslife"))))
     ,@body))

;;; -----------------------------------------------------------------
;;; Central action dispatcher
;;;
;;; All dashboard tab links, org-buffer action links, and verb-menu
;;; callbacks funnel through `emacslife-do' so that we have a single
;;; chokepoint for: autosave, dead-character guard, refresh-after.

(defvar emacslife--action-handlers (make-hash-table :test 'eq)
  "Hash of action KEY → (lambda (action-form) ...).
Keys are the first symbol in the action plist (e.g. :age, :activity).")

(defun emacslife-register-action (key handler)
  "Install HANDLER for action KEY.
HANDLER is called with the full action form (a list)."
  (puthash key handler emacslife--action-handlers))

(defun emacslife-do (action)
  "Dispatch ACTION (a list whose CAR is a keyword).
Examples:
  (emacslife-do \\='(:age))
  (emacslife-do \\='(:activity gym))
  (emacslife-do \\='(:tab stats))"
  (interactive)
  (unless (listp action)
    (user-error "emacslife-do: ACTION must be a list, got %S" action))
  (let* ((key (car action))
         (handler (gethash key emacslife--action-handlers)))
    (unless handler
      (user-error "No handler registered for action %S" key))
    (funcall handler action)))

;; `emacslife-log' lives in emacslife-character.el alongside the struct
;; whose accessors it needs.

(provide 'emacslife-core)
;;; emacslife-core.el ends here
