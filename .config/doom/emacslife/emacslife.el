;;; emacslife.el --- EmacsLife: BitLife clone for Emacs (entry) -*- lexical-binding: t; -*-
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
;; Top-level entry for EmacsLife.  Adds this directory to `load-path'
;; and requires every submodule.  Exposes the user-facing
;; `M-x emacslife' command (which is also wired into the SPC G L
;; shortcut from .config/doom/+games.el).
;;
;; Architecture: pluggable registries.  Each content domain (activities,
;; jobs, crimes, events, ribbons) is populated by `emacslife-register-*'
;; calls in its own file.  Add a new event by dropping one
;; `emacslife-register-event' call anywhere; no engine changes needed.
;;
;; Save state: see `emacslife-save-dir' (default
;; `~/.local/share/doom/emacslife/').  Optional recap export to
;; `emacslife-recap-export-dir' (default inside `org-directory').

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when dir (add-to-list 'load-path dir)))

(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-family)
(require 'emacslife-relationships)
(require 'emacslife-activities)
(require 'emacslife-instruments)
(require 'emacslife-skills)
(require 'emacslife-career)
(require 'emacslife-crime)
(require 'emacslife-assets)
(require 'emacslife-events)
(require 'emacslife-ribbons)
(require 'emacslife-achievements)
(require 'emacslife-travel)
(require 'emacslife-save)
(require 'emacslife-ui)

(setq emacslife--registry-loaded t)

;;;###autoload
(defun emacslife ()
  "Launch EmacsLife.  Shows the slot picker, then the dashboard hub."
  (interactive)
  (unless emacslife--state
    (emacslife-pick-slot-and-load))
  (emacslife-show-hub))

(provide 'emacslife)
;;; emacslife.el ends here
