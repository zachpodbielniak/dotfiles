;;; emacslife-travel.el --- EmacsLife: vacations + passport stamps -*- lexical-binding: t; -*-
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
;; Vacation system.  Pick a country, pay the trip cost, roll for a
;; vacation outcome.  Tracks passport stamps in
;; `emacslife-character-metadata' so achievements can chase
;; "visit N countries" style goals.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-skills)
(require 'emacslife-achievements)

;;; -----------------------------------------------------------------
;;; Passport tracking

(defun emacslife-passport-stamps (char)
  "List of country IDs CHAR has visited."
  (or (plist-get (emacslife-character-metadata char) :passport-stamps)
      '()))

(defun emacslife-add-stamp (char country)
  (let* ((md (or (emacslife-character-metadata char) '()))
         (stamps (or (plist-get md :passport-stamps) '())))
    (unless (memq country stamps)
      (push country stamps)
      (setf (emacslife-character-metadata char)
            (plist-put md :passport-stamps stamps)))))

;;; -----------------------------------------------------------------
;;; Vacation outcomes — one event per trip

(defconst emacslife--vacation-outcomes
  '((:text "Hit every landmark and ate too much."
     :tone :good
     :handler (lambda (c) (emacslife-bump-stat c :happiness 12)))
    (:text "Got lost in the old quarter, found a hidden bistro."
     :tone :good
     :handler (lambda (c) (emacslife-bump-stat c :happiness 10)
                          (emacslife-bump-stat c :smarts 1)))
    (:text "Met someone wonderful at a beach bar."
     :tone :good
     :handler (lambda (c) (emacslife-meet-someone c "vacation")
                          (emacslife-bump-stat c :happiness 8)))
    (:text "Got food poisoning from street meat."
     :tone :bad
     :handler (lambda (c) (emacslife-bump-stat c :health -8)
                          (emacslife-bump-stat c :happiness -3)))
    (:text "Pickpocketed in a crowded square."
     :tone :bad
     :handler (lambda (c) (emacslife-bump-money c
                                                  (- (min 500 (emacslife-character-cash c))))))
    (:text "Hiked a famous mountain."
     :tone :good
     :handler (lambda (c) (emacslife-bump-stat c :health 5)
                          (emacslife-bump-stat c :happiness 8)))
    (:text "Spent most of the trip in the airport."
     :tone :neutral
     :handler (lambda (c) (emacslife-bump-stat c :happiness -3)))
    (:text "Picked up some of the local language."
     :tone :good
     :handler (lambda (c) (emacslife-bump-stat c :smarts 2)))
    (:text "Took thousands of photos."
     :tone :good
     :handler (lambda (c) (emacslife-bump-skill c 'photography 5)))
    (:text "Tried something on the menu you couldn't read."
     :tone :neutral
     :handler (lambda (c) (emacslife-bump-stat c :happiness 4))))
  "List of vacation outcome plists.")

;;; -----------------------------------------------------------------
;;; Trip cost helper

(defun emacslife--trip-cost (char dest)
  "Cost of trip from CHAR's home country to DEST country (id)."
  (if (eq dest (emacslife-character-country char))
      300
    (let* ((home (emacslife-character-country char))
           (continental-pairs
            ;; rough adjacency clusters for cheap continental trips
            '((usa canada mexico)
              (uk germany france sweden)
              (japan)
              (brazil)
              (india nigeria))))
      (if (cl-some (lambda (group) (and (memq home group) (memq dest group)))
                   continental-pairs)
          (+ 800 (random 1200))      ; continental ~$800-$2000
        (+ 2500 (random 4500))))))   ; intercontinental ~$2500-$7000

;;; -----------------------------------------------------------------
;;; Take a trip

(defun emacslife-take-vacation ()
  "Pick a destination country and take a vacation."
  (interactive)
  (emacslife-with-state char
    (when (< (emacslife-character-age char) 5)
      (user-error "Too young to travel alone"))
    (let* ((rows (mapcar (lambda (cell)
                           (cons (plist-get (cdr cell) :name)
                                 (car cell)))
                          emacslife--countries))
           (dest (cdr (assoc (completing-read "Destination: "
                                                (mapcar #'car rows) nil t)
                              rows)))
           (cost (emacslife--trip-cost char dest)))
      (when (< (emacslife-character-cash char) cost)
        (user-error "Trip costs $%s; you have $%s."
                    (emacslife-format-money cost)
                    (emacslife-format-money (emacslife-character-cash char))))
      (when (yes-or-no-p (format "Trip to %s costs $%s. Go? "
                                  (cdr (assoc dest
                                              (mapcar (lambda (r) (cons (cdr r) (car r)))
                                                       rows)))
                                  (emacslife-format-money cost)))
        (emacslife-bump-money char (- cost))
        (emacslife-add-stamp char dest)
        ;; Roll outcome
        (let ((outcome (nth (random (length emacslife--vacation-outcomes))
                            emacslife--vacation-outcomes)))
          (funcall (plist-get outcome :handler) char)
          (emacslife-log char (format "Visited %s: %s"
                                        (plist-get (emacslife-country dest) :name)
                                        (plist-get outcome :text))
                          (plist-get outcome :tone)))
        (message "Stamp acquired.")
        (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))))

;;; -----------------------------------------------------------------
;;; World traveler achievements (defined here to live with the feature)

(emacslife-register-achievement
 :id 'world-traveler :name "World Traveler"
 :description "Visited 5+ countries."
 :predicate (lambda (c) (>= (length (emacslife-passport-stamps c)) 5)))

(emacslife-register-achievement
 :id 'globetrotter :name "Globetrotter"
 :description "Visited 10+ countries."
 :predicate (lambda (c) (>= (length (emacslife-passport-stamps c)) 10)))

(emacslife-register-action :travel (lambda (_) (emacslife-take-vacation)))

(provide 'emacslife-travel)
;;; emacslife-travel.el ends here
