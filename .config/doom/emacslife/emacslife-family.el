;;; emacslife-family.el --- EmacsLife: NPCs, family tree, aging -*- lexical-binding: t; -*-
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
;; NPC struct, family tree generation at birth, aging NPCs each turn,
;; relationship-bar decay, NPC death rolls, and small lookups used by
;; the relationships UI.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)

;;; -----------------------------------------------------------------
;;; NPC struct

(cl-defstruct emacslife-npc
  id                                   ; unique symbol
  name surname gender age alive
  relation                             ; symbol — see below
  bar                                  ; 0..100 relationship strength
  stats                                ; plist :happiness :health :smarts :looks
  job income
  birth-year death-year death-cause
  notes                                ; per-NPC scratch (e.g. fiance-years)
  history)                             ; list of (age . text) — shared timeline

(defconst emacslife-relations
  '(mother father stepmother stepfather
    sibling halfsibling stepsibling
    grandmother grandfather
    aunt uncle cousin
    nephew niece
    spouse fiance partner crush date ex
    child stepchild grandchild
    friend bestfriend
    coworker boss employee classmate teacher
    enemy lover
    pet)
  "All possible NPC relation symbols.")

(defvar emacslife--npc-counter 0
  "Monotonically incrementing source for NPC IDs.")

(defun emacslife--new-npc-id ()
  (intern (format "npc-%d" (cl-incf emacslife--npc-counter))))

;;; -----------------------------------------------------------------
;;; NPC creation helpers

(cl-defun emacslife--make-npc (char &key relation gender age surname stats job income notes)
  "Build a fresh NPC tied to CHAR.  Defaults derive from CHAR's country."
  (let* ((country (emacslife-character-country char))
         (g (or gender (emacslife-pick '(male female))))
         (sn (or surname (emacslife--random-surname country)))
         (nm (emacslife--random-name country g))
         (year (- (+ (emacslife-character-birth-year char)
                     (emacslife-character-age char))
                  (or age 0)))
         (npc (make-emacslife-npc
               :id (emacslife--new-npc-id)
               :name nm :surname sn :gender g
               :age (or age 0) :alive t :relation relation
               :bar 70
               :stats (or stats (list :happiness 60 :health 70 :smarts 60 :looks 60))
               :job job :income (or income 0)
               :birth-year year :death-year nil :death-cause nil
               :notes notes :history nil)))
    (push (cons (emacslife-npc-id npc) npc)
          (emacslife-character-npcs char))
    npc))

(defun emacslife-npc-by-id (char id)
  "Return NPC struct for ID in CHAR, or nil."
  (cdr (assq id (emacslife-character-npcs char))))

(defun emacslife-npcs-by-relation (char relation)
  "All NPCs of CHAR whose :relation matches RELATION."
  (mapcar #'cdr
          (cl-remove-if-not
           (lambda (cell) (eq (emacslife-npc-relation (cdr cell)) relation))
           (emacslife-character-npcs char))))

(defun emacslife-living-npcs (char)
  "Return all living NPCs of CHAR."
  (mapcar #'cdr
          (cl-remove-if-not
           (lambda (cell) (emacslife-npc-alive (cdr cell)))
           (emacslife-character-npcs char))))

;;; -----------------------------------------------------------------
;;; Initial family tree generation

(defun emacslife-generate-family (char)
  "Populate CHAR with parents, grandparents, and 0-3 siblings."
  (let* ((country (emacslife-character-country char))
         (family-surname (emacslife--random-surname country))
         ;; Inherit surname from family
         (mother (emacslife--make-npc
                  char :relation 'mother :gender 'female
                  :age (+ 22 (random 18))
                  :surname family-surname
                  :stats (list :happiness (+ 40 (random 40))
                               :health (+ 50 (random 40))
                               :smarts (+ 40 (random 40))
                               :looks (+ 40 (random 40)))
                  :job (emacslife-pick
                        '("Nurse" "Teacher" "Engineer" "Accountant" "Homemaker"
                          "Cashier" "Manager" "Designer" "Doctor" "Cook"))
                  :income (+ 30000 (random 70000))))
         (father (emacslife--make-npc
                  char :relation 'father :gender 'male
                  :age (+ 24 (random 20))
                  :surname family-surname
                  :stats (list :happiness (+ 40 (random 40))
                               :health (+ 50 (random 40))
                               :smarts (+ 40 (random 40))
                               :looks (+ 40 (random 40)))
                  :job (emacslife-pick
                        '("Mechanic" "Plumber" "Engineer" "Driver" "Salesman"
                          "Construction" "Manager" "Doctor" "Lawyer" "Chef"))
                  :income (+ 30000 (random 80000))))
         (sibling-count (emacslife-weighted-pick
                         '((0 . 40) (1 . 30) (2 . 20) (3 . 10)))))
    ;; Force the character's surname to match family if newborn
    (setf (emacslife-character-surname char) family-surname)
    (setf (emacslife-character-parents char)
          (list (emacslife-npc-id mother) (emacslife-npc-id father)))
    ;; Siblings
    (dotimes (_ sibling-count)
      (let ((s (emacslife--make-npc
                char :relation 'sibling
                :age (- (random 11) 5) ; -5..+5 years
                :surname family-surname)))
        (when (< (emacslife-npc-age s) 0)
          (setf (emacslife-npc-age s) 0))
        (push (emacslife-npc-id s) (emacslife-character-siblings char))))
    ;; Grandparents (maybe alive)
    (when (emacslife-roll 0.85)
      (emacslife--make-npc char :relation 'grandmother :gender 'female
                           :age (+ 50 (random 30)) :surname family-surname))
    (when (emacslife-roll 0.7)
      (emacslife--make-npc char :relation 'grandfather :gender 'male
                           :age (+ 52 (random 32)) :surname family-surname))
    char))

;;; -----------------------------------------------------------------
;;; Yearly NPC aging & death rolls

(defun emacslife--npc-death-chance (npc)
  "Naturally-aged death probability per year for NPC."
  (let* ((age (emacslife-npc-age npc))
         (health (or (plist-get (emacslife-npc-stats npc) :health) 60))
         (base (cond
                ((< age 30) 0.001)
                ((< age 50) 0.005)
                ((< age 65) 0.015)
                ((< age 75) 0.04)
                ((< age 85) 0.10)
                ((< age 95) 0.25)
                (t 0.50))))
    (* base (max 0.2 (/ (- 110.0 health) 80.0)))))

(defun emacslife-age-npcs (char)
  "Advance all NPCs of CHAR by one year; roll deaths and decay bars."
  (dolist (cell (emacslife-character-npcs char))
    (let ((npc (cdr cell)))
      (when (emacslife-npc-alive npc)
        (cl-incf (emacslife-npc-age npc))
        ;; Death roll
        (when (emacslife-roll (emacslife--npc-death-chance npc))
          (setf (emacslife-npc-alive npc) nil)
          (setf (emacslife-npc-death-year npc)
                (+ (emacslife-character-birth-year char)
                   (emacslife-character-age char)))
          (setf (emacslife-npc-death-cause npc)
                (emacslife-pick '("natural causes" "heart attack" "stroke"
                                  "cancer" "freak accident" "old age"
                                  "the flu" "a bee sting")))
          (emacslife-log char
                         (format "%s your %s, died at age %d (%s)."
                                 (emacslife-npc-name npc)
                                 (emacslife--relation-display-name
                                  (emacslife-npc-relation npc))
                                 (emacslife-npc-age npc)
                                 (emacslife-npc-death-cause npc))
                         :bad))
        ;; Bar decays slowly without interaction
        (setf (emacslife-npc-bar npc)
              (max 0 (- (emacslife-npc-bar npc) 1)))))))

(defun emacslife--relation-display-name (rel)
  "Friendly display string for relation symbol REL."
  (pcase rel
    ('mother "mother") ('father "father")
    ('stepmother "step-mother") ('stepfather "step-father")
    ('sibling "sibling") ('halfsibling "half-sibling")
    ('stepsibling "step-sibling")
    ('grandmother "grandmother") ('grandfather "grandfather")
    ('aunt "aunt") ('uncle "uncle") ('cousin "cousin")
    ('nephew "nephew") ('niece "niece")
    ('spouse "spouse") ('fiance "fiancé(e)") ('partner "partner")
    ('crush "crush") ('date "date") ('ex "ex")
    ('child "child") ('stepchild "step-child") ('grandchild "grandchild")
    ('friend "friend") ('bestfriend "best friend")
    ('coworker "coworker") ('boss "boss") ('employee "employee")
    ('classmate "classmate") ('teacher "teacher")
    ('enemy "enemy") ('lover "lover") ('pet "pet")
    (_ (symbol-name rel))))

;;; -----------------------------------------------------------------
;;; Group helpers for the relationships browser

(defconst emacslife-relation-groups
  '(("Family"    . (mother father stepmother stepfather
                    sibling halfsibling stepsibling
                    grandmother grandfather
                    aunt uncle cousin nephew niece))
    ("Romantic"  . (spouse fiance partner crush date ex lover))
    ("Children"  . (child stepchild grandchild))
    ("Friends"   . (friend bestfriend))
    ("Work"      . (coworker boss employee classmate teacher))
    ("Enemies"   . (enemy))
    ("Pets"      . (pet)))
  "Display grouping for the relationships browser.")

(defun emacslife-npcs-grouped (char)
  "Return alist GROUP-NAME → list-of-living-npcs for CHAR."
  (mapcar
   (lambda (g)
     (let* ((group-name (car g))
            (rels (cdr g))
            (npcs (cl-remove-if-not
                   (lambda (n)
                     (and (emacslife-npc-alive n)
                          (memq (emacslife-npc-relation n) rels)))
                   (mapcar #'cdr (emacslife-character-npcs char)))))
       (cons group-name (sort npcs (lambda (a b)
                                     (> (emacslife-npc-bar a)
                                        (emacslife-npc-bar b)))))))
   emacslife-relation-groups))

;;; -----------------------------------------------------------------
;;; Bar manipulation

(defun emacslife-bump-bar (npc delta)
  "Change NPC's relationship bar by DELTA (clamped 0..100)."
  (setf (emacslife-npc-bar npc)
        (emacslife-clamp (+ (emacslife-npc-bar npc) delta))))

(defun emacslife-npc-log (npc text)
  "Append TEXT to NPC's shared history at NPC's current age."
  (setf (emacslife-npc-history npc)
        (append (emacslife-npc-history npc)
                (list (cons (emacslife-npc-age npc) text)))))

(provide 'emacslife-family)
;;; emacslife-family.el ends here
