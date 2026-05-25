;;; emacslife-relationships.el --- EmacsLife: verbs, dating, marriage, kids -*- lexical-binding: t; -*-
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
;; Per-NPC verb menus (friendly / mean / romantic / conflict), dating
;; mechanics (meeting people, asking out, marrying), and the kids
;; pipeline (try for baby, adopt, surrogate).

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-family)

;;; -----------------------------------------------------------------
;;; Verb registry

(defvar emacslife--npc-verbs nil
  "Alist of (ID . PLIST) for per-NPC interaction verbs.
Plist keys:
  :label string
  :category symbol (friendly, mean, romantic, conflict)
  :min-age int (character's age)
  :requires fn(char npc) returns bool
  :handler fn(char npc)")

(cl-defun emacslife-register-verb
    (&key id label category min-age requires handler)
  (setf (alist-get id emacslife--npc-verbs)
        (list :id id :label label :category (or category 'friendly)
              :min-age (or min-age 0)
              :requires requires :handler handler))
  id)

(defun emacslife-applicable-verbs (char npc)
  "Return all verbs that pass age/req gates for CHAR/NPC pair."
  (cl-remove-if-not
   (lambda (v)
     (let ((p (cdr v)))
       (and (>= (emacslife-character-age char) (plist-get p :min-age))
            (let ((req (plist-get p :requires)))
              (or (null req) (funcall req char npc))))))
   emacslife--npc-verbs))

;;; -----------------------------------------------------------------
;;; Verb handlers — friendly

(emacslife-register-verb
 :id 'compliment :label "Compliment" :category 'friendly :min-age 3
 :handler
 (lambda (_char npc)
   (let ((delta (+ 3 (random 6))))
     (emacslife-bump-bar npc delta)
     (emacslife-npc-log npc (format "You complimented them (+%d bar)." delta))
     (message "%s blushed.  Bar +%d." (emacslife-npc-name npc) delta))))

(emacslife-register-verb
 :id 'converse :label "Have a conversation" :category 'friendly :min-age 3
 :handler
 (lambda (_char npc)
   (let ((delta (+ 1 (random 5))))
     (emacslife-bump-bar npc delta)
     (emacslife-npc-log npc (format "Nice chat. (+%d bar)" delta))
     (message "Nice chat with %s.  Bar +%d." (emacslife-npc-name npc) delta))))

(emacslife-register-verb
 :id 'give-gift :label "Give a gift ($50)" :category 'friendly :min-age 5
 :requires (lambda (char _) (>= (emacslife-character-cash char) 50))
 :handler
 (lambda (char npc)
   (emacslife-bump-money char -50)
   (let ((delta (+ 5 (random 8))))
     (emacslife-bump-bar npc delta)
     (emacslife-npc-log npc (format "You gave them a gift. (+%d)" delta))
     (message "%s loved the gift.  Bar +%d." (emacslife-npc-name npc) delta))))

(emacslife-register-verb
 :id 'spend-time :label "Spend time" :category 'friendly :min-age 1
 :handler
 (lambda (char npc)
   (let ((delta (+ 2 (random 5))))
     (emacslife-bump-bar npc delta)
     (emacslife-bump-stat char :happiness 2)
     (emacslife-npc-log npc (format "Quality time. (+%d)" delta))
     (message "Quality time with %s.  +%d bar." (emacslife-npc-name npc) delta))))

;;; -----------------------------------------------------------------
;;; Verb handlers — mean

(emacslife-register-verb
 :id 'insult :label "Insult" :category 'mean :min-age 3
 :handler
 (lambda (char npc)
   (let ((delta (- (+ 5 (random 10)))))
     (emacslife-bump-bar npc delta)
     (emacslife-bump-stat char :karma -2)
     (emacslife-bump-stat char :happiness -1)
     (emacslife-npc-log npc (format "You insulted them. (%d)" delta))
     (message "%s is furious.  Bar %d." (emacslife-npc-name npc) delta))))

(emacslife-register-verb
 :id 'argue :label "Pick a fight" :category 'mean :min-age 3
 :handler
 (lambda (char npc)
   (let ((delta (- (+ 8 (random 12)))))
     (emacslife-bump-bar npc delta)
     (emacslife-bump-stat char :karma -3)
     (emacslife-bump-stat char :happiness -3)
     (emacslife-npc-log npc (format "Big argument. (%d)" delta))
     (message "Heated argument with %s.  Bar %d."
              (emacslife-npc-name npc) delta))))

(emacslife-register-verb
 :id 'slap :label "Slap" :category 'mean :min-age 5
 :handler
 (lambda (char npc)
   (let ((delta (- (+ 15 (random 15)))))
     (emacslife-bump-bar npc delta)
     (emacslife-bump-stat char :karma -8)
     (when (emacslife-roll 0.1)
       (emacslife-log char (format "%s called the cops on you for assault."
                                   (emacslife-npc-name npc))
                      :bad)
       (push (list :crime 'assault :year (+ (emacslife-character-birth-year char)
                                            (emacslife-character-age char)))
             (emacslife-character-jail-record char)))
     (emacslife-npc-log npc (format "You slapped them! (%d)" delta))
     (message "SLAP.  %s's bar %d." (emacslife-npc-name npc) delta))))

;;; -----------------------------------------------------------------
;;; Verb handlers — romantic

(defun emacslife--romance-eligible-p (char npc)
  "Can CHAR pursue NPC romantically?
Age 12+ and not a blood/adopted relative or pet.  No restriction on
gender — that's a per-encounter decision the player makes."
  (and (>= (emacslife-character-age char) 12)
       (>= (emacslife-npc-age npc) 12)
       (not (memq (emacslife-npc-relation npc)
                  '(mother father stepmother stepfather
                    sibling halfsibling stepsibling
                    grandmother grandfather aunt uncle
                    child stepchild grandchild
                    nephew niece pet)))))

(emacslife-register-verb
 :id 'flirt :label "Flirt" :category 'romantic :min-age 12
 :requires #'emacslife--romance-eligible-p
 :handler
 (lambda (char npc)
   (let* ((looks (emacslife-character-looks char))
          (chance (/ (+ 40.0 looks) 200.0)))
     (if (emacslife-roll chance)
         (progn
           (emacslife-bump-bar npc (+ 5 (random 10)))
           (message "%s flirted back!" (emacslife-npc-name npc)))
       (emacslife-bump-bar npc -3)
       (message "%s wasn't impressed." (emacslife-npc-name npc))))))

(emacslife-register-verb
 :id 'ask-out :label "Ask out on a date" :category 'romantic :min-age 13
 :requires (lambda (char npc)
             (and (emacslife--romance-eligible-p char npc)
                  (not (eq (emacslife-npc-relation npc) 'spouse))
                  (>= (emacslife-npc-bar npc) 40)))
 :handler
 (lambda (char npc)
   (if (emacslife-roll (/ (emacslife-npc-bar npc) 100.0))
       (progn
         (setf (emacslife-npc-relation npc) 'date)
         (emacslife-bump-bar npc 10)
         (emacslife-log char (format "%s said YES to a date!"
                                     (emacslife-npc-name npc))
                        :good)
         (message "It's a date with %s." (emacslife-npc-name npc)))
     (emacslife-bump-bar npc -10)
     (emacslife-log char (format "%s rejected you."
                                 (emacslife-npc-name npc))
                    :bad)
     (message "Rejection hurts."))))

(emacslife-register-verb
 :id 'become-couple :label "Become an official couple" :category 'romantic
 :min-age 13
 :requires (lambda (_char npc)
             (and (memq (emacslife-npc-relation npc) '(date crush))
                  (>= (emacslife-npc-bar npc) 60)))
 :handler
 (lambda (char npc)
   (setf (emacslife-npc-relation npc) 'partner)
   (emacslife-bump-bar npc 5)
   (emacslife-log char (format "You and %s are now a couple."
                               (emacslife-npc-name npc))
                  :good)
   (message "Couple status unlocked.")))

(emacslife-register-verb
 :id 'propose :label "Propose marriage (ring $5000)" :category 'romantic
 :min-age 16
 :requires (lambda (char npc)
             (and (emacslife-can-marry-p char)
                  (memq (emacslife-npc-relation npc) '(partner date))
                  (null (emacslife-character-spouse char))
                  (>= (emacslife-character-cash char) 5000)
                  (>= (emacslife-npc-bar npc) 70)))
 :handler
 (lambda (char npc)
   (emacslife-bump-money char -5000)
   (if (emacslife-roll (/ (+ 50.0 (emacslife-npc-bar npc)) 200.0))
       (progn
         (setf (emacslife-npc-relation npc) 'fiance)
         (setf (emacslife-npc-notes npc)
               (plist-put (or (emacslife-npc-notes npc) '()) :engaged-year
                          (+ (emacslife-character-birth-year char)
                             (emacslife-character-age char))))
         (emacslife-bump-bar npc 10)
         (emacslife-log char
                        (format "%s said YES! You're engaged."
                                (emacslife-npc-name npc))
                        :good)
         (message "She/he said yes!"))
     (emacslife-bump-bar npc -20)
     (emacslife-log char
                    (format "%s said NO. Awkward."
                            (emacslife-npc-name npc))
                    :bad)
     (message "Ouch."))))

(emacslife-register-verb
 :id 'marry :label "Hold the wedding (costs $15000)" :category 'romantic
 :min-age 16
 :requires (lambda (char npc)
             (and (eq (emacslife-npc-relation npc) 'fiance)
                  (>= (emacslife-character-cash char) 15000)))
 :handler
 (lambda (char npc)
   (emacslife-bump-money char -15000)
   (setf (emacslife-npc-relation npc) 'spouse)
   (setf (emacslife-character-spouse char) (emacslife-npc-id npc))
   (emacslife-bump-stat char :happiness 25)
   (emacslife-bump-bar npc 10)
   (emacslife-log char
                  (format "You married %s. Lavish wedding!"
                          (emacslife-npc-name npc))
                  :good)
   (message "Married!")))

(emacslife-register-verb
 :id 'divorce :label "File for divorce (legal $10000)" :category 'romantic
 :min-age 16
 :requires (lambda (char npc)
             (and (eq (emacslife-npc-relation npc) 'spouse)
                  (>= (emacslife-character-cash char) 10000)))
 :handler
 (lambda (char npc)
   (emacslife-bump-money char -10000)
   (setf (emacslife-npc-relation npc) 'ex)
   (setf (emacslife-character-spouse char) nil)
   (push (emacslife-npc-id npc) (emacslife-character-ex-spouses char))
   (emacslife-bump-stat char :happiness -15)
   (emacslife-log char
                  (format "You divorced %s. The lawyers got rich."
                          (emacslife-npc-name npc))
                  :bad)
   (message "Divorced.")))

(emacslife-register-verb
 :id 'try-baby :label "Try for a baby" :category 'romantic :min-age 16
 :requires (lambda (char npc)
             (and (memq (emacslife-npc-relation npc) '(spouse partner))
                  (memq (emacslife-character-gender char) '(male female))))
 :handler
 (lambda (char npc)
   (let* ((age (emacslife-character-age char))
          (fertility (cond ((< age 18) 0.30)
                           ((< age 25) 0.55)
                           ((< age 35) 0.45)
                           ((< age 42) 0.20)
                           (t 0.05)))
          (twins-chance 0.03))
     (cond
      ((emacslife-roll fertility)
       (let ((n (if (emacslife-roll twins-chance) 2 1)))
         (dotimes (_ n)
           (let ((baby (emacslife--make-npc
                        char :relation 'child :age 0
                        :surname (emacslife-character-surname char))))
             (push (emacslife-npc-id baby)
                   (emacslife-character-children char))))
         (emacslife-bump-stat char :happiness 15)
         (emacslife-log char
                        (format "You and %s welcomed %s baby/babies!"
                                (emacslife-npc-name npc)
                                (if (= n 2) "TWIN" "a"))
                        :good)
         (message "It's a baby!")))
      (t
       (emacslife-log char "No baby this year." :neutral)
       (message "Better luck next year."))))))

;;; -----------------------------------------------------------------
;;; Meet new people (action — triggered from activities)

(defun emacslife-meet-someone (char where)
  "Generate a new NPC encounter at WHERE (\\='bar, \\='gym, \\='work, etc.)."
  (let* ((gender (emacslife-pick '(male female)))
         (age-delta (- (random 11) 5))
         (npc-age (max 18 (+ (emacslife-character-age char) age-delta)))
         (npc (emacslife--make-npc
               char :relation 'crush :gender gender :age npc-age)))
    (setf (emacslife-npc-bar npc) (+ 30 (random 40)))
    (emacslife-log char
                   (format "You met %s at the %s."
                           (emacslife-npc-name npc) where)
                   :good)
    npc))

;;; -----------------------------------------------------------------
;;; Dispatch entry: pick NPC, pick verb, run handler

(defun emacslife--read-npc (char prompt)
  "Choose an NPC from CHAR via completing-read."
  (let* ((npcs (emacslife-living-npcs char))
         (rows (mapcar (lambda (n)
                         (cons (format "%s (%s, age %d, bar %d)"
                                       (emacslife-npc-name n)
                                       (emacslife--relation-display-name
                                        (emacslife-npc-relation n))
                                       (emacslife-npc-age n)
                                       (emacslife-npc-bar n))
                               n))
                       npcs)))
    (unless rows (user-error "No NPCs to interact with"))
    (cdr (assoc (completing-read prompt (mapcar #'car rows) nil t) rows))))

(defun emacslife--run-verb-menu (char npc)
  "Show applicable verbs for NPC, then run the chosen verb."
  (let* ((verbs (emacslife-applicable-verbs char npc))
         (rows (mapcar (lambda (v)
                         (let ((p (cdr v)))
                           (cons (format "[%s] %s"
                                         (plist-get p :category)
                                         (plist-get p :label))
                                 (plist-get p :id))))
                       verbs))
         (choice (completing-read (format "Action with %s: "
                                          (emacslife-npc-name npc))
                                  (mapcar #'car rows) nil t)))
    (when choice
      (let* ((id (cdr (assoc choice rows)))
             (entry (alist-get id emacslife--npc-verbs))
             (handler (plist-get entry :handler)))
        (funcall handler char npc)))))

(defun emacslife-interact (&optional npc-id)
  "Open the verb menu for NPC-ID (or prompt to pick one)."
  (interactive)
  (emacslife-with-state char
    (let ((npc (if npc-id
                   (or (emacslife-npc-by-id char npc-id)
                       (user-error "No such NPC"))
                 (emacslife--read-npc char "Interact with: "))))
      (emacslife--run-verb-menu char npc)
      ;; Refresh UI if it's loaded
      (when (fboundp 'emacslife-ui-refresh)
        (emacslife-ui-refresh)))))

;;; Register the action with the dispatcher

(emacslife-register-action
 :interact
 (lambda (form)
   (emacslife-interact (cadr form))))

(provide 'emacslife-relationships)
;;; emacslife-relationships.el ends here
