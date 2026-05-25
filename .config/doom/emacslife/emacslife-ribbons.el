;;; emacslife-ribbons.el --- EmacsLife: ribbons & end-of-life eval -*- lexical-binding: t; -*-
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
;; Ribbon registry, end-of-life evaluation that awards the best-fitting
;; ribbon (or randomly picks among ties), and content for the death
;; screen + recap.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)

(defvar emacslife--ribbons nil
  "Alist (ID . PLIST) of ribbons.
Plist keys:
  :name string
  :description string
  :priority int (higher = preferred when multiple qualify)
  :predicate fn(char)→bool")

(cl-defun emacslife-register-ribbon
    (&key id name description priority predicate)
  (setf (alist-get id emacslife--ribbons)
        (list :id id :name name :description description
              :priority (or priority 1) :predicate predicate))
  id)

(emacslife-register-ribbon
 :id 'loaded :name "Loaded"
 :description "Died with $20M+ in net worth."
 :priority 9
 :predicate (lambda (c)
              (>= (+ (emacslife-character-cash c)
                     (apply #'+ (mapcar (lambda (a) (plist-get a :purchase-price))
                                        (emacslife-character-assets c))))
                  20000000)))

(emacslife-register-ribbon
 :id 'big-boss :name "Big Boss"
 :description "Climbed to CEO."
 :priority 8
 :predicate (lambda (c)
              (cl-some (lambda (j) (string-match-p "CEO" (or (plist-get j :title) "")))
                       (cons (or (emacslife-character-job c) '())
                             (emacslife-character-job-history c)))))

(emacslife-register-ribbon
 :id 'famous :name "Famous"
 :description "Achieved fame ≥ 80."
 :priority 7
 :predicate (lambda (c) (>= (emacslife-character-fame c) 80)))

(emacslife-register-ribbon
 :id 'naomi-who :name "Naomi Who?"
 :description "Topped the modeling/acting world."
 :priority 8
 :predicate (lambda (c)
              (cl-some (lambda (j)
                         (memq (plist-get j :track) '(acting music)))
                       (cons (or (emacslife-character-job c) '())
                             (emacslife-character-job-history c)))))

(emacslife-register-ribbon
 :id 'hero :name "Hero"
 :description "Saved lives at least once."
 :priority 6
 :predicate (lambda (c)
              (memq 'hero-rescue (emacslife-character-achievements c))))

(emacslife-register-ribbon
 :id 'fertile :name "Fertile"
 :description "Had at least 4 children."
 :priority 5
 :predicate (lambda (c) (>= (length (emacslife-character-children c)) 4)))

(emacslife-register-ribbon
 :id 'generous :name "Generous"
 :description "Had high karma at death."
 :priority 5
 :predicate (lambda (c) (>= (emacslife-character-karma c) 85)))

(emacslife-register-ribbon
 :id 'scandalous :name "Scandalous"
 :description "Served prison time."
 :priority 6
 :predicate (lambda (c) (>= (emacslife-character-prison-time-served c) 5)))

(emacslife-register-ribbon
 :id 'jailbird :name "Jailbird"
 :description "Convicted of three or more crimes."
 :priority 6
 :predicate (lambda (c) (>= (length (emacslife-character-jail-record c)) 3)))

(emacslife-register-ribbon
 :id 'houdini :name "Houdini"
 :description "Escaped from prison at least once."
 :priority 7
 :predicate (lambda (c)
              (memq 'houdini-attempt (emacslife-character-achievements c))))

(emacslife-register-ribbon
 :id 'lustful :name "Lustful"
 :description "More than three ex-spouses."
 :priority 5
 :predicate (lambda (c) (>= (length (emacslife-character-ex-spouses c)) 3)))

(emacslife-register-ribbon
 :id 'inner-peace :name "At Inner Peace"
 :description "Lived past 75 with happiness ≥ 85."
 :priority 6
 :predicate (lambda (c) (and (>= (emacslife-character-age c) 75)
                             (>= (emacslife-character-happiness c) 85))))

(emacslife-register-ribbon
 :id 'plain-jane :name "Plain Jane"
 :description "Had a perfectly average life."
 :priority 2
 :predicate (lambda (c)
              (and (< (emacslife-character-fame c) 10)
                   (< (length (emacslife-character-children c)) 3)
                   (< (emacslife-character-prison-time-served c) 1)
                   (< (length (emacslife-character-degrees c)) 2))))

(emacslife-register-ribbon
 :id 'candywriter :name "Candywriter"
 :description "Wrote video games for a living."
 :priority 7
 :predicate (lambda (c)
              (cl-some (lambda (j)
                         (eq (plist-get j :track) 'tech))
                       (cons (or (emacslife-character-job c) '())
                             (emacslife-character-job-history c)))))

(emacslife-register-ribbon
 :id 'mediocre :name "Mediocre"
 :description "Default — life happened, that's about it."
 :priority 1
 :predicate (lambda (_c) t))

(defun emacslife-award-ribbon (char)
  "Pick the highest-priority qualifying ribbon for CHAR and award it."
  (let* ((qualified (cl-remove-if-not
                     (lambda (cell)
                       (funcall (plist-get (cdr cell) :predicate) char))
                     emacslife--ribbons))
         (sorted (sort qualified
                       (lambda (a b)
                         (> (plist-get (cdr a) :priority)
                            (plist-get (cdr b) :priority)))))
         (winner (cdar sorted)))
    (when winner
      (push (plist-get winner :id) (emacslife-character-ribbons char)))
    winner))

(provide 'emacslife-ribbons)
;;; emacslife-ribbons.el ends here
