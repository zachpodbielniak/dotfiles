;;; emacslife-achievements.el --- EmacsLife: 40+ achievements -*- lexical-binding: t; -*-
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
;; A separate, lighter system than ribbons.  Ribbons are awarded ONCE
;; at death.  Achievements unlock during play whenever their predicate
;; passes — checked at every age-up, after any major action via
;; `emacslife-check-achievements'.  Multiple achievements may unlock
;; per turn.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-instruments)
(require 'emacslife-assets)
(require 'emacslife-skills)

(defvar emacslife--achievements nil
  "Alist (ID . PLIST).  Plist keys:
:name string :description string :secret bool
:predicate fn(char) → bool.")

(cl-defun emacslife-register-achievement
    (&key id name description secret predicate)
  (setf (alist-get id emacslife--achievements)
        (list :id id :name name :description description
              :secret secret :predicate predicate))
  id)

(defun emacslife-achievement (id) (alist-get id emacslife--achievements))

(defun emacslife-achievement-name (id)
  (or (plist-get (emacslife-achievement id) :name)
      (symbol-name id)))

(defun emacslife-grant-achievement (char id)
  "Grant achievement ID to CHAR if not already held.
Logs to the timeline.  Returns t if newly granted."
  (unless (memq id (emacslife-character-achievements char))
    (push id (emacslife-character-achievements char))
    (emacslife-log char
                   (format "🏅 Achievement unlocked: %s"
                            (emacslife-achievement-name id))
                   :good)
    t))

(defun emacslife-check-achievements (char)
  "Run every achievement predicate; grant any that pass."
  (dolist (cell emacslife--achievements)
    (let ((id (car cell))
          (entry (cdr cell)))
      (unless (memq id (emacslife-character-achievements char))
        (let ((pred (plist-get entry :predicate)))
          (when (and pred
                     (condition-case _err
                         (funcall pred char)
                       (error nil)))
            (emacslife-grant-achievement char id)))))))

;;; -----------------------------------------------------------------
;;; Achievement catalogue

;; Wealth
(emacslife-register-achievement
 :id 'first-1k :name "First $1,000"
 :description "Saved your first thousand dollars."
 :predicate (lambda (c) (>= (emacslife-character-cash c) 1000)))

(emacslife-register-achievement
 :id 'first-10k :name "Five-Digit Club"
 :description "Net worth over $10,000."
 :predicate (lambda (c) (>= (+ (emacslife-character-cash c)
                                (emacslife-portfolio-value c)) 10000)))

(emacslife-register-achievement
 :id 'first-100k :name "Six-Figure Saver"
 :description "Net worth crossed $100,000."
 :predicate (lambda (c) (>= (+ (emacslife-character-cash c)
                                (emacslife-portfolio-value c)) 100000)))

(emacslife-register-achievement
 :id 'millionaire :name "Millionaire"
 :description "Net worth crossed $1,000,000."
 :predicate (lambda (c) (>= (+ (emacslife-character-cash c)
                                (emacslife-portfolio-value c)) 1000000)))

(emacslife-register-achievement
 :id 'decamillionaire :name "Decamillionaire"
 :description "Net worth crossed $10,000,000."
 :predicate (lambda (c) (>= (+ (emacslife-character-cash c)
                                (emacslife-portfolio-value c)) 10000000)))

(emacslife-register-achievement
 :id 'centimillionaire :name "Centimillionaire"
 :description "Net worth crossed $100,000,000."
 :predicate (lambda (c) (>= (+ (emacslife-character-cash c)
                                (emacslife-portfolio-value c)) 100000000)))

(emacslife-register-achievement
 :id 'billionaire :name "Billionaire"
 :description "Net worth crossed $1,000,000,000.  Yes, with a B."
 :predicate (lambda (c) (>= (+ (emacslife-character-cash c)
                                (emacslife-portfolio-value c)) 1000000000)))

(emacslife-register-achievement
 :id 'deep-in-debt :name "Deeply In Debt"
 :description "Owed more than $100K.  Yikes."
 :predicate (lambda (c) (>= (emacslife-character-debt c) 100000)))

;; Career
(emacslife-register-achievement
 :id 'first-paycheck :name "First Paycheck"
 :description "Held a real job for a year."
 :predicate (lambda (c) (and (emacslife-character-job c)
                              (>= (plist-get (emacslife-character-job c) :years) 1))))

(emacslife-register-achievement
 :id 'career-hopper :name "Career Hopper"
 :description "Quit 5 different jobs."
 :predicate (lambda (c) (>= (length (emacslife-character-job-history c)) 5)))

(emacslife-register-achievement
 :id 'ceo :name "C-Suite"
 :description "Climbed to a CEO/CTO/CFO/COO title."
 :predicate (lambda (c)
              (cl-some (lambda (j)
                         (let ((title (or (plist-get j :title) "")))
                           (string-match-p "\\(CEO\\|CTO\\|CFO\\|COO\\)" title)))
                       (cons (or (emacslife-character-job c) '())
                             (emacslife-character-job-history c)))))

;; Education
(emacslife-register-achievement
 :id 'graduated :name "Graduated"
 :description "Finished high school."
 :predicate (lambda (c) (memq 'high-school-diploma
                               (emacslife-character-degrees c))))

(emacslife-register-achievement
 :id 'bachelor :name "Bachelor's Degree"
 :description "Earned a bachelor's degree."
 :predicate (lambda (c) (memq 'bachelors (emacslife-character-degrees c))))

(emacslife-register-achievement
 :id 'doctorate :name "Doctorate"
 :description "Earned a graduate or professional degree."
 :predicate (lambda (c) (cl-some (lambda (d) (memq d '(md jd graduate)))
                                  (emacslife-character-degrees c))))

(emacslife-register-achievement
 :id 'lifelong-learner :name "Lifelong Learner"
 :description "Earned 3+ degrees."
 :predicate (lambda (c) (>= (length (emacslife-character-degrees c)) 3)))

;; Family
(emacslife-register-achievement
 :id 'newlywed :name "Newlywed"
 :description "Got married for the first time."
 :predicate (lambda (c) (emacslife-character-spouse c)))

(emacslife-register-achievement
 :id 'one-true-love :name "One True Love"
 :description "Married once, never divorced, 25+ years."
 :predicate (lambda (c)
              (and (emacslife-character-spouse c)
                    (null (emacslife-character-ex-spouses c))
                    (let ((sp (emacslife-npc-by-id c (emacslife-character-spouse c))))
                      (and sp
                            (>= (- (emacslife-character-age c)
                                    (or (plist-get (emacslife-npc-notes sp) :engaged-year)
                                        (emacslife-character-age c)))
                                25))))))

(emacslife-register-achievement
 :id 'serial-divorcer :name "Serial Divorcer"
 :description "Three or more divorces."
 :predicate (lambda (c) (>= (length (emacslife-character-ex-spouses c)) 3)))

(emacslife-register-achievement
 :id 'first-kid :name "First Kid"
 :description "Had your first child."
 :predicate (lambda (c) (>= (length (emacslife-character-children c)) 1)))

(emacslife-register-achievement
 :id 'big-family :name "Big Family"
 :description "Five or more children."
 :predicate (lambda (c) (>= (length (emacslife-character-children c)) 5)))

(emacslife-register-achievement
 :id 'dozen-kids :name "Football Team"
 :description "Twelve or more children.  Calm down."
 :predicate (lambda (c) (>= (length (emacslife-character-children c)) 12)))

;; Skills + language
(emacslife-register-achievement
 :id 'jack-of-all-trades :name "Jack of All Trades"
 :description "Every skill at level 30+."
 :predicate (lambda (c)
              (cl-every (lambda (cell) (>= (emacslife-skill c (car cell)) 30))
                         emacslife-skill-names)))

(emacslife-register-achievement
 :id 'renaissance :name "Renaissance Soul"
 :description "Every skill at level 60+."
 :predicate (lambda (c)
              (cl-every (lambda (cell) (>= (emacslife-skill c (car cell)) 60))
                         emacslife-skill-names)))

(emacslife-register-achievement
 :id 'virtuoso :name "Virtuoso"
 :description "Maxed any skill at 100."
 :predicate (lambda (c)
              (cl-some (lambda (cell) (>= (emacslife-skill c (car cell)) 100))
                       emacslife-skill-names)))

(emacslife-register-achievement
 :id 'bilingual :name "Bilingual"
 :description "Speak 2+ languages at 60+."
 :predicate (lambda (c) (>= (length (emacslife-languages-spoken c)) 2)))

(emacslife-register-achievement
 :id 'polyglot :name "Polyglot"
 :description "Speak 5+ languages at 60+."
 :predicate (lambda (c) (>= (length (emacslife-languages-spoken c)) 5)))

;; Crime & jail
(emacslife-register-achievement
 :id 'first-crime :name "Rookie Criminal"
 :description "Committed your first crime."
 :predicate (lambda (c) (emacslife-character-jail-record c)))

(emacslife-register-achievement
 :id 'rap-sheet :name "Rap Sheet"
 :description "Five or more convictions."
 :predicate (lambda (c) (>= (length (emacslife-character-jail-record c)) 5)))

(emacslife-register-achievement
 :id 'long-stretch :name "Long Stretch"
 :description "Served 15+ years total in prison."
 :predicate (lambda (c) (>= (emacslife-character-prison-time-served c) 15)))

(emacslife-register-achievement
 :id 'houdini :name "Houdini"
 :description "Escaped from prison."
 :predicate (lambda (c) (memq 'houdini-attempt
                               (emacslife-character-achievements c))))

;; Karma & morality
(emacslife-register-achievement
 :id 'saint :name "Saint"
 :description "Karma at 95+ at some point."
 :predicate (lambda (c) (>= (emacslife-character-karma c) 95)))

(emacslife-register-achievement
 :id 'pure-evil :name "Pure Evil"
 :description "Karma at 5 or below at some point."
 :predicate (lambda (c) (<= (emacslife-character-karma c) 5)))

;; Health / age
(emacslife-register-achievement
 :id 'centenarian :name "Centenarian"
 :description "Lived to 100."
 :predicate (lambda (c) (>= (emacslife-character-age c) 100)))

(emacslife-register-achievement
 :id 'supercentenarian :name "Supercentenarian"
 :description "Lived to 110."
 :predicate (lambda (c) (>= (emacslife-character-age c) 110)))

(emacslife-register-achievement
 :id 'peak-fitness :name "Peak Fitness"
 :description "Health at 95+."
 :predicate (lambda (c) (>= (emacslife-character-health c) 95)))

(emacslife-register-achievement
 :id 'glamour :name "Glamour"
 :description "Looks at 95+."
 :predicate (lambda (c) (>= (emacslife-character-looks c) 95)))

(emacslife-register-achievement
 :id 'genius :name "Genius"
 :description "Smarts at 95+."
 :predicate (lambda (c) (>= (emacslife-character-smarts c) 95)))

;; Fame
(emacslife-register-achievement
 :id 'recognized :name "Recognized"
 :description "Fame reached 50."
 :predicate (lambda (c) (>= (emacslife-character-fame c) 50)))

(emacslife-register-achievement
 :id 'icon :name "Icon"
 :description "Fame reached 95."
 :predicate (lambda (c) (>= (emacslife-character-fame c) 95)))

;; Lifestyle
(emacslife-register-achievement
 :id 'lottery-winner :name "Lottery Winner"
 :description "Won the lottery (jackpot)."
 :predicate (lambda (c) (memq 'jackpot
                               (emacslife-character-achievements c))))

(emacslife-register-achievement
 :id 'pet-parent :name "Pet Parent"
 :description "Adopted your first pet."
 :predicate (lambda (c) (>= (length (emacslife-character-pets c)) 1)))

(emacslife-register-achievement
 :id 'menagerie :name "Menagerie"
 :description "Owned 5+ pets at once."
 :predicate (lambda (c) (>= (length (cl-remove-if-not (lambda (p)
                                                        (plist-get p :alive))
                                                      (emacslife-character-pets c)))
                              5)))

(emacslife-register-achievement
 :id 'asset-collector :name "Asset Collector"
 :description "Owned 5+ assets at once."
 :predicate (lambda (c) (>= (length (emacslife-character-assets c)) 5)))

(emacslife-register-achievement
 :id 'investor :name "Investor"
 :description "Held 5+ distinct positions in portfolio."
 :predicate (lambda (c) (>= (length (emacslife-character-investments c)) 5)))

(emacslife-register-achievement
 :id 'whale :name "Whale"
 :description "Portfolio value crossed $5M."
 :predicate (lambda (c) (>= (emacslife-portfolio-value c) 5000000)))

(emacslife-register-achievement
 :id 'first-dividend :name "First Dividend Check"
 :description "Received your first dividend payout."
 :predicate (lambda (c) (>= (emacslife--lifetime-dividends c) 1)))

(emacslife-register-achievement
 :id 'dividend-grower :name "Dividend Grower"
 :description "Lifetime dividends/distributions crossed $10K."
 :predicate (lambda (c) (>= (emacslife--lifetime-dividends c) 10000)))

(emacslife-register-achievement
 :id 'income-investor :name "Income Investor"
 :description "Lifetime dividends/distributions crossed $100K."
 :predicate (lambda (c) (>= (emacslife--lifetime-dividends c) 100000)))

(emacslife-register-achievement
 :id 'dividend-king :name "Dividend King"
 :description "Lifetime dividends/distributions crossed $1M."
 :predicate (lambda (c) (>= (emacslife--lifetime-dividends c) 1000000)))

(emacslife-register-achievement
 :id 'living-off-dividends :name "Living Off Dividends"
 :description "Annual portfolio yield exceeds $50K (financial independence)."
 :predicate (lambda (c) (>= (emacslife-portfolio-annual-income c) 50000)))

(emacslife-register-achievement
 :id 'reit-mogul :name "REIT Mogul"
 :description "Hold 3+ different REIT positions at once."
 :predicate (lambda (c)
              (>= (length (cl-remove-if-not
                            (lambda (p)
                              (let ((i (and (plist-get p :symbol)
                                            (emacslife-instrument
                                             (plist-get p :symbol)))))
                                (and i (eq (plist-get i :type) 'reit))))
                            (emacslife-character-investments c)))
                  3)))

;; Secret / silly
(emacslife-register-achievement
 :id 'foreign-agent :name "Recruited By Foreigners"
 :description "You did, didn't you." :secret t
 :predicate (lambda (c) (memq 'foreign-agent
                               (emacslife-character-achievements c))))

(emacslife-register-achievement
 :id 'hot-dog-champion :name "Hot Dog Champion"
 :description "Ate competitively." :secret t
 :predicate (lambda (c) (memq 'hot-dog-champion
                               (emacslife-character-achievements c))))

(emacslife-register-achievement
 :id 'cancer-survivor :name "Cancer Survivor"
 :description "Beat cancer." :secret t
 :predicate (lambda (c) (memq 'cancer-survivor
                               (emacslife-character-achievements c))))

(emacslife-register-achievement
 :id 'hero-rescue :name "Hero"
 :description "Saved a life."
 :predicate (lambda (c) (memq 'hero-rescue
                               (emacslife-character-achievements c))))

(provide 'emacslife-achievements)
;;; emacslife-achievements.el ends here
