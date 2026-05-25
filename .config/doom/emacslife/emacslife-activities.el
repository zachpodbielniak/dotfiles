;;; emacslife-activities.el --- EmacsLife: mind&body + education -*- lexical-binding: t; -*-
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
;; Pluggable activity registry plus the Mind & Body and Education
;; content sets.  Each activity is an `emacslife-register-activity'
;; call; the registry is consumed by the dashboard hub via the
;; `:activity' action.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-family)

;; Forward declarations — these live in modules that load AFTER us
;; (skills.el and relationships.el both `require' this file), so the
;; byte-compiler can't see them when compiling here.
(declare-function emacslife-skill          "emacslife-skills"        (char skill))
(declare-function emacslife-meet-someone   "emacslife-relationships" (char where))

;;; -----------------------------------------------------------------
;;; Registry

(defvar emacslife--activities nil
  "Alist of (ID . PLIST) for registered activities.
Plist keys:
  :id symbol           unique
  :label string        display
  :category symbol     one of: mind-body education lifestyle pet
  :min-age int         min char age
  :max-age int|nil     max char age (inclusive)
  :cost int            money deducted on attempt (negative ok)
  :requires fn(char) returning bool
  :handler fn(char)    side-effect: applies deltas, logs, may message
  :description string  optional one-liner")

(cl-defun emacslife-register-activity
    (&key id label category min-age max-age cost requires handler description)
  (unless (and id label handler)
    (error "emacslife-register-activity: :id :label :handler all required"))
  (setf (alist-get id emacslife--activities)
        (list :id id :label label :category (or category 'lifestyle)
              :min-age (or min-age 0) :max-age max-age
              :cost (or cost 0) :requires requires
              :handler handler :description description))
  id)

(defun emacslife-applicable-activities (char category)
  "Return activities of CATEGORY that pass CHAR's age/requirements."
  (let ((age (emacslife-character-age char)))
    (cl-remove-if-not
     (lambda (cell)
       (let ((p (cdr cell)))
         (and (eq (plist-get p :category) category)
              (>= age (plist-get p :min-age))
              (or (null (plist-get p :max-age))
                  (<= age (plist-get p :max-age)))
              (let ((r (plist-get p :requires)))
                (or (null r) (funcall r char))))))
     emacslife--activities)))

(defun emacslife-run-activity (char id)
  "Run activity ID on CHAR.  Deducts cost, calls handler."
  (let* ((entry (or (alist-get id emacslife--activities)
                    (user-error "Unknown activity: %S" id)))
         (cost (plist-get entry :cost)))
    (when (> cost 0)
      (when (< (emacslife-character-cash char) cost)
        (user-error "You can't afford %s ($%d)."
                    (plist-get entry :label) cost))
      (emacslife-bump-money char (- cost)))
    (funcall (plist-get entry :handler) char)))

(defun emacslife-pick-activity-and-run (char category)
  "Show menu of CATEGORY activities for CHAR and run the chosen one."
  (let* ((acts (emacslife-applicable-activities char category))
         (_ (unless acts (user-error "No %S activities available right now."
                                     category)))
         (rows (mapcar
                (lambda (cell)
                  (let ((p (cdr cell)))
                    (cons (format "%-28s  %s"
                                  (plist-get p :label)
                                  (if (> (plist-get p :cost) 0)
                                      (format "($%d) %s"
                                              (plist-get p :cost)
                                              (or (plist-get p :description) ""))
                                    (or (plist-get p :description) "")))
                          (plist-get p :id))))
                acts))
         (choice (completing-read (format "%S: " category) (mapcar #'car rows) nil t)))
    (when choice
      (emacslife-run-activity char (cdr (assoc choice rows))))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; Register dispatcher actions

(emacslife-register-action
 :activity
 (lambda (form) (emacslife-with-state c
                  (emacslife-run-activity c (cadr form)))))

(emacslife-register-action
 :activities
 (lambda (form) (emacslife-with-state c
                  (emacslife-pick-activity-and-run c (cadr form)))))

;;; -----------------------------------------------------------------
;;; Mind & Body activities

(emacslife-register-activity
 :id 'library :label "Visit the library" :category 'mind-body :min-age 4
 :description "Quiet your mind. Smarts +1..5."
 :handler
 (lambda (char)
   (let ((d (+ 1 (random 5))))
     (emacslife-bump-stat char :smarts d)
     (emacslife-bump-stat char :happiness 1)
     (emacslife-log char (format "Went to the library. Smarts +%d." d) :good)
     (message "You feel slightly nerdier."))))

(emacslife-register-activity
 :id 'gym :label "Hit the gym" :category 'mind-body :min-age 12 :cost 40
 :description "Health +; small chance of injury."
 :handler
 (lambda (char)
   (let ((d (+ 2 (random 5))))
     (emacslife-bump-stat char :health d)
     (when (emacslife-roll 0.05)
       (emacslife-bump-stat char :health -8)
       (emacslife-log char "You pulled something at the gym." :bad))
     (emacslife-bump-stat char :looks 1)
     (emacslife-log char (format "Gym session. Health +%d." d) :good)
     (message "Pump achieved."))))

(emacslife-register-activity
 :id 'meditate :label "Meditate" :category 'mind-body :min-age 5
 :description "Happiness ++; tiny smarts."
 :handler
 (lambda (char)
   (let ((d (+ 3 (random 5))))
     (emacslife-bump-stat char :happiness d)
     (emacslife-bump-stat char :smarts 1)
     (emacslife-log char (format "Meditated. Happiness +%d." d) :good)
     (message "Inner peace, briefly."))))

(emacslife-register-activity
 :id 'hospital :label "Go to the hospital" :category 'mind-body
 :cost 500 :min-age 0
 :description "Cure ailments, fix some health."
 :handler
 (lambda (char)
   (let ((d (+ 10 (random 15))))
     (emacslife-bump-stat char :health d)
     (emacslife-log char (format "Hospital visit. Health +%d. Bill: $500." d)
                    :good)
     (message "You feel better."))))

(emacslife-register-activity
 :id 'therapist :label "See a therapist" :category 'mind-body
 :cost 200 :min-age 12
 :description "Happiness ++; long-term resilience."
 :handler
 (lambda (char)
   (let ((d (+ 5 (random 10))))
     (emacslife-bump-stat char :happiness d)
     (emacslife-bump-stat char :smarts 1)
     (emacslife-log char (format "Therapy session. Happiness +%d." d) :good)
     (message "Cathartic."))))

(emacslife-register-activity
 :id 'plastic-surgery :label "Plastic surgery" :category 'mind-body
 :cost 8000 :min-age 18
 :description "Risky looks boost (or disaster)."
 :handler
 (lambda (char)
   (cond
    ((emacslife-roll 0.85)
     (let ((d (+ 5 (random 15))))
       (emacslife-bump-stat char :looks d)
       (emacslife-log char (format "Plastic surgery success. Looks +%d." d) :good)
       (message "Looking fresh.")))
    (t
     (emacslife-bump-stat char :looks -10)
     (emacslife-bump-stat char :health -5)
     (emacslife-log char "Plastic surgery botched. Looks -10." :bad)
     (message "Yikes.")))))

(emacslife-register-activity
 :id 'salon :label "Hair salon" :category 'mind-body :cost 80 :min-age 5
 :description "Small looks boost."
 :handler
 (lambda (char)
   (let ((d (+ 1 (random 4))))
     (emacslife-bump-stat char :looks d)
     (emacslife-bump-stat char :happiness 2)
     (emacslife-log char (format "New hair. Looks +%d." d) :good)
     (message "Fresh cut."))))

(emacslife-register-activity
 :id 'spa :label "Day at the spa" :category 'mind-body :cost 250 :min-age 16
 :description "Happiness + looks."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 6)
   (emacslife-bump-stat char :looks 2)
   (emacslife-log char "Spa day. Bliss." :good)
   (message "Cucumber slices applied.")))

(emacslife-register-activity
 :id 'tattoo :label "Get a tattoo" :category 'mind-body :cost 200 :min-age 16
 :description "Looks delta is random and committed."
 :handler
 (lambda (char)
   (let ((d (- (random 11) 5)))
     (emacslife-bump-stat char :looks d)
     (emacslife-bump-stat char :karma -1)
     (emacslife-log char
                    (format "You got a tattoo. Looks %s%d."
                            (if (>= d 0) "+" "") d)
                    (if (>= d 0) :good :bad))
     (message "Permanent regret/joy locked in."))))

(emacslife-register-activity
 :id 'witch-doctor :label "Visit a witch doctor" :category 'mind-body
 :cost 100 :min-age 5
 :description "Anything could happen."
 :handler
 (lambda (char)
   (pcase (random 5)
     (0 (emacslife-bump-stat char :health 15)
        (emacslife-log char "The witch doctor healed you mysteriously." :good))
     (1 (emacslife-bump-stat char :looks 10)
        (emacslife-log char "Witch doctor potion made you radiant." :good))
     (2 (emacslife-bump-stat char :happiness -5)
        (emacslife-bump-stat char :health -5)
        (emacslife-log char "The witch doctor cursed you. Brilliant." :bad))
     (3 (emacslife-bump-stat char :smarts 5)
        (emacslife-log char "Witch doctor showed you the True Light." :good))
     (4 (emacslife-log char "Witch doctor took your money and left." :neutral)))
   (message "Mystical.")))

(emacslife-register-activity
 :id 'hike :label "Go hiking" :category 'mind-body :min-age 8
 :description "Happiness + small health."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 4)
   (emacslife-bump-stat char :health 2)
   (emacslife-log char "Long hike. Brain feels better." :good)
   (message "Trees acquired.")))

(emacslife-register-activity
 :id 'yoga :label "Yoga class" :category 'mind-body :cost 30 :min-age 10
 :description "Happiness + flexibility."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 4)
   (emacslife-bump-stat char :health 1)
   (emacslife-log char "Yoga.  Downward facing emacs." :good)
   (message "Namaste.")))

(emacslife-register-activity
 :id 'diet :label "Go on a strict diet" :category 'mind-body :min-age 14
 :description "Health/looks +, happiness -"
 :handler
 (lambda (char)
   (emacslife-bump-stat char :health 4)
   (emacslife-bump-stat char :looks 3)
   (emacslife-bump-stat char :happiness -4)
   (emacslife-log char "Started a diet. You miss carbs already." :neutral)
   (message "Hungry.")))

(emacslife-register-activity
 :id 'martial-arts :label "Train martial arts" :category 'mind-body
 :cost 100 :min-age 8
 :description "Health + a little smarts."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :health 3)
   (emacslife-bump-stat char :smarts 2)
   (emacslife-log char "Martial arts training. Hi-yah." :good)
   (message "Crane stance engaged.")))

(emacslife-register-activity
 :id 'cult :label "Join a cult" :category 'mind-body :min-age 16
 :description "Wild stat swings."
 :handler
 (lambda (char)
   (let ((h (- (random 41) 15))
         (k (- (random 31) 20)))
     (emacslife-bump-stat char :happiness h)
     (emacslife-bump-stat char :karma k)
     (emacslife-log char
                    (format "You joined a cult. Happiness %s%d, karma %s%d."
                            (if (>= h 0) "+" "") h
                            (if (>= k 0) "+" "") k)
                    :funny)
     (message "Welcome, brother/sister."))))

;;; -----------------------------------------------------------------
;;; Education activities — these advance education-stage

(defconst emacslife-education-stages
  '((none           . 0)
    (daycare        . 3)
    (elementary     . 6)
    (middle-school  . 11)
    (high-school    . 14)
    (community      . 18)
    (university     . 18)
    (grad-school    . 22)
    (med-school     . 22)
    (law-school     . 22)
    (graduated      . 100))
  "Min entry age for each education stage.")

(defun emacslife-education-stage-name (stage)
  (pcase stage
    ('none "Not in school")
    ('daycare "Daycare")
    ('elementary "Elementary school")
    ('middle-school "Middle school")
    ('high-school "High school")
    ('community "Community college")
    ('university "University")
    ('grad-school "Grad school")
    ('med-school "Medical school")
    ('law-school "Law school")
    ('graduated "Graduated")
    (_ (symbol-name stage))))

;; Auto-progression hook: called from age-up. Promotes through K-12 by age.
(defun emacslife-auto-school-progress (char)
  "Advance CHAR automatically through K-12 stages by age.
Returns t if anything changed."
  (let* ((age (emacslife-character-age char))
         (stage (emacslife-character-education-stage char))
         (new (cond
               ((and (eq stage 'none) (>= age 3))
                'daycare)
               ((and (eq stage 'daycare) (>= age 6))
                'elementary)
               ((and (eq stage 'elementary) (>= age 11))
                'middle-school)
               ((and (eq stage 'middle-school) (>= age 14))
                'high-school)
               ((and (eq stage 'high-school) (>= age 18))
                'graduated)
               (t nil))))
    (when new
      (setf (emacslife-character-education-stage char) new)
      (emacslife-log char
                     (format "Started %s." (emacslife-education-stage-name new))
                     :good)
      (when (eq new 'graduated)
        (push 'high-school-diploma (emacslife-character-degrees char))
        (emacslife-log char "Graduated high school!" :good))
      t)))

(emacslife-register-activity
 :id 'study-hard :label "Study harder" :category 'education :min-age 6
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(elementary middle-school high-school
                                          community university
                                          grad-school med-school law-school)))
 :description "Smarts +, GPA +"
 :handler
 (lambda (char)
   (emacslife-bump-stat char :smarts (+ 2 (random 4)))
   (setf (emacslife-character-gpa char)
         (min 4.0 (+ (emacslife-character-gpa char) 0.1)))
   (emacslife-log char "Studied hard. Smarts up, GPA bumped." :good)
   (message "Books cracked.")))

(emacslife-register-activity
 :id 'skip-class :label "Skip class" :category 'education :min-age 12
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(middle-school high-school community
                                             university grad-school)))
 :description "Happiness +, GPA -"
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 3)
   (setf (emacslife-character-gpa char)
         (max 0.0 (- (emacslife-character-gpa char) 0.15)))
   (emacslife-log char "Skipped class. Felt great until grades arrived." :funny)
   (message "Hooky achieved.")))

(emacslife-register-activity
 :id 'join-club :label "Join a club" :category 'education :min-age 8
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(elementary middle-school high-school
                                          community university)))
 :description "Happiness + smarts"
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 3)
   (emacslife-bump-stat char :smarts 1)
   (emacslife-log char "Joined a club. Made some friends." :good)
   (when (emacslife-roll 0.3)
     (emacslife--make-npc char :relation 'friend
                          :age (emacslife-character-age char)))
   (message "Clubbed.")))

(emacslife-register-activity
 :id 'run-president :label "Run for class president" :category 'education
 :min-age 12
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(middle-school high-school university)))
 :description "Win unlocks +looks +happiness"
 :handler
 (lambda (char)
   (let* ((chance (/ (+ (emacslife-character-smarts char)
                        (emacslife-character-looks char))
                     200.0)))
     (if (emacslife-roll chance)
         (progn
           (emacslife-bump-stat char :looks 3)
           (emacslife-bump-stat char :happiness 8)
           (emacslife-log char "Elected class president!" :good)
           (message "Sash unlocked."))
       (emacslife-bump-stat char :happiness -3)
       (emacslife-log char "Lost the class-president election." :bad)
       (message "Cried in the bathroom.")))))

(emacslife-register-activity
 :id 'apply-college :label "Apply to university" :category 'education
 :min-age 17
 :requires (lambda (c) (and (eq (emacslife-character-education-stage c)
                                'graduated)
                            (null (emacslife-character-major c))))
 :description "Need GPA + Smarts"
 :handler
 (lambda (char)
   (let* ((score (+ (emacslife-character-smarts char)
                    (* 20 (emacslife-character-gpa char))))
          (admitted (>= score 80))
          (majors '("Computer Science" "English" "Biology" "History"
                    "Engineering" "Business" "Art" "Philosophy"
                    "Psychology" "Physics" "Math" "Music")))
     (if admitted
         (let ((m (completing-read "Major: " majors nil t)))
           (setf (emacslife-character-major char) m)
           (setf (emacslife-character-education-stage char) 'university)
           (setf (emacslife-character-gpa char) 3.0)
           (emacslife-log char (format "Admitted to university (major: %s)." m)
                          :good)
           (message "College bound."))
       (emacslife-bump-stat char :happiness -5)
       (emacslife-log char "University application rejected." :bad)
       (message "Try community college.")))))

(emacslife-register-activity
 :id 'community-college :label "Enroll in community college" :category 'education
 :min-age 17
 :requires (lambda (c) (and (eq (emacslife-character-education-stage c)
                                'graduated)
                            (null (emacslife-character-major c))))
 :description "Easy entry."
 :handler
 (lambda (char)
   (setf (emacslife-character-education-stage char) 'community)
   (setf (emacslife-character-major char) "General Studies")
   (setf (emacslife-character-gpa char) 2.5)
   (emacslife-log char "Enrolled in community college." :neutral)
   (message "It counts.")))

(emacslife-register-activity
 :id 'graduate-uni :label "Graduate university" :category 'education
 :min-age 21
 :requires (lambda (c) (and (memq (emacslife-character-education-stage c)
                                  '(university community))
                            (>= (emacslife-character-age c) 21)))
 :description "Requires GPA >= 2.0"
 :handler
 (lambda (char)
   (if (>= (emacslife-character-gpa char) 2.0)
       (progn
         (push 'bachelors (emacslife-character-degrees char))
         (setf (emacslife-character-education-stage char) 'graduated)
         (emacslife-bump-stat char :smarts 5)
         (emacslife-bump-stat char :happiness 10)
         (emacslife-log char "Graduated university!" :good)
         (message "Diploma earned."))
     (emacslife-log char "Flunked out. GPA too low to graduate." :bad)
     (setf (emacslife-character-education-stage char) 'graduated)
     (message "Try grad school?"))))

(emacslife-register-activity
 :id 'apply-grad :label "Apply to grad school" :category 'education :min-age 22
 :requires (lambda (c) (memq 'bachelors (emacslife-character-degrees c)))
 :description "Med/Law/PhD."
 :handler
 (lambda (char)
   (let ((path (completing-read "Grad path: " '("MBA" "PhD" "Med" "Law") nil t)))
     (setf (emacslife-character-education-stage char)
           (pcase path ("Med" 'med-school) ("Law" 'law-school) (_ 'grad-school)))
     (setf (emacslife-character-gpa char) 3.5)
     (emacslife-log char (format "Started %s." path) :good))))

(emacslife-register-activity
 :id 'graduate-grad :label "Graduate grad school" :category 'education
 :min-age 24
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(grad-school med-school law-school)))
 :description "Requires GPA >= 3.0"
 :handler
 (lambda (char)
   (if (>= (emacslife-character-gpa char) 3.0)
       (let ((stage (emacslife-character-education-stage char)))
         (push (pcase stage
                 ('med-school 'md) ('law-school 'jd) (_ 'graduate))
               (emacslife-character-degrees char))
         (setf (emacslife-character-education-stage char) 'graduated)
         (emacslife-bump-stat char :smarts 8)
         (emacslife-log char "Graduate degree earned!" :good)
         (message "Doctor (of something) now."))
     (emacslife-log char "Failed grad school." :bad))))

(emacslife-register-activity
 :id 'drop-out :label "Drop out of school" :category 'education :min-age 15
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(high-school university community
                                           grad-school med-school law-school)))
 :description "Free time, bad future."
 :handler
 (lambda (char)
   (setf (emacslife-character-education-stage char) 'none)
   (setf (emacslife-character-gpa char) 0.0)
   (emacslife-bump-stat char :happiness 5)
   (emacslife-log char "Dropped out of school. Mom is disappointed." :funny)
   (message "Free at last.")))

;;; -----------------------------------------------------------------
;;; Pets

(emacslife-register-activity
 :id 'adopt-pet :label "Adopt a pet ($300)" :category 'pet :cost 300 :min-age 8
 :description "Pick a species and name."
 :handler
 (lambda (char)
   (let* ((species (completing-read "Species: "
                                    '("Dog" "Cat" "Fish" "Bird" "Reptile" "Horse")
                                    nil t))
          (name (read-string "Pet name: ")))
     (push (list :species species :name name :age 0 :alive t :happiness 70)
           (emacslife-character-pets char))
     (emacslife-bump-stat char :happiness 8)
     (emacslife-log char (format "Adopted %s the %s." name species) :good)
     (message "Pet acquired."))))

;;; -----------------------------------------------------------------
;;; Leisure — going out, having fun, possibly meeting people
;;;
;;; The hallmark of this category is the chance to bump into a new
;;; NPC (`emacslife-meet-someone') as a side effect of just being
;;; out in the world.

(defun emacslife--maybe-meet (char chance where)
  "With probability CHANCE (0..1), spawn a new crush NPC met at WHERE.
Slightly biased toward higher meet chance for higher Looks."
  (let* ((looks (emacslife-character-looks char))
         (boost (* 0.002 (- looks 50)))   ; +/-0.10 at extremes
         (final (max 0.0 (min 1.0 (+ chance boost)))))
    (when (emacslife-roll final)
      (emacslife-meet-someone char where))))

(emacslife-register-activity
 :id 'coffee-shop :label "Go to a coffee shop" :category 'leisure
 :min-age 12 :cost 8
 :description "Sit with a book. Tiny chance to meet someone."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 2)
   (emacslife--maybe-meet char 0.10 "coffee shop")
   (emacslife-log char "Went to the coffee shop." :good)))

(emacslife-register-activity
 :id 'go-movies :label "Go to the movies" :category 'leisure
 :min-age 6 :cost 18
 :description "Happiness +. Small chance to meet someone after the show."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 4)
   (emacslife--maybe-meet char 0.08 "movie theater")
   (emacslife-log char "Went to the movies." :good)))

(emacslife-register-activity
 :id 'hang-with-friends :label "Hang out with friends" :category 'leisure
 :min-age 8
 :description "Free + cheerful.  Bumps a random friend's bar too."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 5)
   (let ((friends (cl-remove-if-not
                    (lambda (n) (and (emacslife-npc-alive n)
                                     (memq (emacslife-npc-relation n)
                                           '(friend bestfriend))))
                    (mapcar #'cdr (emacslife-character-npcs char)))))
     (when friends
       (let ((f (emacslife-pick friends)))
         (emacslife-bump-bar f 5)
         (emacslife-log char (format "Hung out with %s." (emacslife-npc-name f))
                        :good))
       (when (null friends)
         (emacslife-log char "Hung out with friends." :good))))))

(emacslife-register-activity
 :id 'go-park :label "Go to the park" :category 'leisure
 :min-age 3
 :description "Free.  Sunshine and small chance to meet someone."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 3)
   (emacslife-bump-stat char :health 1)
   (emacslife--maybe-meet char 0.07 "park")
   (emacslife-log char "Spent the afternoon at the park." :good)))

(emacslife-register-activity
 :id 'restaurant :label "Go to a nice restaurant" :category 'leisure
 :min-age 10 :cost 75
 :description "Happiness ++. Tiny meet chance."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 6)
   (emacslife--maybe-meet char 0.05 "restaurant")
   (emacslife-log char "Nice dinner out." :good)))

(emacslife-register-activity
 :id 'go-bar :label "Go out to a bar" :category 'leisure
 :min-age 18 :cost 35
 :requires (lambda (c) (emacslife-can-drink-p c))
 :description "Happiness ++. Good chance to meet someone."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 5)
   (emacslife--maybe-meet char 0.35 "bar")
   (when (emacslife-roll 0.10)
     (emacslife-bump-stat char :health -2)
     (emacslife-log char "Hangover the next morning was rough." :bad))
   (emacslife-log char "Bar night." :good)))

(emacslife-register-activity
 :id 'go-clubbing :label "Go clubbing" :category 'leisure
 :min-age 18 :cost 60
 :requires (lambda (c) (emacslife-can-drink-p c))
 :description "Happiness +++. Highest chance to meet someone."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 8)
   (emacslife--maybe-meet char 0.50 "the club")
   (when (emacslife-roll 0.15)
     (emacslife-bump-stat char :health -3)
     (emacslife-log char "Ears ringing for three days." :neutral))
   (when (emacslife-roll 0.05)
     (emacslife-bump-money char -200)
     (emacslife-log char "Bought way too many bottle services." :bad))
   (emacslife-log char "Night out at the club." :good)))

(emacslife-register-activity
 :id 'house-party :label "Go to a house party" :category 'leisure
 :min-age 14 :cost 10
 :description "Happiness ++.  Solid chance to meet someone."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 6)
   (emacslife--maybe-meet char 0.30 "house party")
   (when (and (< (emacslife-character-age char) 21)
              (emacslife-roll 0.05))
     (push (list :crime 'minor-in-poss :sentence 0 :year
                 (+ (emacslife-character-birth-year char)
                    (emacslife-character-age char)))
           (emacslife-character-jail-record char))
     (emacslife-log char "Cops broke up the party. You got a ticket." :bad))
   (emacslife-log char "House party." :good)))

(emacslife-register-activity
 :id 'concert :label "Go to a concert" :category 'leisure
 :min-age 12 :cost 90
 :description "Happiness +++. Meet chance."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 10)
   (emacslife--maybe-meet char 0.20 "concert")
   (emacslife-log char "Live music hit different tonight." :good)))

(emacslife-register-activity
 :id 'karaoke :label "Karaoke night" :category 'leisure
 :min-age 16 :cost 25
 :description "Happiness ++. Meet chance."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 6)
   (emacslife--maybe-meet char 0.20 "karaoke bar")
   (when (>= (emacslife-skill char 'music) 30)
     (emacslife-bump-stat char :happiness 3)
     (emacslife-log char "Slayed your karaoke set." :good))
   (emacslife-log char "Karaoke night." :good)))

(emacslife-register-activity
 :id 'comedy-show :label "Go to a comedy show" :category 'leisure
 :min-age 16 :cost 40
 :description "Happiness ++."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 6)
   (emacslife--maybe-meet char 0.10 "comedy club")
   (emacslife-log char "Saw a great comedy show." :good)))

(emacslife-register-activity
 :id 'sports-game :label "Go to a sporting event" :category 'leisure
 :min-age 8 :cost 80
 :description "Happiness ++. Meet a fellow fan."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 7)
   (emacslife--maybe-meet char 0.15 "the game")
   (emacslife-log char "Caught the game live." :good)))

(emacslife-register-activity
 :id 'museum :label "Visit a museum" :category 'leisure
 :min-age 5 :cost 25
 :description "Happiness + smarts +. Small meet chance."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 3)
   (emacslife-bump-stat char :smarts 2)
   (emacslife--maybe-meet char 0.06 "museum")
   (emacslife-log char "Walked the galleries." :good)))

(emacslife-register-activity
 :id 'art-gallery :label "Art gallery opening" :category 'leisure
 :min-age 18 :cost 30
 :description "Happiness +. Meet a cultured stranger."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 3)
   (emacslife-bump-stat char :smarts 1)
   (emacslife--maybe-meet char 0.15 "art gallery")
   (emacslife-log char "Pretentious art talk all night." :good)))

(emacslife-register-activity
 :id 'theme-park :label "Theme park day" :category 'leisure
 :min-age 5 :cost 130
 :description "Happiness +++."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 12)
   (emacslife--maybe-meet char 0.10 "theme park")
   (emacslife-log char "Theme park.  Worth every dollar." :good)))

(emacslife-register-activity
 :id 'beach-day :label "Beach day" :category 'leisure
 :min-age 5 :cost 20
 :description "Happiness + health."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 7)
   (emacslife-bump-stat char :health 2)
   (emacslife--maybe-meet char 0.12 "beach")
   (emacslife-log char "Sunburn but happy." :good)))

(emacslife-register-activity
 :id 'shopping :label "Shopping spree" :category 'leisure
 :min-age 12 :cost 250
 :description "Happiness + looks +."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 6)
   (emacslife-bump-stat char :looks 2)
   (emacslife-log char "Retail therapy applied." :good)))

(emacslife-register-activity
 :id 'brewery-tour :label "Brewery tour" :category 'leisure
 :min-age 18 :cost 55
 :requires (lambda (c) (emacslife-can-drink-p c))
 :description "Happiness ++. Meet chance."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 5)
   (emacslife--maybe-meet char 0.15 "brewery")
   (emacslife-log char "Brewery tour, three free samples." :good)))

(emacslife-register-activity
 :id 'game-night :label "Board-game night" :category 'leisure
 :min-age 6 :cost 12
 :description "Happiness +."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 4)
   (emacslife--maybe-meet char 0.08 "game night")
   (emacslife-log char "Stayed up late playing board games." :good)))

(emacslife-register-activity
 :id 'meet-people :label "Try to meet someone new" :category 'leisure
 :min-age 14
 :description "Intentional meet attempt — moderate chance + minor happiness."
 :handler
 (lambda (char)
   (emacslife-bump-stat char :happiness 1)
   (cond
    ((emacslife--maybe-meet char 0.45 "out and about")
     (message "You hit it off with someone."))
    (t
     (emacslife-log char "Tried to meet people. Crickets." :neutral)
     (message "No luck this time.")))))

;;; -----------------------------------------------------------------
;;; Convenience dispatcher for the hub

(defun emacslife-leisure-menu ()
  "Pick a leisure / social activity to do."
  (interactive)
  (emacslife-pick-activity-and-run emacslife--state 'leisure))

(emacslife-register-action :leisure (lambda (_) (emacslife-leisure-menu)))

(provide 'emacslife-activities)
;;; emacslife-activities.el ends here
