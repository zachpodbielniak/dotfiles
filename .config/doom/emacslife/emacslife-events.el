;;; emacslife-events.el --- EmacsLife: random life-event engine -*- lexical-binding: t; -*-
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
;; Weighted random event engine.  Each event is a plist with gates
;; (min/max age, marital status, has-job, country) plus a list of
;; choice → outcome handlers.  Events fire 0..N times per age-up
;; (configurable via `emacslife-events-per-year-range').
;;
;; The catalog below leans heavily on absurd comedy — BitLife's
;; signature voice.  Drop a new `emacslife-register-event' call
;; anywhere to add to the random pool.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-family)
(require 'emacslife-relationships)

;;; -----------------------------------------------------------------
;;; Registry

(defvar emacslife--events nil
  "Alist (ID . PLIST) for registered random events.
Plist keys:
  :title string         short headline
  :body string          longer prompt text
  :weight int           random selection weight
  :tone keyword         :good :bad :neutral :funny
  :min-age int          inclusive
  :max-age int|nil      inclusive
  :requires fn(char)→bool
  :auto bool            if t, fire without modal (just message)
  :choices list of (CHOICE-LABEL . HANDLER)")

(cl-defun emacslife-register-event
    (&key id title body weight tone min-age max-age requires auto choices)
  (setf (alist-get id emacslife--events)
        (list :id id :title title :body body
              :weight (or weight 1) :tone (or tone :neutral)
              :min-age (or min-age 0) :max-age max-age
              :requires requires :auto auto
              :choices (or choices '(("OK" . (lambda (_) nil))))))
  id)

;;; -----------------------------------------------------------------
;;; Per-event cooldown tracking

(defun emacslife--event-last-fired (char id)
  "Return the age at which event ID last fired for CHAR, or nil."
  (cdr (assq id (plist-get (emacslife-character-metadata char)
                           :event-last-year))))

(defun emacslife--event-on-cooldown-p (char id)
  "Non-nil if event ID's cooldown hasn't elapsed for CHAR."
  (when-let* ((last (emacslife--event-last-fired char id)))
    (< (- (emacslife-character-age char) last)
       emacslife-event-cooldown-years)))

(defun emacslife--mark-event-fired (char id)
  "Record that event ID fired this year for CHAR."
  (let* ((md (or (emacslife-character-metadata char) '()))
         (tracker (or (plist-get md :event-last-year) '())))
    (setf (alist-get id tracker) (emacslife-character-age char))
    (setf (emacslife-character-metadata char)
          (plist-put md :event-last-year tracker))))

;;; -----------------------------------------------------------------
;;; Applicability + weighted pick

(defun emacslife-applicable-events (char &optional exclude-ids)
  "Events whose gates pass for CHAR.  EXCLUDE-IDS is a list of IDs
to skip (e.g. things already fired this year)."
  (let ((age (emacslife-character-age char)))
    (cl-remove-if-not
     (lambda (cell)
       (let ((id (car cell))
             (p (cdr cell)))
         (and (not (memq id exclude-ids))
              (not (emacslife--event-on-cooldown-p char id))
              (>= age (plist-get p :min-age))
              (or (null (plist-get p :max-age))
                  (<= age (plist-get p :max-age)))
              (let ((r (plist-get p :requires)))
                (or (null r) (funcall r char))))))
     emacslife--events)))

(defun emacslife--pick-event (char &optional exclude-ids)
  "Weighted-pick a random applicable event for CHAR, excluding EXCLUDE-IDS."
  (let* ((apps (emacslife-applicable-events char exclude-ids))
         (weighted (mapcar (lambda (cell)
                             (cons (car cell)
                                   (plist-get (cdr cell) :weight)))
                           apps)))
    (when weighted
      (let ((id (emacslife-weighted-pick weighted)))
        (cdr (assq id emacslife--events))))))

;;; -----------------------------------------------------------------
;;; Event execution

(defun emacslife--run-event (char event)
  "Present EVENT to the user; apply the chosen handler.
Marks the event as fired (for cooldown) in CHAR's metadata."
  (let* ((id (plist-get event :id))
         (title (plist-get event :title))
         (body (plist-get event :body))
         (tone (plist-get event :tone))
         (choices (plist-get event :choices)))
    (emacslife--mark-event-fired char id)
    (if (plist-get event :auto)
        (progn
          (funcall (cdar choices) char)
          (emacslife-log char (format "%s — %s" title (caar choices)) tone))
      (let* ((prompt (format "[ %s ]\n%s" title body))
             (labels (mapcar #'car choices))
             (pick (completing-read (concat prompt "\nChoose: ")
                                    labels nil t)))
        (when pick
          (funcall (cdr (assoc pick choices)) char)
          (emacslife-log char (format "%s — chose: %s" title pick) tone))))))

;;; -----------------------------------------------------------------
;;; Per-year event budget — scales by age band

(defun emacslife--event-chance-this-year (char)
  "Look up the per-year event-fire chance for CHAR's age."
  (let ((age (emacslife-character-age char))
        (best 0.0))
    (dolist (cell emacslife-event-chance-by-age)
      (when (>= age (car cell))
        (setq best (cdr cell))))
    best))

(defun emacslife-fire-events (char)
  "Maybe roll random events for CHAR this year.
Guarantees no event repeats within the same year and respects each
event's cooldown across years."
  (let ((chance (emacslife--event-chance-this-year char))
        (fired-ids '())
        (slots emacslife-event-max-per-year))
    (when (emacslife-roll chance)
      ;; First event guaranteed if we passed the roll
      (when-let* ((event (emacslife--pick-event char fired-ids)))
        (push (plist-get event :id) fired-ids)
        (emacslife--run-event char event)
        (cl-decf slots))
      ;; Each additional event is independently rolled at half base chance
      (while (and (> slots 0) (emacslife-roll (* 0.5 chance)))
        (when-let* ((event (emacslife--pick-event char fired-ids)))
          (push (plist-get event :id) fired-ids)
          (emacslife--run-event char event))
        (cl-decf slots)))))

;;; -----------------------------------------------------------------
;;; ----- COMEDIC / ABSURD EVENTS

(emacslife-register-event
 :id 'severed-finger
 :title "Cat With A Finger"
 :body "Your cat brought home a severed human finger and dropped it at your feet, purring."
 :weight 3 :tone :funny :min-age 8
 :choices
 `(("Pet the cat anyway" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))
   ("Call the police"   . ,(lambda (c)
                             (emacslife-bump-stat c :karma 3)
                             (when (emacslife-roll 0.3)
                               (push (list :crime 'unrelated :sentence 0)
                                     (emacslife-character-jail-record c)))))
   ("Bury it in the yard" . ,(lambda (c) (emacslife-bump-stat c :karma -2)))
   ("Eat it"               . ,(lambda (c)
                                (emacslife-bump-stat c :health -8)
                                (emacslife-bump-stat c :happiness -5)))))

(emacslife-register-event
 :id 'ghost-bedroom
 :title "Spectral Roommate"
 :body "A ghost has begun haunting your bedroom. He says his name is Gary."
 :weight 2 :tone :funny :min-age 10
 :choices
 `(("Befriend Gary" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Hire an exorcist ($800)" . ,(lambda (c)
                                   (emacslife-bump-money c -800)
                                   (emacslife-bump-stat c :happiness 3)))
   ("Charge Gary rent" . ,(lambda (c) (emacslife-bump-money c 500)))
   ("Move out" . ,(lambda (c) (emacslife-bump-money c -2000)))))

(emacslife-register-event
 :id 'alien-abduction
 :title "Alien Abduction"
 :body "Aliens abducted you for a long weekend.  They gave you a hat."
 :weight 1 :tone :funny :min-age 15
 :choices
 `(("Wear the hat proudly" . ,(lambda (c)
                                (emacslife-bump-stat c :looks 4)
                                (emacslife-bump-stat c :happiness 5)))
   ("Sell the hat ($5000)" . ,(lambda (c) (emacslife-bump-money c 5000)))
   ("Worship the hat" . ,(lambda (c) (emacslife-bump-stat c :karma -10)))
   ("Pretend it never happened" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'dad-underwear
 :title "Underwear Mishap"
 :body "You were caught wearing your dad's underwear at a college party."
 :weight 2 :tone :funny :min-age 16
 :max-age 30
 :choices
 `(("Own it confidently" . ,(lambda (c)
                              (emacslife-bump-stat c :looks 2)
                              (emacslife-bump-stat c :happiness 3)))
   ("Flee in shame" . ,(lambda (c) (emacslife-bump-stat c :happiness -5)))
   ("Set them on fire (still wearing them)" .
    ,(lambda (c) (emacslife-bump-stat c :health -10)))))

(emacslife-register-event
 :id 'fanfiction-discovered
 :title "Fanfic Discovered"
 :body "Your boss found your 80,000-word Star Wars fan-fiction account."
 :weight 2 :tone :funny :min-age 22
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Demand a promotion" .
    ,(lambda (c) (when (emacslife-character-job c)
                   (let ((j (emacslife-character-job c)))
                     (setf (plist-get j :performance) 90)
                     (setf (emacslife-character-job c) j)))))
   ("Resign in disgrace" .
    ,(lambda (c) (setf (emacslife-character-job c) nil)))
   ("Brag about your word count" .
    ,(lambda (c) (emacslife-bump-stat c :happiness 5)))))

(emacslife-register-event
 :id 'ex-lottery
 :title "Ex Wins Your Lottery"
 :body "You won the lottery — but the ticket was in your ex's name."
 :weight 1 :tone :funny :min-age 20
 :requires (lambda (c) (emacslife-character-ex-spouses c))
 :choices
 `(("Sue them" . ,(lambda (c)
                    (emacslife-bump-money c -10000)
                    (when (emacslife-roll 0.3)
                      (emacslife-bump-money c 500000))))
   ("Beg them" . ,(lambda (c) (emacslife-bump-stat c :happiness -10)))
   ("Move on" . ,(lambda (c) (emacslife-bump-stat c :karma 3)))))

(emacslife-register-event
 :id 'interview-sneeze
 :title "Interview Disaster"
 :body "You sneezed hard during a job interview.  Things came out."
 :weight 2 :tone :funny :min-age 18
 :choices
 `(("Apologize calmly" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Demand a higher salary anyway" . ,(lambda (c) (emacslife-bump-money c 2000)))
   ("Walk out" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'penguin-stalker
 :title "Penguin Stalker"
 :body "A penguin appears to be following you everywhere.  It is wearing a tiny hat."
 :weight 1 :tone :funny :min-age 5
 :choices
 `(("Befriend the penguin" . ,(lambda (c) (emacslife-bump-stat c :happiness 8)))
   ("Outrun the penguin" . ,(lambda (c) (emacslife-bump-stat c :health 1)))
   ("Adopt the penguin" . ,(lambda (c)
                             (push (list :species "Penguin" :name "Stalker"
                                         :age 1 :alive t :happiness 80)
                                   (emacslife-character-pets c))))))

(emacslife-register-event
 :id 'sword-stranger
 :title "Stranger With A Sword"
 :body "A man with a katana asked if you remembered him.  You do not."
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Apologize profusely" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Pretend to remember" . ,(lambda (c) (emacslife-bump-stat c :smarts 2)))
   ("Run away" . ,(lambda (c) (emacslife-bump-stat c :health -1)))
   ("Challenge him to duel" . ,(lambda (c)
                                 (if (emacslife-roll 0.4)
                                     (progn (emacslife-bump-stat c :looks 10)
                                            (emacslife-bump-stat c :happiness 10))
                                   (emacslife-bump-stat c :health -25))))))

(emacslife-register-event
 :id 'haunted-vacuum
 :title "Possessed Roomba"
 :body "Your robot vacuum has begun whispering Latin while it cleans."
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Listen carefully" . ,(lambda (c) (emacslife-bump-stat c :smarts 5)))
   ("Unplug it" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))
   ("Sell on eBay ($300)" . ,(lambda (c) (emacslife-bump-money c 300)))))

(emacslife-register-event
 :id 'pyramid-scheme
 :title "Pyramid Scheme Pitch"
 :body "A high-school classmate cornered you at a coffee shop with an Important Opportunity."
 :weight 2 :tone :funny :min-age 22
 :choices
 `(("Join enthusiastically" . ,(lambda (c)
                                 (emacslife-bump-money c -2000)
                                 (when (emacslife-roll 0.1)
                                   (emacslife-bump-money c 8000))))
   ("Roast them publicly" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Pretend to consider it" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))))

(emacslife-register-event
 :id 'fake-relative
 :title "Surprise Sibling"
 :body "Someone claiming to be your long-lost half-sibling showed up at your door."
 :weight 2 :tone :neutral :min-age 18
 :choices
 `(("Welcome them" .
    ,(lambda (c) (emacslife--make-npc c :relation 'halfsibling :age (+ 10 (random 30)))
                 (emacslife-bump-stat c :happiness 3)))
   ("Demand a DNA test ($500)" .
    ,(lambda (c) (emacslife-bump-money c -500)
                 (when (emacslife-roll 0.5)
                   (emacslife--make-npc c :relation 'halfsibling
                                        :age (+ 10 (random 30))))))
   ("Slam the door" .
    ,(lambda (c) (emacslife-bump-stat c :karma -2)))))

;;; ----- ACCIDENTS / HEALTH

(emacslife-register-event
 :id 'car-crash :title "Car Crash"
 :body "You were t-boned at an intersection."
 :weight 3 :tone :bad :min-age 16
 :choices
 `(("Sue the other driver" .
    ,(lambda (c) (when (emacslife-roll 0.4)
                   (emacslife-bump-money c (* 1000 (1+ (random 50)))))))
   ("Accept fault" . ,(lambda (c)
                        (emacslife-bump-money c -5000)
                        (emacslife-bump-stat c :health -10)))
   ("Walk it off" . ,(lambda (c) (emacslife-bump-stat c :health -15)))))

(emacslife-register-event
 :id 'lightning
 :title "Struck by Lightning"
 :body "You were struck by lightning during a walk.  You are surprisingly OK."
 :weight 1 :tone :neutral :min-age 6
 :choices
 `(("Joke about superpowers" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Sue the sky" . ,(lambda (c) (emacslife-bump-stat c :smarts -1)))
   ("Go to the hospital" .
    ,(lambda (c) (emacslife-bump-money c -1500)
                 (emacslife-bump-stat c :health 5)))))

(emacslife-register-event
 :id 'flu :title "You Caught The Flu"
 :body "You feel terrible.  Hot tea is involved."
 :weight 1 :tone :bad :min-age 5
 :requires (lambda (c)
             ;; Flu fires only when the immune system is plausibly compromised
             (or (< (emacslife-character-health c) 65)
                 (> (emacslife-character-age c) 65)))
 :choices
 `(("Tough it out" . ,(lambda (c) (emacslife-bump-stat c :health -5)))
   ("Go to the doctor ($150)" .
    ,(lambda (c) (emacslife-bump-money c -150) (emacslife-bump-stat c :health 1)))))

(emacslife-register-event
 :id 'cancer :title "Cancer Diagnosis"
 :body "A routine checkup uncovered cancer.  Treatment options exist."
 :weight 1 :tone :bad :min-age 40
 :choices
 `(("Aggressive treatment ($80k)" .
    ,(lambda (c) (emacslife-bump-money c -80000)
                 (if (emacslife-roll 0.75)
                     (progn (emacslife-bump-stat c :health 15)
                            (push 'cancer-survivor (emacslife-character-achievements c)))
                   (setf (emacslife-character-alive c) nil)
                   (setf (emacslife-character-death-cause c) "cancer"))))
   ("Alternative remedies" .
    ,(lambda (c) (when (emacslife-roll 0.3)
                   (setf (emacslife-character-alive c) nil)
                   (setf (emacslife-character-death-cause c) "untreated cancer"))))))

;;; ----- HERO PATH

(emacslife-register-event
 :id 'save-drowning :title "Drowning Kid"
 :body "A child is drowning in a hotel pool while parents look at phones."
 :weight 2 :tone :good :min-age 15
 :choices
 `(("Dive in to save them" .
    ,(lambda (c) (emacslife-bump-stat c :karma 12)
                 (push 'hero-rescue (emacslife-character-achievements c))
                 (emacslife-bump-stat c :happiness 10)))
   ("Call lifeguard" .
    ,(lambda (c) (emacslife-bump-stat c :karma 3)))
   ("Film it for TikTok" .
    ,(lambda (c) (emacslife-bump-stat c :karma -10)
                 (setf (emacslife-character-fame c)
                       (emacslife-clamp (+ (emacslife-character-fame c) 5)))))))

(emacslife-register-event
 :id 'fire-rescue :title "Burning Building"
 :body "Your neighbor's apartment is on fire and they're inside."
 :weight 1 :tone :good :min-age 18
 :choices
 `(("Rush in" . ,(lambda (c)
                   (if (emacslife-roll 0.6)
                       (progn (emacslife-bump-stat c :karma 15)
                              (push 'hero-rescue (emacslife-character-achievements c))
                              (emacslife-bump-stat c :happiness 15))
                     (emacslife-bump-stat c :health -30))))
   ("Call 911 and wait" . ,(lambda (c) (emacslife-bump-stat c :karma 2)))))

;;; ----- ROMANCE / DRAMA

(emacslife-register-event
 :id 'bar-flirt :title "Stranger At Bar"
 :body "Someone attractive is making eyes at you across the bar."
 :weight 3 :tone :neutral :min-age 18
 :choices
 `(("Buy them a drink" .
    ,(lambda (c) (emacslife-bump-money c -20)
                 (when (emacslife-roll 0.5)
                   (emacslife-meet-someone c "bar"))))
   ("Ignore them" . ,(lambda (_) nil))))

(emacslife-register-event
 :id 'ex-text :title "Ex Texted You"
 :body "Your ex texted: 'u up?'"
 :weight 3 :tone :neutral :min-age 18
 :requires (lambda (c) (emacslife-character-ex-spouses c))
 :choices
 `(("Reply yes" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)
                                (emacslife-bump-stat c :karma -2)))
   ("Reply no" . ,(lambda (c) (emacslife-bump-stat c :karma 3)))
   ("Block them" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)))))

(emacslife-register-event
 :id 'partner-cheats :title "Partner Caught Cheating"
 :body "Your spouse was photographed with someone else."
 :weight 1 :tone :bad :min-age 22
 :requires (lambda (c) (emacslife-character-spouse c))
 :choices
 `(("Confront them" .
    ,(lambda (c) (emacslife-bump-stat c :happiness -10)))
   ("Cheat back" .
    ,(lambda (c) (emacslife-bump-stat c :karma -5)
                 (emacslife-bump-stat c :happiness 5)))
   ("File for divorce" .
    ,(lambda (c) (when (emacslife-character-spouse c)
                   (let ((sp (emacslife-npc-by-id c (emacslife-character-spouse c))))
                     (when sp
                       (setf (emacslife-npc-relation sp) 'ex)
                       (push (emacslife-npc-id sp)
                             (emacslife-character-ex-spouses c))
                       (setf (emacslife-character-spouse c) nil)
                       (emacslife-bump-money c -10000))))))))

;;; ----- WORK

(emacslife-register-event
 :id 'surprise-bonus :title "Surprise Bonus"
 :body "Your boss handed you an unexpected envelope."
 :weight 2 :tone :good :min-age 18
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Cash it gratefully" .
    ,(lambda (c) (emacslife-bump-money c (* 500 (1+ (random 20))))))
   ("Demand more" .
    ,(lambda (c) (when (emacslife-roll 0.5)
                   (emacslife-bump-money c (* 100 (1+ (random 50)))))))))

(emacslife-register-event
 :id 'layoff :title "Layoffs"
 :body "The company is downsizing.  You're affected."
 :weight 1 :tone :bad :min-age 20
 :requires (lambda (c) (emacslife-character-job c))
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (push (append (emacslife-character-job c)
                                       (list :laid-off t))
                              (emacslife-character-job-history c))
                              (setf (emacslife-character-job c) nil)
                              (emacslife-bump-stat c :happiness -10)))))

;;; ----- ANIMAL ENCOUNTERS

(emacslife-register-event
 :id 'raccoon-trash :title "Raccoon Diplomat"
 :body "A raccoon is going through your trash and making direct eye contact."
 :weight 2 :tone :funny :min-age 12
 :choices
 `(("Negotiate with the raccoon" . ,(lambda (c) (emacslife-bump-stat c :happiness 4)))
   ("Charge at it" . ,(lambda (c) (when (emacslife-roll 0.3)
                                    (emacslife-bump-stat c :health -10))))
   ("Feed it" . ,(lambda (c) (push (list :species "Raccoon" :name "Banditos"
                                         :age 2 :alive t :happiness 80)
                                   (emacslife-character-pets c))))))

(emacslife-register-event
 :id 'bee-attack :title "Bee Swarm"
 :body "You disturbed a hive."
 :weight 2 :tone :bad :min-age 5
 :choices
 `(("Run" . ,(lambda (c) (emacslife-bump-stat c :health -3)))
   ("Cannonball into pool" . ,(lambda (c) (emacslife-bump-stat c :health -1)))
   ("Negotiate" . ,(lambda (c) (emacslife-bump-stat c :health -15)))))

;;; ----- CRIME VICTIM

(emacslife-register-event
 :id 'mugged :title "Got Mugged"
 :body "Someone is pointing a knife at you on a poorly-lit street."
 :weight 2 :tone :bad :min-age 14
 :choices
 `(("Hand over wallet" . ,(lambda (c) (emacslife-bump-money c
                                                            (- (min 500 (emacslife-character-cash c))))))
   ("Fight back" . ,(lambda (c) (if (emacslife-roll 0.4)
                                     (emacslife-bump-stat c :looks 2)
                                   (emacslife-bump-stat c :health -20))))
   ("Run" . ,(lambda (c) (when (emacslife-roll 0.5)
                           (emacslife-bump-money c (- (min 200 (emacslife-character-cash c)))))))))

(emacslife-register-event
 :id 'identity-theft :title "Identity Theft"
 :body "Someone has been opening credit cards in your name."
 :weight 1 :tone :bad :min-age 18
 :choices
 `(("Fix it (months of paperwork)" .
    ,(lambda (c) (emacslife-bump-stat c :happiness -8)))
   ("Embrace the chaos" .
    ,(lambda (c) (setf (emacslife-character-credit-score c) 350)))))

;;; ----- CURRENT EVENTS / QUIRKY

(emacslife-register-event
 :id 'tabloid :title "Tabloid Headline"
 :body "A tabloid is running a story about you with a deeply unflattering photo."
 :weight 1 :tone :funny :min-age 18
 :requires (lambda (c) (>= (emacslife-character-fame c) 20))
 :choices
 `(("Sue ($30k upfront)" .
    ,(lambda (c) (emacslife-bump-money c -30000)
                 (when (emacslife-roll 0.5) (emacslife-bump-money c 200000))))
   ("Make a joke about it" .
    ,(lambda (c) (setf (emacslife-character-fame c)
                       (emacslife-clamp (+ (emacslife-character-fame c) 5)))))
   ("Pose for a sequel" .
    ,(lambda (c) (emacslife-bump-money c 5000)))))

(emacslife-register-event
 :id 'tiktok-viral
 :title "You Went Viral"
 :body "A video of you doing nothing in particular has 80 million views."
 :weight 1 :tone :funny :min-age 13
 :choices
 `(("Become an influencer" .
    ,(lambda (c) (setf (emacslife-character-fame c)
                       (emacslife-clamp (+ (emacslife-character-fame c) 30)))
                 (emacslife-bump-money c 8000)))
   ("Delete everything" .
    ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Sell unrelated merchandise" .
    ,(lambda (c) (emacslife-bump-money c 12000)))))

(emacslife-register-event
 :id 'cult-recruiter
 :title "Cult Recruiter"
 :body "A stranger says you are 'glowing' and would you like to learn more about a 'community.'"
 :weight 2 :tone :funny :min-age 18
 :choices
 `(("Definitely yes" . ,(lambda (c) (emacslife-bump-stat c :karma -5)
                                     (emacslife-bump-money c -2000)))
   ("Definitely no" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Counter-recruit them to YOUR cult" .
    ,(lambda (c) (emacslife-bump-money c 1000)))))

(emacslife-register-event
 :id 'helicopter
 :title "Helicopter In Your Yard"
 :body "A helicopter has landed in your yard.  Three men in suits get out."
 :weight 1 :tone :funny :min-age 25
 :choices
 `(("Wave politely" . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))
   ("Hide" . ,(lambda (c) (emacslife-bump-stat c :health -1)))
   ("Get in the helicopter" .
    ,(lambda (c) (if (emacslife-roll 0.3)
                     (emacslife-bump-money c 100000)
                   (emacslife-bump-stat c :health -50))))))

;;; ----- CURRENT EVENTS WITH MONEY HOOK

(emacslife-register-event
 :id 'inheritance
 :title "Distant Relative Died"
 :body "A great-aunt you've never met left you a sum of money."
 :weight 1 :tone :good :min-age 18
 :choices
 `(("Spend it on yourself" .
    ,(lambda (c) (emacslife-bump-money c (* 1000 (1+ (random 100))))))
   ("Donate it" .
    ,(lambda (c) (emacslife-bump-stat c :karma 10)))))

(emacslife-register-event
 :id 'random-stranger-gift
 :title "Stranger Gave You Cash"
 :body "A man in a velvet suit handed you cash on the street, said 'You're doing great,' and walked away."
 :weight 1 :tone :funny :min-age 10
 :choices
 `(("Pocket it" . ,(lambda (c) (emacslife-bump-money c 200)))
   ("Chase him down" . ,(lambda (c) (emacslife-bump-stat c :health -1)))
   ("Refuse loudly" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))))

;;; ----- DEPRESSION / ADDICTION

(emacslife-register-event
 :id 'depression :title "Depressive Episode"
 :body "You haven't felt right for months."
 :weight 2 :tone :bad :min-age 15
 :requires (lambda (c) (< (emacslife-character-happiness c) 40))
 :choices
 `(("See a therapist ($200)" .
    ,(lambda (c) (emacslife-bump-money c -200)
                 (emacslife-bump-stat c :happiness 8)))
   ("Try meds ($100)" .
    ,(lambda (c) (emacslife-bump-money c -100)
                 (emacslife-bump-stat c :happiness 6)))
   ("Power through" .
    ,(lambda (c) (emacslife-bump-stat c :happiness -5)))))

;;; ----- CHILDHOOD (ages 0-12) — keeps young lives from feeling empty

(emacslife-register-event
 :id 'lost-tooth :title "Lost A Tooth"
 :body "One of your baby teeth fell out at the worst possible moment."
 :weight 2 :tone :neutral :min-age 5 :max-age 11
 :choices
 `(("Put it under the pillow" . ,(lambda (c) (emacslife-bump-money c 5)))
   ("Show everyone" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))
   ("Swallow it accidentally" . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))))

(emacslife-register-event
 :id 'birthday-disaster :title "Birthday Party Disaster"
 :body "Nobody showed up to your party. The bouncy castle deflated dramatically."
 :weight 2 :tone :funny :min-age 4 :max-age 12
 :choices
 `(("Eat the cake yourself" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)))
   ("Cry about it" . ,(lambda (c) (emacslife-bump-stat c :happiness -5)))
   ("Blame the dog" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))))

(emacslife-register-event
 :id 'imaginary-friend :title "Imaginary Friend"
 :body "You've made an imaginary friend.  They have very strong opinions."
 :weight 2 :tone :funny :min-age 3 :max-age 9
 :choices
 `(("Hang out with them constantly" . ,(lambda (c) (emacslife-bump-stat c :happiness 4)
                                                   (emacslife-bump-stat c :smarts 1)))
   ("Argue with them in public" . ,(lambda (c) (emacslife-bump-stat c :smarts -1)))
   ("Banish them" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

(emacslife-register-event
 :id 'stray-dog :title "Stray Dog Followed You Home"
 :body "A dog with no collar trotted along behind you the whole way back from school."
 :weight 2 :tone :good :min-age 4 :max-age 14
 :choices
 `(("Beg parents to keep it" .
    ,(lambda (c) (if (emacslife-roll 0.5)
                     (progn
                       (push (list :species "Dog" :name "Stray" :age 2 :alive t :happiness 80)
                             (emacslife-character-pets c))
                       (emacslife-bump-stat c :happiness 8))
                   (emacslife-bump-stat c :happiness -3))))
   ("Take it to the shelter" . ,(lambda (c) (emacslife-bump-stat c :karma 3)))
   ("Ignore it" . ,(lambda (c) (emacslife-bump-stat c :karma -2)))))

(emacslife-register-event
 :id 'bully :title "School Bully"
 :body "Older kid keeps shoving you in the hallway."
 :weight 2 :tone :bad :min-age 6 :max-age 14
 :choices
 `(("Tell a teacher" . ,(lambda (c) (when (emacslife-roll 0.6)
                                      (emacslife-bump-stat c :happiness 3))))
   ("Stand up to them" . ,(lambda (c) (if (emacslife-roll 0.4)
                                           (emacslife-bump-stat c :happiness 5)
                                         (emacslife-bump-stat c :health -3))))
   ("Hide in the bathroom" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))))

(emacslife-register-event
 :id 'spelling-bee :title "Spelling Bee"
 :body "You're competing in the regional spelling bee."
 :weight 1 :tone :neutral :min-age 7 :max-age 13
 :choices
 `(("Spell aggressively" .
    ,(lambda (c) (if (emacslife-roll (/ (emacslife-character-smarts c) 100.0))
                     (progn (emacslife-bump-stat c :smarts 4)
                            (emacslife-bump-stat c :happiness 6))
                   (emacslife-bump-stat c :happiness -3))))
   ("Forfeit" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

(emacslife-register-event
 :id 'forgot-homework :title "Forgot Your Homework"
 :body "Twelve pages of fractions, due today, sitting on the kitchen counter."
 :weight 2 :tone :funny :min-age 6 :max-age 15
 :choices
 `(("Make up an elaborate lie" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)
                                              (emacslife-bump-stat c :karma -1)))
   ("Tell the truth" . ,(lambda (c) (emacslife-bump-stat c :karma 2)))
   ("Blame the cat" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))))

(emacslife-register-event
 :id 'goldfish-died :title "Pet Goldfish Died"
 :body "Bubbles the goldfish has gone belly-up after one day."
 :weight 2 :tone :funny :min-age 3 :max-age 14
 :choices
 `(("Hold a funeral" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))
   ("Flush it without ceremony" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))
   ("Demand a refund" . ,(lambda (c) (emacslife-bump-money c 5)))))

(emacslife-register-event
 :id 'stuck-bead :title "Bead Up The Nose"
 :body "There is a small plastic bead in your left nostril."
 :weight 1 :tone :funny :min-age 2 :max-age 7
 :choices
 `(("Tell mom" . ,(lambda (c) (emacslife-bump-money c -200)))
   ("Pretend it isn't there" . ,(lambda (c) (emacslife-bump-stat c :health -2)))
   ("Push it deeper" . ,(lambda (c) (emacslife-bump-stat c :health -5)
                                    (emacslife-bump-money c -500)))))

(emacslife-register-event
 :id 'treehouse :title "Built A Treehouse"
 :body "You and the neighbor kids assembled a structurally questionable treehouse."
 :weight 2 :tone :good :min-age 6 :max-age 13
 :choices
 `(("Test the floor" . ,(lambda (c) (if (emacslife-roll 0.7)
                                         (emacslife-bump-stat c :happiness 6)
                                       (emacslife-bump-stat c :health -8))))
   ("Charge admission" . ,(lambda (c) (emacslife-bump-money c 25)))
   ("Declare yourself the king" . ,(lambda (c) (emacslife-bump-stat c :happiness 4)))))

(emacslife-register-event
 :id 'lost-at-zoo :title "Lost At The Zoo"
 :body "You're separated from your parents near the reptile house."
 :weight 1 :tone :neutral :min-age 3 :max-age 10
 :choices
 `(("Ask a stranger for help" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Stay put and cry" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Try to break into the reptile house" . ,(lambda (c) (emacslife-bump-stat c :karma -3)
                                                          (emacslife-bump-stat c :health -2)))))

(emacslife-register-event
 :id 'school-play :title "School Play Stage Fright"
 :body "You're on stage in a tree costume.  Everyone is staring."
 :weight 2 :tone :funny :min-age 5 :max-age 12
 :choices
 `(("Improvise dramatically" . ,(lambda (c) (emacslife-bump-stat c :looks 2)
                                            (emacslife-bump-stat c :happiness 4)))
   ("Freeze in place" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Run off stage" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

(emacslife-register-event
 :id 'mystery-rash :title "Mystery Rash"
 :body "You've broken out in something colorful and itchy."
 :weight 1 :tone :bad :min-age 0 :max-age 10
 :choices
 `(("Doctor visit ($120)" .
    ,(lambda (c) (emacslife-bump-money c -120) (emacslife-bump-stat c :health 2)))
   ("Scratch through it" .
    ,(lambda (c) (emacslife-bump-stat c :health -3)
                 (emacslife-bump-stat c :looks -1)))))

(emacslife-register-event
 :id 'baby-cute :title "You Are Adorable"
 :body "Strangers keep telling your parents how cute you are."
 :weight 1 :tone :good :min-age 0 :max-age 4 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :looks 1)))))

(emacslife-register-event
 :id 'first-word :title "Said Your First Word"
 :body "It was, against all expectations, \"butterfly.\""
 :weight 1 :tone :good :min-age 1 :max-age 2 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)
                        (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'baby-laughed :title "Just Existed Happily"
 :body "Today involved a lot of giggling.  And one peanut butter incident."
 :weight 1 :tone :good :min-age 0 :max-age 3 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

;;; ----- FAMILY DRAMA

(emacslife-register-event
 :id 'paternity :title "Paternity Surprise"
 :body "A DNA test reveals your dad isn't your biological dad."
 :weight 1 :tone :neutral :min-age 18
 :choices
 `(("Confront mom" . ,(lambda (c) (emacslife-bump-stat c :happiness -8)))
   ("Find biological dad" .
    ,(lambda (c) (emacslife--make-npc c :relation 'father :age (+ 40 (random 30)))))
   ("Ignore the test" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))))

;;; -----------------------------------------------------------------
;;; ============================================================
;;; EXPANSION PACK — production-grade event variety
;;; Adds ~85 more events spread across age bands & categories so
;;; that the random pool at any given age has 40+ applicable items.
;;; ============================================================

;;; ----- CHILDHOOD II (more 3-12) -----

(emacslife-register-event
 :id 'tooth-fairy-scam :title "Tooth Fairy Conspiracy"
 :body "You woke up and caught your dad swapping the tooth for a quarter."
 :weight 2 :tone :funny :min-age 5 :max-age 10
 :choices
 `(("Pretend not to see" . ,(lambda (c) (emacslife-bump-stat c :smarts 2)))
   ("Demand more money"  . ,(lambda (c) (emacslife-bump-money c 20)
                                        (emacslife-bump-stat c :karma -1)))
   ("Tell your sibling"  . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))))

(emacslife-register-event
 :id 'picked-last :title "Picked Last For Kickball"
 :body "Even Greg picked before you. Greg eats glue."
 :weight 2 :tone :bad :min-age 5 :max-age 12
 :choices
 `(("Become really good at kickball" . ,(lambda (c) (emacslife-bump-stat c :health 3)))
   ("Pretend you don't care"         . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))
   ("Quit team sports forever"       . ,(lambda (c) (emacslife-bump-stat c :health -1)))))

(emacslife-register-event
 :id 'found-five-bucks :title "Found Money"
 :body "You found a $5 bill at the playground.  Score."
 :weight 2 :tone :good :min-age 4 :max-age 12
 :choices
 `(("Pocket it"  . ,(lambda (c) (emacslife-bump-money c 5)))
   ("Find the owner" . ,(lambda (c) (emacslife-bump-stat c :karma 2)))))

(emacslife-register-event
 :id 'carsick :title "Carsick"
 :body "The family road trip is going about as well as expected."
 :weight 1 :tone :bad :min-age 3 :max-age 12
 :choices
 `(("Open the window"  . ,(lambda (c) (emacslife-bump-stat c :health 0)))
   ("Pretend you're fine" . ,(lambda (c) (emacslife-bump-stat c :health -2)))))

(emacslife-register-event
 :id 'halloween-malfunction :title "Halloween Costume Malfunction"
 :body "Your robot costume's wiring got, uh, ambitious."
 :weight 1 :tone :funny :min-age 4 :max-age 13
 :choices
 `(("Commit to the bit" . ,(lambda (c) (emacslife-bump-stat c :happiness 4)))
   ("Run home crying"   . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Try to fix mid-trick-or-treat" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'secret-candy-stash :title "Mom Found The Stash"
 :body "Your sock drawer full of Halloween candy has been discovered."
 :weight 1 :tone :funny :min-age 5 :max-age 12
 :choices
 `(("Deny everything" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))
   ("Confess and share" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Eat the evidence" . ,(lambda (c) (emacslife-bump-stat c :health -3)))))

(emacslife-register-event
 :id 'sleepover-disaster :title "Sleepover Gone Wrong"
 :body "Someone wet the bed and it MIGHT have been you."
 :weight 1 :tone :funny :min-age 5 :max-age 11
 :choices
 `(("Blame the dog"   . ,(lambda (c) (emacslife-bump-stat c :karma -2)))
   ("Pretend to sleepwalk" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Own up to it"   . ,(lambda (c) (emacslife-bump-stat c :karma 2)
                                    (emacslife-bump-stat c :happiness -3)))))

(emacslife-register-event
 :id 'broke-window :title "Broke A Window"
 :body "The baseball went exactly where you didn't intend."
 :weight 1 :tone :bad :min-age 6 :max-age 13
 :choices
 `(("Tell parents immediately" . ,(lambda (c) (emacslife-bump-money c -200)
                                              (emacslife-bump-stat c :karma 2)))
   ("Hide and hope no one notices" . ,(lambda (c) (emacslife-bump-stat c :karma -2)))
   ("Pin it on a neighborhood cat" . ,(lambda (c) (emacslife-bump-stat c :karma -3)))))

(emacslife-register-event
 :id 'flipped-board :title "Board Game Rage"
 :body "You flipped the Monopoly board.  Pieces flew."
 :weight 1 :tone :funny :min-age 5 :max-age 14
 :choices
 `(("Apologize" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Walk away majestically" . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))
   ("Demand a redo" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))))

;;; ----- TEEN (13-17) -----

(emacslife-register-event
 :id 'high-school-crush :title "Crush"
 :body "You can't stop thinking about that one classmate."
 :weight 3 :tone :neutral :min-age 13 :max-age 19
 :choices
 `(("Tell them directly" .
    ,(lambda (c) (if (emacslife-roll (/ (+ (emacslife-character-looks c) 20.0) 150.0))
                     (progn (emacslife-bump-stat c :happiness 8)
                            (emacslife-meet-someone c "school"))
                   (emacslife-bump-stat c :happiness -5))))
   ("Write secret poems" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Pretend to hate them" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

(emacslife-register-event
 :id 'first-kiss :title "First Kiss"
 :body "Behind the bleachers.  It was both magical and very damp."
 :weight 2 :tone :good :min-age 13 :max-age 18 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 10)
                        (emacslife-bump-stat c :looks 1)))))

(emacslife-register-event
 :id 'asked-to-prom :title "Asked To Prom"
 :body "Someone you barely know just asked you to prom in cafeteria."
 :weight 2 :tone :good :min-age 16 :max-age 18
 :choices
 `(("Say yes" . ,(lambda (c) (emacslife-bump-stat c :happiness 6)
                              (emacslife-bump-money c -200)))
   ("Politely decline" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Counter-propose elopement (joking)" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'caught-cheating-test :title "Caught Cheating"
 :body "The teacher saw the answer key written on your arm."
 :weight 2 :tone :bad :min-age 13 :max-age 22
 :requires (lambda (c) (memq (emacslife-character-education-stage c)
                             '(middle-school high-school university)))
 :choices
 `(("Take detention"   . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Argue your case"  . ,(lambda (c) (when (emacslife-roll 0.4)
                                        (emacslife-bump-stat c :smarts 1))))
   ("Cry strategically" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))))

(emacslife-register-event
 :id 'underage-drinking :title "Caught Drinking Underage"
 :body "The cops at the park were not impressed by your wine cooler."
 :weight 2 :tone :bad :min-age 14 :max-age 20
 :choices
 `(("Take the citation" . ,(lambda (c) (emacslife-bump-money c -250)
                                       (push (list :crime 'minor-in-poss
                                                   :sentence 0)
                                             (emacslife-character-jail-record c))))
   ("Run"               . ,(lambda (c) (when (emacslife-roll 0.5)
                                         (emacslife-bump-stat c :health -2))))
   ("Pretend to be older" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'driver-permit :title "Driver's Permit"
 :body "Time to terrify your parent in the passenger seat."
 :weight 2 :tone :neutral :min-age 14 :max-age 18
 :choices
 `(("Drive cautiously" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Floor it"         . ,(lambda (c) (when (emacslife-roll 0.3)
                                        (emacslife-bump-stat c :health -3))))))

(emacslife-register-event
 :id 'failed-road-test :title "Failed The Road Test"
 :body "You hit a cone. Then another cone. The instructor sighed."
 :weight 1 :tone :funny :min-age 16 :max-age 19
 :choices
 `(("Try again next year" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))
   ("Blame the cones"    . ,(lambda (c) (emacslife-bump-stat c :karma -1)))))

(emacslife-register-event
 :id 'fake-id :title "Got A Fake ID"
 :body "You're now 27, named Bartholomew, from North Dakota."
 :weight 1 :tone :funny :min-age 16 :max-age 20
 :choices
 `(("Use it brazenly" . ,(lambda (c) (when (emacslife-roll 0.3)
                                       (push (list :crime 'fraud :sentence 0)
                                             (emacslife-character-jail-record c)))))
   ("Just keep it in wallet" . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))))

(emacslife-register-event
 :id 'snuck-out :title "Snuck Out"
 :body "Out the window at 1am.  Window did not cooperate."
 :weight 2 :tone :funny :min-age 13 :max-age 18
 :choices
 `(("Apologize"        . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Do it again the next night" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)))))

(emacslife-register-event
 :id 'awkward-talk :title "'The Talk'"
 :body "Dad sat you down with diagrams.  HE HAD DIAGRAMS."
 :weight 1 :tone :funny :min-age 11 :max-age 14 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)
                        (emacslife-bump-stat c :happiness -3)))))

(emacslife-register-event
 :id 'dumped-via-text :title "Dumped Via Text"
 :body "It just said 'sry, vibes.'  Three letters less than 'sorry.'"
 :weight 2 :tone :bad :min-age 14 :max-age 30
 :requires (lambda (c) (cl-some (lambda (n)
                                  (memq (emacslife-npc-relation n)
                                        '(partner crush date)))
                                (mapcar #'cdr (emacslife-character-npcs c))))
 :choices
 `(("Eat ice cream" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))
   ("Write a song"  . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Plot revenge"  . ,(lambda (c) (emacslife-bump-stat c :karma -3)))))

;;; ----- YOUNG ADULT (18-29) -----

(emacslife-register-event
 :id 'roommate-rich :title "Rich Roommate"
 :body "Turns out your broke-looking roommate is the heir to a vacuum empire."
 :weight 1 :tone :funny :min-age 18 :max-age 26
 :choices
 `(("Befriend strategically" . ,(lambda (c) (emacslife-bump-money c 2000)
                                            (emacslife-bump-stat c :karma -2)))
   ("Treat them normally"    . ,(lambda (c) (emacslife-bump-stat c :karma 2)))
   ("Move out immediately"   . ,(lambda (c) (emacslife-bump-money c -1500)))))

(emacslife-register-event
 :id 'food-poisoning :title "Food Poisoning"
 :body "The wedding shrimp had its revenge."
 :weight 2 :tone :bad :min-age 18
 :choices
 `(("Suffer in silence" . ,(lambda (c) (emacslife-bump-stat c :health -8)))
   ("Sue the caterer"   . ,(lambda (c) (when (emacslife-roll 0.4)
                                         (emacslife-bump-money c 3000))))))

(emacslife-register-event
 :id 'drunk-texted-boss :title "Drunk-Texted Boss"
 :body "It's 2am and your boss now knows your real feelings."
 :weight 2 :tone :funny :min-age 21 :max-age 60
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Pretend to be hacked" . ,(lambda (c) (when (emacslife-roll 0.5)
                                            (emacslife-bump-stat c :smarts 1))))
   ("Apologize in person"   . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Send another"         . ,(lambda (c) (let ((j (emacslife-character-job c)))
                                            (when j
                                              (setf (plist-get j :performance) 10)
                                              (setf (emacslife-character-job c) j)))))))

(emacslife-register-event
 :id 'locked-out-pajamas :title "Locked Out In Pajamas"
 :body "You stepped out for the mail.  In SpongeBob pants.  Door clicked."
 :weight 2 :tone :funny :min-age 18
 :choices
 `(("Wait for spouse" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))
   ("Call locksmith ($200)" . ,(lambda (c) (emacslife-bump-money c -200)))
   ("Break in" . ,(lambda (c) (when (emacslife-roll 0.3)
                                (push (list :crime 'B-and-E :sentence 0)
                                      (emacslife-character-jail-record c)))))))

(emacslife-register-event
 :id 'nft-rug :title "NFT Rug Pull"
 :body "Your $40,000 cartoon llama PFP is now worth $11."
 :weight 1 :tone :funny :min-age 18 :max-age 40
 :requires (lambda (c) (>= (emacslife-character-cash c) 40000))
 :choices
 `(("Cope on Twitter" . ,(lambda (c) (emacslife-bump-money c -40000)
                                     (emacslife-bump-stat c :happiness -8)))
   ("Buy more"         . ,(lambda (c) (emacslife-bump-money c -10000)))))

(emacslife-register-event
 :id 'podcast-launched :title "You Started A Podcast"
 :body "It has 12 listeners. Eleven are bots and your mom."
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Stop the podcast"   . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))
   ("Push through" . ,(lambda (c) (when (emacslife-roll 0.1)
                                    (emacslife-bump-money c 5000)
                                    (setf (emacslife-character-fame c)
                                          (emacslife-clamp
                                           (+ (emacslife-character-fame c) 5))))))))

(emacslife-register-event
 :id 'tattoo-ex-name :title "Tattooed Your Ex's Name"
 :body "Bold move at 22.  Bolder now."
 :weight 1 :tone :funny :min-age 18 :max-age 30
 :choices
 `(("Get it removed ($2k)" . ,(lambda (c) (emacslife-bump-money c -2000)
                                          (emacslife-bump-stat c :looks 2)))
   ("Cover with bigger tattoo" . ,(lambda (c) (emacslife-bump-money c -500)))
   ("Embrace the past" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))))

(emacslife-register-event
 :id 'plant-dies :title "Houseplant Died"
 :body "Phil the philodendron didn't make it.  You watered him daily."
 :weight 1 :tone :neutral :min-age 18 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))))

(emacslife-register-event
 :id 'nigerian-prince :title "Nigerian Prince Email"
 :body "He just needs your bank details to release the inheritance."
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Reply with details" . ,(lambda (c) (emacslife-bump-money c
                                                              (- (min 2000 (emacslife-character-cash c))))))
   ("Delete and move on" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Scam THEM back"     . ,(lambda (c) (when (emacslife-roll 0.1)
                                          (emacslife-bump-money c 5000))))))

(emacslife-register-event
 :id 'wrong-meeting :title "Wrong Meeting Room"
 :body "You walked into a board meeting in your pajamas, presenting on the wrong topic."
 :weight 1 :tone :funny :min-age 22 :max-age 65
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Improvise brilliantly" . ,(lambda (c) (let ((j (emacslife-character-job c)))
                                             (when j
                                               (setf (plist-get j :performance) 95)
                                               (setf (emacslife-character-job c) j)))))
   ("Flee" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Take a slow bow" . ,(lambda (c) (emacslife-bump-stat c :looks 1)))))

(emacslife-register-event
 :id 'met-celebrity :title "Met A Celebrity"
 :body "You bumped into a famous actor at the coffee shop."
 :weight 1 :tone :good :min-age 16
 :choices
 `(("Stay cool"      . ,(lambda (c) (emacslife-bump-stat c :looks 1)
                                    (setf (emacslife-character-fame c)
                                          (emacslife-clamp (+ (emacslife-character-fame c) 3)))))
   ("Ask for selfie" . ,(lambda (c) (emacslife-bump-stat c :happiness 4)))
   ("Spill coffee on them" . ,(lambda (c) (emacslife-bump-money c -200)
                                          (emacslife-bump-stat c :karma -2)))))

(emacslife-register-event
 :id 'fought-ticket :title "Beat A Parking Ticket"
 :body "You went to court for the $75 ticket. You won."
 :weight 1 :tone :good :min-age 18
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 4)
                        (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'roommate-vanished :title "Roommate Skipped Rent"
 :body "Your roommate's room is empty. And so is the bathroom shelf."
 :weight 1 :tone :bad :min-age 18 :max-age 30
 :choices
 `(("Cover the rent yourself" . ,(lambda (c) (emacslife-bump-money c -1800)))
   ("Sublease quickly"        . ,(lambda (c) (emacslife-bump-money c -200)))
   ("Sue them"                . ,(lambda (c) (emacslife-bump-money c -500)
                                              (when (emacslife-roll 0.3)
                                                (emacslife-bump-money c 2000))))))

(emacslife-register-event
 :id 'mystery-meal :title "Life-Changing Mystery Meal"
 :body "Some street food vendor handed you something with no name."
 :weight 1 :tone :funny :min-age 16
 :choices
 `(("Eat the whole thing" . ,(lambda (c) (if (emacslife-roll 0.6)
                                              (progn (emacslife-bump-stat c :happiness 6)
                                                     (emacslife-log c "It changed your life. Religious experience." :good))
                                            (emacslife-bump-stat c :health -10))))
   ("One bite"     . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))
   ("Politely decline" . ,(lambda (c) (emacslife-bump-stat c :karma 0)))))

(emacslife-register-event
 :id 'briefly-vegan :title "Vegan Phase"
 :body "You tried being vegan.  It lasted six days."
 :weight 1 :tone :funny :min-age 18 :max-age 50 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :health 1)
                        (emacslife-bump-stat c :happiness -1)))))

(emacslife-register-event
 :id 'hot-dog-contest :title "Hot Dog Eating Contest"
 :body "Are you really going to do this?"
 :weight 1 :tone :funny :min-age 18 :max-age 60
 :choices
 `(("Compete fully" . ,(lambda (c) (if (emacslife-roll 0.2)
                                        (progn (emacslife-bump-money c 1000)
                                               (push 'hot-dog-champion
                                                     (emacslife-character-achievements c)))
                                      (emacslife-bump-stat c :health -8))))
   ("Watch from sidelines" . ,(lambda (c) (emacslife-bump-stat c :happiness 1)))))

;;; ----- ADULT (30-49) -----

(emacslife-register-event
 :id 'midlife-car :title "Mid-Life Crisis Car"
 :body "You stared at a candy-apple-red convertible in the dealership lot."
 :weight 2 :tone :funny :min-age 35 :max-age 55
 :choices
 `(("Buy it now ($75k)" . ,(lambda (c) (when (>= (emacslife-character-cash c) 75000)
                                         (emacslife-bump-money c -75000)
                                         (push (list :kind 'car :name "Red Convertible"
                                                     :purchase-price 75000
                                                     :purchase-year (+ (emacslife-character-birth-year c)
                                                                       (emacslife-character-age c)))
                                               (emacslife-character-assets c))
                                         (emacslife-bump-stat c :happiness 12))))
   ("Walk away wisely" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Take it for a test drive only" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'hidden-talent :title "Discovered A Hidden Talent"
 :body "Apparently you can juggle five oranges blindfolded?"
 :weight 1 :tone :funny :min-age 25
 :choices
 `(("Take it seriously"  . ,(lambda (c) (emacslife-bump-stat c :happiness 5)
                                        (setf (emacslife-character-fame c)
                                              (emacslife-clamp (+ (emacslife-character-fame c) 2)))))
   ("Hide it forever"    . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Monetize on TikTok" . ,(lambda (c) (when (emacslife-roll 0.3)
                                          (emacslife-bump-money c 5000))))))

(emacslife-register-event
 :id 'weird-antique :title "Inherited A Weird Antique"
 :body "Great-aunt left you a porcelain doll that 'watches' the room."
 :weight 1 :tone :funny :min-age 25
 :choices
 `(("Display it proudly" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))
   ("Sell it ($800)" . ,(lambda (c) (emacslife-bump-money c 800)))
   ("Burn it" . ,(lambda (c) (when (emacslife-roll 0.4)
                               (emacslife-bump-stat c :health -5))))))

(emacslife-register-event
 :id 'doppelganger :title "Doppelganger Sighting"
 :body "Three people swear they saw you in a city you've never visited."
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Investigate" . ,(lambda (c) (emacslife-bump-stat c :smarts 2)))
   ("Ignore" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Frame them for your taxes" . ,(lambda (c) (emacslife-bump-stat c :karma -5)))))

(emacslife-register-event
 :id 'ancestor-famous :title "Famous Ancestor"
 :body "Genealogy site revealed you're descended from a 17th-century pirate."
 :weight 1 :tone :good :min-age 25 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)
                        (setf (emacslife-character-fame c)
                              (emacslife-clamp (+ (emacslife-character-fame c) 2)))))))

(emacslife-register-event
 :id 'hit-deer :title "Hit A Deer"
 :body "It walked out at 2am. Your bumper did not survive."
 :weight 2 :tone :bad :min-age 18
 :requires (lambda (c) (cl-some (lambda (a) (eq (plist-get a :kind) 'car))
                                (emacslife-character-assets c)))
 :choices
 `(("Pay for repair ($3k)" . ,(lambda (c) (emacslife-bump-money c -3000)))
   ("Drive it dented" . ,(lambda (c) (emacslife-bump-stat c :looks -1)))))

(emacslife-register-event
 :id 'hoa-letter :title "Angry HOA Letter"
 :body "Mary from down the street disapproves of your lawn flamingo."
 :weight 1 :tone :funny :min-age 25
 :requires (lambda (c) (cl-some (lambda (a) (eq (plist-get a :kind) 'house))
                                (emacslife-character-assets c)))
 :choices
 `(("Add 12 more flamingos" . ,(lambda (c) (emacslife-bump-stat c :happiness 6)
                                            (emacslife-bump-money c -200)))
   ("Comply meekly" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Start an HOA war" . ,(lambda (c) (emacslife-bump-stat c :karma -2)
                                       (emacslife-bump-stat c :happiness 4)))))

(emacslife-register-event
 :id 'dinner-party :title "Disastrous Dinner Party"
 :body "The soufflé fell. Then the guests started arguing about politics."
 :weight 1 :tone :funny :min-age 25
 :choices
 `(("Order pizza" . ,(lambda (c) (emacslife-bump-money c -80)))
   ("Power through" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))
   ("Pretend it's performance art" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'bought-boat :title "Bought A Boat (Unwisely)"
 :body "It's a 28-foot fishing boat you'll use exactly twice."
 :weight 1 :tone :funny :min-age 30 :max-age 65
 :requires (lambda (c) (>= (emacslife-character-cash c) 40000))
 :choices
 `(("Buy the boat ($40k)" . ,(lambda (c) (emacslife-bump-money c -40000)
                                          (push (list :kind 'boat :name "Liability"
                                                      :purchase-price 40000
                                                      :purchase-year (+ (emacslife-character-birth-year c)
                                                                        (emacslife-character-age c)))
                                                (emacslife-character-assets c))))
   ("Walk away" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'youtube-channel :title "Started a True-Crime YouTube"
 :body "Your voice is unsettlingly soothing."
 :weight 1 :tone :funny :min-age 25 :max-age 60
 :choices
 `(("Commit to it" . ,(lambda (c) (when (emacslife-roll 0.25)
                                     (emacslife-bump-money c 12000)
                                     (setf (emacslife-character-fame c)
                                           (emacslife-clamp
                                            (+ (emacslife-character-fame c) 8))))))
   ("Just do it as a hobby" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'hit-deer-2 :title "Foreign Government Recruitment"
 :body "A man in a hat at a coffee shop slid you a folder.  It contained a flag."
 :weight 1 :tone :funny :min-age 22 :max-age 55
 :choices
 `(("Accept the mission" . ,(lambda (c) (emacslife-bump-money c 50000)
                                         (emacslife-bump-stat c :karma -5)
                                         (push 'foreign-agent
                                               (emacslife-character-achievements c))))
   ("Decline politely" . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Call the FBI" . ,(lambda (c) (emacslife-bump-stat c :karma 5)
                                   (emacslife-bump-money c 5000)))))

;;; ----- SENIOR (50+) -----

(emacslife-register-event
 :id 'movie-quotes :title "Quoting Old Movies"
 :body "You now communicate exclusively in Casablanca quotes."
 :weight 1 :tone :funny :min-age 55 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)
                        (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'glasses-on-head :title "Glasses-On-Head Incident"
 :body "You spent 20 minutes looking for your glasses.  They were on your head."
 :weight 2 :tone :funny :min-age 50 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))))

(emacslife-register-event
 :id 'strongly-worded-letter :title "Strongly-Worded Letter"
 :body "You wrote a 4-page letter to the editor about pickle prices."
 :weight 1 :tone :funny :min-age 50
 :choices
 `(("Send it"      . ,(lambda (c) (emacslife-bump-stat c :happiness 4)))
   ("Send a longer one" . ,(lambda (c) (emacslife-bump-stat c :happiness 6)))))

(emacslife-register-event
 :id 'community-band :title "Joined Community Band"
 :body "You play the tuba now.  Apparently."
 :weight 1 :tone :good :min-age 55
 :choices
 `(("Throw yourself in" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)
                                        (emacslife-bump-stat c :health 2)))
   ("Quit after a week"  . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))))

(emacslife-register-event
 :id 'fence-feud :title "Neighbor Fence Feud"
 :body "Bob said your azaleas are crossing into his yard.  WAR."
 :weight 2 :tone :funny :min-age 50
 :choices
 `(("Escalate gleefully" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Make peace"         . ,(lambda (c) (emacslife-bump-stat c :karma 2)))
   ("Move the fence at midnight" . ,(lambda (c) (emacslife-bump-stat c :karma -3)))))

(emacslife-register-event
 :id 'gardening-comp :title "Competitive Gardening"
 :body "There's a $200 prize at the county fair for biggest pumpkin."
 :weight 1 :tone :good :min-age 50
 :choices
 `(("Go for the prize" . ,(lambda (c) (if (emacslife-roll 0.4)
                                           (emacslife-bump-money c 200)
                                         (emacslife-bump-stat c :happiness -1))))
   ("Tend casually" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'mistaken-celebrity :title "Mistaken For A Celebrity"
 :body "Strangers keep asking for your autograph.  You sign anyway."
 :weight 1 :tone :funny :min-age 50
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)))))

(emacslife-register-event
 :id 'twitter-feud :title "Twitter Feud"
 :body "You're now in a heated online argument with a kelp influencer."
 :weight 1 :tone :funny :min-age 30
 :choices
 `(("Engage fully" . ,(lambda (c) (when (emacslife-roll 0.4)
                                    (setf (emacslife-character-fame c)
                                          (emacslife-clamp
                                           (+ (emacslife-character-fame c) 3))))))
   ("Log off forever" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Demand to speak to their manager" . ,(lambda (c) (emacslife-bump-stat c :smarts -1)))))

(emacslife-register-event
 :id 'refuse-smartphone :title "Refused To Use A Smartphone"
 :body "You've decided your flip phone is plenty."
 :weight 1 :tone :neutral :min-age 60 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)
                        (emacslife-bump-stat c :smarts -1)))))

(emacslife-register-event
 :id 'beat-teen-videogames :title "Beat A Teen At Video Games"
 :body "You destroyed your grandkid at Smash Bros.  They've stopped visiting."
 :weight 1 :tone :funny :min-age 60
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness 6)))))

;;; ----- WORK -----

(emacslife-register-event
 :id 'boss-took-credit :title "Boss Took Credit"
 :body "The presentation that crushed it? They forgot to mention you."
 :weight 2 :tone :bad :min-age 22
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Confront publicly" . ,(lambda (c) (when (emacslife-roll 0.4)
                                          (emacslife-bump-stat c :happiness 4))))
   ("Document everything" . ,(lambda (c) (emacslife-bump-stat c :smarts 2)))
   ("Update LinkedIn quietly" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'office-party :title "Office Christmas Party"
 :body "It got... weird.  There was a karaoke incident."
 :weight 2 :tone :funny :min-age 22
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Lead the karaoke" . ,(lambda (c) (emacslife-bump-stat c :happiness 5)))
   ("Drink moderately"   . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Mysteriously vanish" . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))))

(emacslife-register-event
 :id 'mentor-someone :title "Asked To Mentor"
 :body "A junior at work wants you to mentor them."
 :weight 2 :tone :good :min-age 28
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Accept enthusiastically" . ,(lambda (c) (emacslife-bump-stat c :karma 5)
                                              (let ((j (emacslife-character-job c)))
                                                (when j
                                                  (setf (plist-get j :performance)
                                                        (min 100 (+ 10 (plist-get j :performance))))
                                                  (setf (emacslife-character-job c) j)))))
   ("Reluctantly agree"      . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Decline"                 . ,(lambda (c) (emacslife-bump-stat c :karma -2)))))

(emacslife-register-event
 :id 'asked-ted-talk :title "Asked To Do A TED Talk"
 :body "Topic of your choice.  Audience of thousands."
 :weight 1 :tone :good :min-age 30
 :requires (lambda (c) (>= (emacslife-character-fame c) 20))
 :choices
 `(("Deliver it brilliantly" . ,(lambda (c) (setf (emacslife-character-fame c)
                                                   (emacslife-clamp (+ (emacslife-character-fame c) 15)))
                                              (emacslife-bump-money c 25000)))
   ("Bomb spectacularly"     . ,(lambda (c) (emacslife-bump-stat c :happiness -10)))
   ("Decline"                . ,(lambda (c) (emacslife-bump-stat c :happiness 0)))))

(emacslife-register-event
 :id 'coworker-pay :title "Coworker Makes More Than You"
 :body "You found the salary spreadsheet.  Dave makes 60K more.  Dave."
 :weight 1 :tone :bad :min-age 25
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Demand a raise"  . ,(lambda (c) (when (emacslife-roll 0.5)
                                        (let ((j (emacslife-character-job c)))
                                          (when j
                                            (setf (plist-get j :salary)
                                                  (round (* (plist-get j :salary) 1.15)))
                                            (setf (emacslife-character-job c) j))))))
   ("Quietly seethe" . ,(lambda (c) (emacslife-bump-stat c :happiness -5)))
   ("Job-hunt aggressively" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'whistleblower :title "Whistleblower Opportunity"
 :body "You uncovered serious accounting fraud at the company."
 :weight 1 :tone :neutral :min-age 25
 :requires (lambda (c) (emacslife-character-job c))
 :choices
 `(("Blow the whistle"  . ,(lambda (c) (emacslife-bump-stat c :karma 10)
                                        (when (emacslife-roll 0.6)
                                          (emacslife-bump-money c 100000))
                                        (push (append (emacslife-character-job c)
                                                      (list :ended-year (+ (emacslife-character-birth-year c)
                                                                            (emacslife-character-age c))))
                                              (emacslife-character-job-history c))
                                        (setf (emacslife-character-job c) nil)))
   ("Stay silent"      . ,(lambda (c) (emacslife-bump-stat c :karma -5)))
   ("Use it as leverage" . ,(lambda (c) (emacslife-bump-money c 15000)
                                         (emacslife-bump-stat c :karma -8)))))

;;; ----- ANIMAL -----

(emacslife-register-event
 :id 'live-mouse :title "Cat Brought Home A Live Mouse"
 :body "It is now under your refrigerator."
 :weight 1 :tone :funny :min-age 18
 :requires (lambda (c) (cl-some (lambda (p) (equal (plist-get p :species) "Cat"))
                                (emacslife-character-pets c)))
 :choices
 `(("Set traps" . ,(lambda (c) (emacslife-bump-money c -30)))
   ("Befriend the mouse" . ,(lambda (c) (push (list :species "Mouse" :name "Squeaky"
                                                     :age 1 :alive t :happiness 60)
                                               (emacslife-character-pets c))))
   ("Move out" . ,(lambda (c) (emacslife-bump-money c -3000)))))

(emacslife-register-event
 :id 'pigeon-fries :title "Pigeon Stole Your Fries"
 :body "A pigeon. Plural fries.  All at once."
 :weight 1 :tone :funny :min-age 10 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))))

(emacslife-register-event
 :id 'bear-camping :title "Bears At Camp"
 :body "Three black bears want your hot dogs."
 :weight 1 :tone :bad :min-age 18
 :choices
 `(("Run" . ,(lambda (c) (when (emacslife-roll 0.5)
                           (emacslife-bump-stat c :health -10))))
   ("Stay calm" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Offer the hot dogs" . ,(lambda (c) (emacslife-bump-stat c :karma 2)))))

(emacslife-register-event
 :id 'spider-hand :title "Hand-Sized Spider"
 :body "It's on the wall.  Just hanging out.  Watching."
 :weight 1 :tone :funny :min-age 6
 :choices
 `(("Catch it in a jar" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Burn the house down" . ,(lambda (c) (emacslife-bump-money c -200000)))
   ("Move out" . ,(lambda (c) (emacslife-bump-money c -3000)))))

(emacslife-register-event
 :id 'snake-toilet :title "Snake In The Toilet"
 :body "There is a snake. In your toilet. Right now."
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Call animal control" . ,(lambda (c) (emacslife-bump-money c -200)))
   ("Negotiate with snake" . ,(lambda (c) (emacslife-bump-stat c :smarts 2)))
   ("Use a different bathroom forever" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

;;; ----- PARANORMAL -----

(emacslife-register-event
 :id 'reflection-winked :title "Reflection Winked At You"
 :body "Bathroom mirror. 3:14am. Your reflection winked."
 :weight 1 :tone :funny :min-age 12
 :choices
 `(("Wink back" . ,(lambda (c) (emacslife-bump-stat c :karma -2)
                               (emacslife-bump-stat c :smarts 2)))
   ("Cover the mirror" . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))
   ("Move out" . ,(lambda (c) (emacslife-bump-money c -3000)))))

(emacslife-register-event
 :id 'phone-yourself :title "Phone Call From Yourself"
 :body "Your phone rang.  Your own number was on the screen."
 :weight 1 :tone :funny :min-age 16
 :choices
 `(("Answer it" . ,(lambda (c) (emacslife-bump-stat c :smarts 3)
                               (emacslife-bump-stat c :karma -2)))
   ("Let it ring" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))
   ("Block your own number" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))))

(emacslife-register-event
 :id 'time-travel-offered :title "Time Travel Offer"
 :body "A pamphlet on your kitchen table reads 'GO BACK. ONE-WAY ONLY.'"
 :weight 1 :tone :funny :min-age 18
 :choices
 `(("Take the offer"        . ,(lambda (c)
                                  (setf (emacslife-character-alive c) nil)
                                  (setf (emacslife-character-death-cause c)
                                        "time travel paradox")))
   ("Frame the pamphlet"    . ,(lambda (c) (emacslife-bump-stat c :happiness 2)))
   ("Sell it on eBay ($50)" . ,(lambda (c) (emacslife-bump-money c 50)))))

(emacslife-register-event
 :id 'possessed-tax :title "Possessed By Tax Spirit"
 :body "You're inhabited by the ghost of a 1950s tax accountant."
 :weight 1 :tone :funny :min-age 25
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-money c 5000)
                        (emacslife-bump-stat c :smarts 3)
                        (emacslife-bump-stat c :happiness -3)))))

(emacslife-register-event
 :id 'ufo :title "UFO Sighting"
 :body "Lights moved against the wind.  No engine sound."
 :weight 1 :tone :funny :min-age 8
 :choices
 `(("Film it carefully" . ,(lambda (c) (when (emacslife-roll 0.4)
                                          (emacslife-bump-money c 2000))))
   ("Wave"               . ,(lambda (c) (emacslife-bump-stat c :karma 1)))
   ("Run"                . ,(lambda (c) (emacslife-bump-stat c :health -1)))))

(emacslife-register-event
 :id 'backyard-ruins :title "Ancient Ruins In Backyard"
 :body "You dug a hole for a tree. You found a stone door."
 :weight 1 :tone :funny :min-age 25
 :choices
 `(("Call archaeologists" . ,(lambda (c) (emacslife-bump-money c 8000)
                                          (emacslife-bump-stat c :karma 3)))
   ("Open the door"       . ,(lambda (c) (if (emacslife-roll 0.5)
                                              (emacslife-bump-money c 50000)
                                            (setf (emacslife-character-alive c) nil)
                                            (setf (emacslife-character-death-cause c)
                                                  "ancient curse"))))
   ("Pretend it isn't there" . ,(lambda (c) (emacslife-bump-stat c :karma -2)))))

;;; ----- TECH / MODERN -----

(emacslife-register-event
 :id 'phone-died :title "Phone Died At Worst Moment"
 :body "Right as the airline counter asked for the boarding pass."
 :weight 1 :tone :funny :min-age 18
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

(emacslife-register-event
 :id 'got-hacked :title "You Got Hacked"
 :body "Someone in another country is now using your Netflix."
 :weight 1 :tone :bad :min-age 18
 :choices
 `(("Change all passwords" . ,(lambda (c) (emacslife-bump-stat c :smarts 1)))
   ("Hire a security firm ($1k)" . ,(lambda (c) (emacslife-bump-money c -1000)))
   ("Give up on the internet" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)))))

(emacslife-register-event
 :id 'crypto-drained :title "Crypto Wallet Drained"
 :body "It was a phishing site.  Your degen coins are gone."
 :weight 1 :tone :bad :min-age 18
 :requires (lambda (c) (cl-some (lambda (p) (eq (plist-get p :type) 'crypto))
                                (emacslife-character-investments c)))
 :choices
 `(("Cope" . ,(lambda (c) (setf (emacslife-character-investments c)
                                 (cl-remove-if (lambda (p) (eq (plist-get p :type) 'crypto))
                                               (emacslife-character-investments c)))
                          (emacslife-bump-stat c :happiness -8)))
   ("Try to recover (good luck)" . ,(lambda (c) (when (emacslife-roll 0.1)
                                                  (emacslife-bump-money c 1000))))))

(emacslife-register-event
 :id 'cancelled :title "Cancelled For Old Tweet"
 :body "A 12-year-old tweet has surfaced. The internet is, predictably, mad."
 :weight 1 :tone :funny :min-age 20
 :requires (lambda (c) (>= (emacslife-character-fame c) 15))
 :choices
 `(("Apologize sincerely"  . ,(lambda (c) (setf (emacslife-character-fame c)
                                                 (max 0 (- (emacslife-character-fame c) 10)))))
   ("Double down"          . ,(lambda (c) (if (emacslife-roll 0.4)
                                                (setf (emacslife-character-fame c)
                                                      (emacslife-clamp (+ (emacslife-character-fame c) 10)))
                                              (setf (emacslife-character-fame c)
                                                    (max 0 (- (emacslife-character-fame c) 25))))))
   ("Disappear for a year" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)))))

(emacslife-register-event
 :id 'deepfake :title "AI Deepfake Of You Viral"
 :body "Apparently 'you' endorsed a brand of fish food."
 :weight 1 :tone :funny :min-age 25
 :requires (lambda (c) (>= (emacslife-character-fame c) 30))
 :choices
 `(("Embrace it"   . ,(lambda (c) (emacslife-bump-money c 5000)))
   ("Sue everyone" . ,(lambda (c) (emacslife-bump-money c -10000)
                                   (when (emacslife-roll 0.4)
                                     (emacslife-bump-money c 50000))))
   ("Make a deepfake response" . ,(lambda (c) (emacslife-bump-stat c :smarts 2)))))

(emacslife-register-event
 :id 'smart-fridge :title "Smart Fridge Made Demands"
 :body "Your fridge is texting you about its 'needs.'"
 :weight 1 :tone :funny :min-age 25
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-money c -200)
                        (emacslife-bump-stat c :happiness -1)))))

;;; ----- ROMANCE / RELATIONSHIPS -----

(emacslife-register-event
 :id 'dirty-dishes :title "Dirty Dishes AGAIN"
 :body "Spouse promised they'd do them. They are not done."
 :weight 2 :tone :funny :min-age 22
 :requires (lambda (c) (emacslife-character-spouse c))
 :choices
 `(("Bring it up calmly" . ,(lambda (c) (emacslife-bump-stat c :happiness -1)))
   ("Passive-aggressive note" . ,(lambda (c) (emacslife-bump-stat c :karma -1)))
   ("Just do them yourself" . ,(lambda (c) (emacslife-bump-stat c :happiness -2)))))

(emacslife-register-event
 :id 'anniversary-gift :title "Anniversary Gift Crisis"
 :body "It's your anniversary in three hours and you have nothing."
 :weight 1 :tone :funny :min-age 22
 :requires (lambda (c) (emacslife-character-spouse c))
 :choices
 `(("Buy expensive jewelry ($2k)" . ,(lambda (c) (emacslife-bump-money c -2000)
                                                   (let ((sp (emacslife-npc-by-id c (emacslife-character-spouse c))))
                                                     (when sp (emacslife-bump-bar sp 15)))))
   ("Write heartfelt poem" . ,(lambda (c) (when (>= (emacslife-character-smarts c) 50)
                                            (let ((sp (emacslife-npc-by-id c (emacslife-character-spouse c))))
                                              (when sp (emacslife-bump-bar sp 20))))))
   ("Gas station roses" . ,(lambda (c) (let ((sp (emacslife-npc-by-id c (emacslife-character-spouse c))))
                                          (when sp (emacslife-bump-bar sp -8)))))))

(emacslife-register-event
 :id 'in-laws-visit :title "In-Laws Are Visiting"
 :body "They booked a hotel.  Just kidding, they're staying with you."
 :weight 2 :tone :funny :min-age 22
 :requires (lambda (c) (emacslife-character-spouse c))
 :choices
 `(("Be a perfect host" . ,(lambda (c) (emacslife-bump-stat c :karma 3)
                                       (emacslife-bump-money c -500)))
   ("Disappear during visit" . ,(lambda (c) (emacslife-bump-stat c :happiness 3)
                                             (let ((sp (emacslife-npc-by-id c (emacslife-character-spouse c))))
                                               (when sp (emacslife-bump-bar sp -10)))))
   ("Pretend to be sick" . ,(lambda (c) (emacslife-bump-stat c :karma -2)))))

(emacslife-register-event
 :id 'surprise-pregnancy :title "Surprise Pregnancy"
 :body "Well.  Well well well."
 :weight 1 :tone :neutral :min-age 18 :max-age 50
 :requires (lambda (c) (and (emacslife-character-spouse c)
                            (memq (emacslife-character-gender c) '(male female))))
 :choices
 `(("Welcome the baby" .
    ,(lambda (c) (let ((baby (emacslife--make-npc c :relation 'child :age 0
                                                  :surname (emacslife-character-surname c))))
                   (push (emacslife-npc-id baby) (emacslife-character-children c)))
                 (emacslife-bump-stat c :happiness 10)))
   ("Panic appropriately" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))))

;;; ----- WORLD EVENTS (apply to almost anyone) -----

(emacslife-register-event
 :id 'world-recession :title "Global Recession"
 :body "Markets are crashing.  Your portfolio is bleeding."
 :weight 1 :tone :bad :min-age 18
 :requires (lambda (c) (or (emacslife-character-investments c)
                           (>= (emacslife-character-cash c) 10000)))
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-money c (- (round (* 0.15 (emacslife-character-cash c)))))
                        (emacslife-bump-stat c :happiness -5)))))

(emacslife-register-event
 :id 'global-pandemic :title "Global Pandemic"
 :body "A new virus is spreading.  Lockdown begins."
 :weight 1 :tone :bad :min-age 0
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :health -3)
                        (emacslife-bump-stat c :happiness -8)
                        (when (and (emacslife-character-job c)
                                    (emacslife-roll 0.1))
                          (push (append (emacslife-character-job c)
                                        (list :laid-off t
                                              :ended-year (+ (emacslife-character-birth-year c)
                                                              (emacslife-character-age c))))
                                (emacslife-character-job-history c))
                          (setf (emacslife-character-job c) nil))))))

(emacslife-register-event
 :id 'tax-refund :title "Surprise Tax Refund"
 :body "Apparently you overpaid.  Cha-ching."
 :weight 1 :tone :good :min-age 21
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-money c (+ 500 (random 3500)))))))

(emacslife-register-event
 :id 'tax-audit :title "Tax Audit"
 :body "An IRS letter wants to chat about the last seven years."
 :weight 1 :tone :bad :min-age 21
 :choices
 `(("Cooperate fully"   . ,(lambda (c) (emacslife-bump-money c -2000)))
   ("Hire a CPA ($3k)"  . ,(lambda (c) (emacslife-bump-money c -3000)
                                        (when (emacslife-roll 0.3)
                                          (emacslife-bump-money c 1500))))
   ("Lie boldly"         . ,(lambda (c) (when (emacslife-roll 0.5)
                                          (push (list :crime 'tax-fraud :sentence 3)
                                                (emacslife-character-jail-record c)))))))

;;; ----- HEALTH ailments (gated, varied) -----

(emacslife-register-event
 :id 'allergy :title "Sudden Allergy"
 :body "You're now violently allergic to something previously delicious."
 :weight 1 :tone :bad :min-age 8
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :happiness -3)))))

(emacslife-register-event
 :id 'broke-arm :title "Broke Your Arm"
 :body "You tried to do a cool thing.  Bones disagreed."
 :weight 1 :tone :bad :min-age 5
 :choices
 `(("Cast and recovery" . ,(lambda (c) (emacslife-bump-money c -800)
                                        (emacslife-bump-stat c :health -3)))
   ("Tough it out" . ,(lambda (c) (emacslife-bump-stat c :health -8)))))

(emacslife-register-event
 :id 'mystery-illness :title "Mystery Illness"
 :body "Doctors don't know what it is.  Tests are inconclusive."
 :weight 1 :tone :bad :min-age 25
 :choices
 `(("Aggressive testing ($5k)" . ,(lambda (c) (emacslife-bump-money c -5000)
                                                (emacslife-bump-stat c :health 3)))
   ("Holistic approach"        . ,(lambda (c) (emacslife-bump-stat c :health -1)))))

(emacslife-register-event
 :id 'back-pain :title "Threw Your Back Out"
 :body "Sneezed wrong.  Cannot move for three days."
 :weight 1 :tone :funny :min-age 30
 :auto t
 :choices
 `(("OK" . ,(lambda (c) (emacslife-bump-stat c :health -2)
                        (emacslife-bump-stat c :happiness -2)))))

(provide 'emacslife-events)
;;; emacslife-events.el ends here
