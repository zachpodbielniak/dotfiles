;;; emacslife-crime.el --- EmacsLife: crime, trial, prison, escape -*- lexical-binding: t; -*-
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
;; Crime registry, trial system with optional lawyer, prison
;; mechanics with in-prison actions, and the escape minigame
;; (grid-based guard-avoidance — completion path to the Houdini
;; ribbon).

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)

;;; -----------------------------------------------------------------
;;; Crime registry

(defvar emacslife--crimes nil
  "Alist of (ID . PLIST) for committable crimes.
Plist keys:
  :label string
  :min-age int
  :success-chance float (0..1 base, modified by smarts/looks/karma)
  :reward-min int
  :reward-max int
  :get-caught-base float (0..1) — chance of getting caught even if successful
  :sentence-years int (max)
  :karma-cost int      :description string")

(cl-defun emacslife-register-crime
    (&key id label min-age success-chance reward-min reward-max
          get-caught-base sentence-years karma-cost description)
  (setf (alist-get id emacslife--crimes)
        (list :id id :label label :min-age (or min-age 16)
              :success-chance success-chance
              :reward-min reward-min :reward-max reward-max
              :get-caught-base get-caught-base
              :sentence-years sentence-years
              :karma-cost karma-cost
              :description description))
  id)

(emacslife-register-crime :id 'pickpocket :label "Pickpocket someone"
  :min-age 12 :success-chance 0.65 :reward-min 5 :reward-max 200
  :get-caught-base 0.20 :sentence-years 1 :karma-cost 3
  :description "Low risk, low reward.")

(emacslife-register-crime :id 'shoplift :label "Shoplift from a store"
  :min-age 12 :success-chance 0.55 :reward-min 10 :reward-max 500
  :get-caught-base 0.25 :sentence-years 1 :karma-cost 4
  :description "Camera-dependent.")

(emacslife-register-crime :id 'mischief :label "Commit petty mischief"
  :min-age 8 :success-chance 0.85 :reward-min 0 :reward-max 0
  :get-caught-base 0.10 :sentence-years 0 :karma-cost 2
  :description "Just being annoying.")

(emacslife-register-crime :id 'burglary :label "Burglarize a house"
  :min-age 16 :success-chance 0.45 :reward-min 1000 :reward-max 30000
  :get-caught-base 0.30 :sentence-years 5 :karma-cost 10
  :description "Medium risk, medium reward.")

(emacslife-register-crime :id 'gta :label "Grand Theft Auto"
  :min-age 16 :success-chance 0.40 :reward-min 8000 :reward-max 60000
  :get-caught-base 0.35 :sentence-years 7 :karma-cost 12
  :description "Steal a car.")

(emacslife-register-crime :id 'bank-robbery :label "Rob a bank"
  :min-age 18 :success-chance 0.30 :reward-min 50000 :reward-max 1000000
  :get-caught-base 0.45 :sentence-years 15 :karma-cost 20
  :description "High risk, high reward.")

(emacslife-register-crime :id 'drug-dealing :label "Deal drugs"
  :min-age 16 :success-chance 0.55 :reward-min 500 :reward-max 25000
  :get-caught-base 0.30 :sentence-years 8 :karma-cost 10
  :description "Recurring income, recurring risk.")

(emacslife-register-crime :id 'embezzle :label "Embezzle from work"
  :min-age 22 :success-chance 0.50 :reward-min 5000 :reward-max 250000
  :get-caught-base 0.35 :sentence-years 10 :karma-cost 12
  :description "Requires a job.")

(emacslife-register-crime :id 'assault :label "Assault a stranger"
  :min-age 14 :success-chance 0.65 :reward-min 0 :reward-max 0
  :get-caught-base 0.40 :sentence-years 4 :karma-cost 15
  :description "Just to be a jerk.")

(emacslife-register-crime :id 'murder :label "Commit murder"
  :min-age 18 :success-chance 0.40 :reward-min 0 :reward-max 0
  :get-caught-base 0.55 :sentence-years 30 :karma-cost 50
  :description "No going back.")

(emacslife-register-crime :id 'hitman :label "Hire a hitman (target an NPC)"
  :min-age 18 :success-chance 0.50 :reward-min 0 :reward-max 0
  :get-caught-base 0.40 :sentence-years 25 :karma-cost 40
  :description "Paid murder.  Costs $20000.")

;; New (production-grade) crimes —

(emacslife-register-crime :id 'tax-evasion :label "Evade your taxes"
  :min-age 21 :success-chance 0.65 :reward-min 5000 :reward-max 80000
  :get-caught-base 0.25 :sentence-years 5 :karma-cost 8
  :description "Hide income from the IRS.")

(emacslife-register-crime :id 'counterfeit :label "Print counterfeit bills"
  :min-age 18 :success-chance 0.35 :reward-min 5000 :reward-max 200000
  :get-caught-base 0.40 :sentence-years 12 :karma-cost 15
  :description "Federal crime. Bigger reward, bigger risk.")

(emacslife-register-crime :id 'arson :label "Commit arson"
  :min-age 16 :success-chance 0.55 :reward-min 0 :reward-max 0
  :get-caught-base 0.35 :sentence-years 12 :karma-cost 25
  :description "Burn it down. For reasons.")

(emacslife-register-crime :id 'carjacking :label "Carjack someone"
  :min-age 16 :success-chance 0.45 :reward-min 0 :reward-max 0
  :get-caught-base 0.40 :sentence-years 8 :karma-cost 15
  :description "Hands-up situation.  Adds a car to assets if successful.")

(emacslife-register-crime :id 'cybercrime :label "Hack a bank"
  :min-age 18 :success-chance 0.45 :reward-min 10000 :reward-max 500000
  :get-caught-base 0.30 :sentence-years 10 :karma-cost 10
  :description "Smarts-gated.  Needs Smarts ≥ 70.")

(emacslife-register-crime :id 'launder :label "Launder money"
  :min-age 22 :success-chance 0.55 :reward-min 0 :reward-max 80000
  :get-caught-base 0.30 :sentence-years 8 :karma-cost 10
  :description "Clean up dirty cash.")

(emacslife-register-crime :id 'insurance-fraud :label "Insurance fraud"
  :min-age 22 :success-chance 0.55 :reward-min 2000 :reward-max 60000
  :get-caught-base 0.30 :sentence-years 6 :karma-cost 8
  :description "Stage an 'accident.'")

(emacslife-register-crime :id 'jaywalk :label "Jaywalk defiantly"
  :min-age 8 :success-chance 0.95 :reward-min 0 :reward-max 0
  :get-caught-base 0.05 :sentence-years 0 :karma-cost 0
  :description "Living dangerously.")

;;; -----------------------------------------------------------------
;;; Crime execution

(defun emacslife-commit-crime (char id)
  "Attempt crime ID for CHAR."
  (let* ((c (or (alist-get id emacslife--crimes)
                (user-error "Unknown crime: %S" id)))
         (smarts-bonus (/ (- (emacslife-character-smarts char) 50) 200.0))
         (success-p (emacslife-roll (+ (plist-get c :success-chance)
                                       smarts-bonus)))
         (caught-p (emacslife-roll (max 0.05
                                        (- (plist-get c :get-caught-base)
                                           smarts-bonus)))))
    (emacslife-bump-stat char :karma (- (plist-get c :karma-cost)))
    (when (eq id 'hitman)
      (when (< (emacslife-character-cash char) 20000)
        (user-error "Hitman costs $20000 you don't have"))
      (emacslife-bump-money char -20000))
    (cond
     ;; success no caught
     ((and success-p (not caught-p))
      (let ((take (+ (plist-get c :reward-min)
                     (random (max 1 (- (plist-get c :reward-max)
                                       (plist-get c :reward-min)))))))
        (emacslife-bump-money char take)
        (emacslife-log char
                       (format "Got away with %s. Pocketed $%s."
                               (plist-get c :label)
                               (emacslife-format-money take))
                       :good)
        (message "Clean getaway.")))
     ;; success but caught
     ((and success-p caught-p)
      (emacslife-log char (format "Caught after committing %s."
                                  (plist-get c :label))
                     :bad)
      (emacslife--trial char c))
     ;; failed and caught
     (caught-p
      (emacslife-log char (format "Failed at %s and got caught."
                                  (plist-get c :label))
                     :bad)
      (emacslife--trial char c))
     ;; failed but escaped
     (t
      (emacslife-log char (format "Botched %s but slipped away."
                                  (plist-get c :label))
                     :neutral)
      (message "Nothing happened.")))))

;;; -----------------------------------------------------------------
;;; Trial

(defun emacslife--trial (char crime)
  "Run a trial for CHAR who's been caught committing CRIME (plist)."
  (let* ((hire (yes-or-no-p
                (format "Hire a lawyer for $%d? (improves your odds) "
                        20000)))
         (afford (and hire (>= (emacslife-character-cash char) 20000)))
         (acquit-base (if (and hire afford) 0.45 0.15))
         (smarts-bonus (/ (- (emacslife-character-smarts char) 50) 300.0)))
    (when (and hire afford)
      (emacslife-bump-money char -20000))
    (when (and hire (not afford))
      (message "You can't afford a lawyer.  Going pro se."))
    (if (emacslife-roll (+ acquit-base smarts-bonus))
        (progn
          (emacslife-log char "ACQUITTED at trial!" :good)
          (message "Walked free."))
      (let ((years (max 1 (random (1+ (plist-get crime :sentence-years))))))
        (push (list :crime (plist-get crime :id) :sentence years
                    :year (+ (emacslife-character-birth-year char)
                             (emacslife-character-age char)))
              (emacslife-character-jail-record char))
        (emacslife-log char (format "Sentenced to %d years."
                                    years)
                       :bad)
        (emacslife--enter-prison char years)))))

;;; -----------------------------------------------------------------
;;; Prison

(defun emacslife--enter-prison (char years)
  "Sentence CHAR to YEARS of prison.  Time advances each age-up
while in prison.  In-prison actions are available."
  (let ((md (or (emacslife-character-metadata char) '())))
    (setf (emacslife-character-metadata char)
          (plist-put (plist-put md :in-prison t)
                     :sentence-remaining years)))
  (emacslife-log char (format "Imprisoned for %d years." years) :bad)
  (when (emacslife-character-job char)
    (push (append (emacslife-character-job char)
                  (list :fired t
                        :ended-year (+ (emacslife-character-birth-year char)
                                       (emacslife-character-age char))))
          (emacslife-character-job-history char))
    (setf (emacslife-character-job char) nil)))

(defun emacslife-in-prison-p (char)
  (plist-get (emacslife-character-metadata char) :in-prison))

(defun emacslife-process-prison-year (char)
  "Decrement remaining sentence, release when done."
  (when (emacslife-in-prison-p char)
    (let* ((md (emacslife-character-metadata char))
           (rem (or (plist-get md :sentence-remaining) 0)))
      (cl-incf (emacslife-character-prison-time-served char))
      (cond
       ((<= rem 1)
        (setf (emacslife-character-metadata char)
              (plist-put (plist-put md :in-prison nil) :sentence-remaining 0))
        (emacslife-log char "Released from prison." :good))
       (t
        (setf (emacslife-character-metadata char)
              (plist-put md :sentence-remaining (1- rem))))))))

(defun emacslife-prison-action ()
  "In-prison action menu."
  (interactive)
  (emacslife-with-state char
    (unless (emacslife-in-prison-p char)
      (user-error "You're not in prison"))
    (let* ((choice (completing-read
                    "Prison action: "
                    '("Work out" "Library" "Join gang" "Start riot"
                      "Make friends" "Pick fights" "Good behavior"
                      "Attempt escape")
                    nil t)))
      (pcase choice
        ("Work out"
         (emacslife-bump-stat char :health 5)
         (emacslife-log char "Lifted weights. Health +5." :neutral))
        ("Library"
         (emacslife-bump-stat char :smarts 3)
         (emacslife-log char "Read in the prison library. Smarts +3." :neutral))
        ("Join gang"
         (emacslife-bump-stat char :karma -8)
         (when (emacslife-roll 0.3)
           (let ((md (emacslife-character-metadata char)))
             (setf (emacslife-character-metadata char)
                   (plist-put md :sentence-remaining
                              (1+ (or (plist-get md :sentence-remaining) 0))))))
         (emacslife-log char "Joined a gang. Karma -8." :bad))
        ("Start riot"
         (emacslife-bump-stat char :happiness 5)
         (let ((md (emacslife-character-metadata char)))
           (setf (emacslife-character-metadata char)
                 (plist-put md :sentence-remaining
                            (+ 2 (or (plist-get md :sentence-remaining) 0)))))
         (emacslife-log char "Started a riot.  +2 years added." :bad))
        ("Make friends"
         (emacslife-bump-stat char :happiness 4)
         (emacslife-log char "Made a prison friend." :good))
        ("Pick fights"
         (let ((md (emacslife-character-metadata char)))
           (setf (emacslife-character-metadata char)
                 (plist-put md :sentence-remaining
                            (1+ (or (plist-get md :sentence-remaining) 0)))))
         (emacslife-bump-stat char :health -3)
         (emacslife-log char "Picked a fight. +1 year." :bad))
        ("Good behavior"
         (let ((md (emacslife-character-metadata char)))
           (setf (emacslife-character-metadata char)
                 (plist-put md :sentence-remaining
                            (max 0 (1- (or (plist-get md :sentence-remaining) 1)))))
           (emacslife-log char "Good behavior. -1 year." :good)))
        ("Attempt escape" (emacslife--escape-minigame char)))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

;;; -----------------------------------------------------------------
;;; Escape minigame — guard-avoidance grid

(defvar emacslife--escape-grid nil)
(defvar emacslife--escape-player nil)
(defvar emacslife--escape-exit nil)
(defvar emacslife--escape-guards nil)
(defvar emacslife--escape-moves 0)
(defvar emacslife--escape-active nil)

(defconst emacslife--escape-width 12)
(defconst emacslife--escape-height 8)

(define-derived-mode emacslife-escape-mode special-mode "Prison-Escape"
  "Major mode for the EmacsLife prison-escape minigame."
  (setq-local cursor-type nil)
  (read-only-mode 1))

(defun emacslife--escape-render ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "PRISON ESCAPE\n" 'face 'emacslife-face-title))
    (insert "Use arrow keys or hjkl. Reach the X without touching a G.\n")
    (insert "Press ESC (or SPC m q) to give up.\n\n")
    (dotimes (y emacslife--escape-height)
      (dotimes (x emacslife--escape-width)
        (cond
         ((equal (list x y) emacslife--escape-player)
          (insert (propertize "@" 'face 'emacslife-face-name)))
         ((equal (list x y) emacslife--escape-exit)
          (insert (propertize "X" 'face 'emacslife-face-event-good)))
         ((member (list x y) emacslife--escape-guards)
          (insert (propertize "G" 'face 'emacslife-face-event-bad)))
         (t (insert "."))))
      (insert "\n"))
    (insert (format "\nMoves: %d\n" emacslife--escape-moves))))

(defun emacslife--escape-move (dx dy)
  (when emacslife--escape-active
    (let* ((p emacslife--escape-player)
           (nx (max 0 (min (1- emacslife--escape-width) (+ (car p) dx))))
           (ny (max 0 (min (1- emacslife--escape-height) (+ (cadr p) dy)))))
      (setq emacslife--escape-player (list nx ny))
      (cl-incf emacslife--escape-moves)
      ;; move guards toward player
      (setq emacslife--escape-guards
            (mapcar
             (lambda (g)
               (let ((gx (car g)) (gy (cadr g)))
                 (list (+ gx (cond ((< gx nx) 1) ((> gx nx) -1) (t 0)))
                       (+ gy (cond ((< gy ny) 1) ((> gy ny) -1) (t 0))))))
             emacslife--escape-guards))
      ;; check collisions / exit
      (cond
       ((equal emacslife--escape-player emacslife--escape-exit)
        (setq emacslife--escape-active nil)
        (emacslife--escape-render)
        (emacslife--escape-resolve t))
       ((member emacslife--escape-player emacslife--escape-guards)
        (setq emacslife--escape-active nil)
        (emacslife--escape-render)
        (emacslife--escape-resolve nil))
       (t (emacslife--escape-render))))))

(defun emacslife--escape-resolve (won)
  (let ((char emacslife--state))
    (cond
     (won
      (let ((md (emacslife-character-metadata char)))
        (setf (emacslife-character-metadata char)
              (plist-put (plist-put md :in-prison nil)
                         :sentence-remaining 0)))
      (push 'houdini-attempt
            (emacslife-character-achievements char))
      (emacslife-log char "ESCAPED FROM PRISON! Houdini ribbon eligibility +1."
                     :good)
      (message "FREE!"))
     (t
      (let ((md (emacslife-character-metadata char)))
        (setf (emacslife-character-metadata char)
              (plist-put md :sentence-remaining
                         (+ 3 (or (plist-get md :sentence-remaining) 0)))))
      (emacslife-bump-stat char :health -5)
      (emacslife-log char "Caught trying to escape. +3 years, health -5." :bad)
      (message "Busted.")))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

(defun emacslife--escape-quit ()
  (interactive)
  (setq emacslife--escape-active nil)
  (kill-buffer (current-buffer)))

;; Escape minigame keymap.
;;
;; Unlike the other emacslife buffers, the escape grid IS a game where
;; hjkl/arrow keys are the actual gameplay moves — like nethack — so
;; rebinding them in evil normal state is the desired UX, not a
;; conflict.  We still expose ESC / SPC m q as escape hatches.
(defvar emacslife-escape-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>")    (lambda () (interactive) (emacslife--escape-move 0 -1)))
    (define-key map (kbd "<down>")  (lambda () (interactive) (emacslife--escape-move 0 1)))
    (define-key map (kbd "<left>")  (lambda () (interactive) (emacslife--escape-move -1 0)))
    (define-key map (kbd "<right>") (lambda () (interactive) (emacslife--escape-move 1 0)))
    (define-key map "k" (lambda () (interactive) (emacslife--escape-move 0 -1)))
    (define-key map "j" (lambda () (interactive) (emacslife--escape-move 0 1)))
    (define-key map "h" (lambda () (interactive) (emacslife--escape-move -1 0)))
    (define-key map "l" (lambda () (interactive) (emacslife--escape-move 1 0)))
    (define-key map (kbd "<escape>") #'emacslife--escape-quit)
    map))

(with-eval-after-load 'evil
  (evil-define-key* 'normal emacslife-escape-mode-map
    (kbd "<up>")    (lambda () (interactive) (emacslife--escape-move 0 -1))
    (kbd "<down>")  (lambda () (interactive) (emacslife--escape-move 0 1))
    (kbd "<left>")  (lambda () (interactive) (emacslife--escape-move -1 0))
    (kbd "<right>") (lambda () (interactive) (emacslife--escape-move 1 0))
    "k" (lambda () (interactive) (emacslife--escape-move 0 -1))
    "j" (lambda () (interactive) (emacslife--escape-move 0 1))
    "h" (lambda () (interactive) (emacslife--escape-move -1 0))
    "l" (lambda () (interactive) (emacslife--escape-move 1 0))))

(when (fboundp 'map!)
  (eval
   '(map! :map emacslife-escape-mode-map
          :localleader
          :desc "Give up / quit" "q" #'emacslife--escape-quit)))

(defun emacslife--escape-minigame (_char)
  "Start the escape minigame (operates on `emacslife--state')."
  (setq emacslife--escape-grid t
        emacslife--escape-player (list 0 (1- emacslife--escape-height))
        emacslife--escape-exit (list (1- emacslife--escape-width) 0)
        emacslife--escape-guards
        (cl-loop repeat 3
                 collect (list (+ 3 (random (- emacslife--escape-width 5)))
                               (+ 2 (random (- emacslife--escape-height 4)))))
        emacslife--escape-moves 0
        emacslife--escape-active t)
  (let ((buf (get-buffer-create "*emacslife: escape*")))
    (with-current-buffer buf
      (emacslife-escape-mode)
      (emacslife--escape-render))
    (pop-to-buffer buf)))

;;; -----------------------------------------------------------------
;;; Crime menu

(defun emacslife-crime-menu ()
  "Pick a crime to commit."
  (interactive)
  (emacslife-with-state char
    (when (emacslife-in-prison-p char)
      (user-error "You're in prison — use prison actions"))
    (let* ((age (emacslife-character-age char))
           (eligible (cl-remove-if-not
                      (lambda (cell) (>= age (plist-get (cdr cell) :min-age)))
                      emacslife--crimes))
           (rows (mapcar (lambda (cell)
                           (let ((p (cdr cell)))
                             (cons (format "%-28s  %s"
                                           (plist-get p :label)
                                           (plist-get p :description))
                                   (plist-get p :id))))
                         eligible))
           (choice (completing-read "Crime: " (mapcar #'car rows) nil t)))
      (when choice
        (emacslife-commit-crime char (cdr (assoc choice rows))))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

(emacslife-register-action :crime (lambda (_form) (emacslife-crime-menu)))
(emacslife-register-action :prison (lambda (_form) (emacslife-prison-action)))

(provide 'emacslife-crime)
;;; emacslife-crime.el ends here
