;;; emacslife-career.el --- EmacsLife: jobs, promotions, special careers -*- lexical-binding: t; -*-
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
;; Job/career registry with ~30 regular jobs plus three special-career
;; pipelines (Music, Acting, Military).  Jobs are tiers; promotions
;; walk up the tier ladder.  The CHAR.job field is a plist:
;;   (:track <key> :title <string> :tier <int> :salary <int>
;;    :years <int> :performance 0..100)

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)

;;; -----------------------------------------------------------------
;;; Job track registry

(defvar emacslife--job-tracks nil
  "Alist (TRACK . PLIST).  Plist keys:
:industry string     :degree symbol-or-nil
:min-smarts int      :min-looks int
:tiers list of (TITLE . SALARY) lowest first.")

(defun emacslife-register-job-track (track &rest plist)
  (setf (alist-get track emacslife--job-tracks) plist)
  track)

;; Eight broad industries / ~30 jobs total via tiered tracks

(emacslife-register-job-track 'tech
  :industry "Technology" :degree 'bachelors
  :min-smarts 60 :min-looks 0
  :tiers '(("Junior Developer" . 65000)
           ("Software Engineer" . 95000)
           ("Senior Engineer" . 130000)
           ("Staff Engineer" . 180000)
           ("Engineering Director" . 240000)
           ("CTO" . 400000)
           ("CEO" . 800000)))

(emacslife-register-job-track 'medicine
  :industry "Healthcare" :degree 'md
  :min-smarts 75 :min-looks 0
  :tiers '(("Resident" . 60000)
           ("Attending Physician" . 220000)
           ("Specialist" . 350000)
           ("Department Head" . 500000)
           ("Chief of Surgery" . 800000)))

(emacslife-register-job-track 'law
  :industry "Legal" :degree 'jd
  :min-smarts 70 :min-looks 0
  :tiers '(("Junior Associate" . 90000)
           ("Senior Associate" . 160000)
           ("Partner" . 350000)
           ("Senior Partner" . 600000)))

(emacslife-register-job-track 'finance
  :industry "Finance" :degree 'bachelors
  :min-smarts 65 :min-looks 50
  :tiers '(("Analyst" . 80000)
           ("Associate" . 130000)
           ("VP" . 220000)
           ("Managing Director" . 500000)
           ("CFO" . 750000)))

(emacslife-register-job-track 'retail
  :industry "Retail" :degree nil
  :min-smarts 0 :min-looks 0
  :tiers '(("Cashier" . 28000)
           ("Shift Lead" . 35000)
           ("Assistant Manager" . 45000)
           ("Store Manager" . 65000)
           ("Regional Manager" . 95000)))

(emacslife-register-job-track 'trade
  :industry "Trades" :degree nil
  :min-smarts 40 :min-looks 0
  :tiers '(("Apprentice" . 35000)
           ("Journeyman" . 55000)
           ("Master Tradesperson" . 85000)
           ("Contractor" . 130000)))

(emacslife-register-job-track 'food
  :industry "Hospitality" :degree nil
  :min-smarts 0 :min-looks 30
  :tiers '(("Server" . 25000)
           ("Bartender" . 38000)
           ("Sous Chef" . 55000)
           ("Head Chef" . 90000)
           ("Restaurateur" . 160000)))

(emacslife-register-job-track 'education
  :industry "Education" :degree 'bachelors
  :min-smarts 60 :min-looks 0
  :tiers '(("Teacher's Aide" . 30000)
           ("Teacher" . 50000)
           ("Department Head" . 75000)
           ("Principal" . 110000)
           ("Superintendent" . 160000)))

;;; -----------------------------------------------------------------
;;; Special-career tracks (separate handlers in addition to tiers)

(emacslife-register-job-track 'music
  :industry "Special: Music" :degree nil
  :min-smarts 0 :min-looks 0
  :tiers '(("Open Mic Performer" . 12000)
           ("Local Band Member" . 30000)
           ("Signed Recording Artist" . 120000)
           ("Touring Headliner" . 500000)
           ("Music Legend" . 1500000)))

(emacslife-register-job-track 'acting
  :industry "Special: Acting" :degree nil
  :min-smarts 0 :min-looks 50
  :tiers '(("Background Actor" . 18000)
           ("Bit-Part Actor" . 45000)
           ("Supporting Role Actor" . 150000)
           ("Lead Actor" . 600000)
           ("A-List Movie Star" . 2000000)))

(emacslife-register-job-track 'military
  :industry "Special: Military" :degree nil
  :min-smarts 40 :min-looks 0
  :tiers '(("Private" . 28000)
           ("Corporal" . 36000)
           ("Sergeant" . 48000)
           ("Lieutenant" . 70000)
           ("Captain" . 95000)
           ("Major" . 120000)
           ("Colonel" . 160000)
           ("General" . 220000)))

;;; -----------------------------------------------------------------
;;; Helpers

(defun emacslife--job-track (track)
  (or (alist-get track emacslife--job-tracks)
      (user-error "Unknown job track: %S" track)))

(defun emacslife--current-job-tier-info (char)
  "Return (TITLE . SALARY) for CHAR's current job tier, or nil."
  (when-let* ((job (emacslife-character-job char))
              (track (plist-get job :track))
              (tier (plist-get job :tier))
              (tiers (plist-get (emacslife--job-track track) :tiers)))
    (nth tier tiers)))

(defun emacslife--track-eligible-p (char track)
  "Does CHAR meet TRACK's entry requirements?"
  (let* ((p (emacslife--job-track track))
         (deg (plist-get p :degree))
         (min-smarts (plist-get p :min-smarts))
         (min-looks (plist-get p :min-looks)))
    (and (or (null deg) (memq deg (emacslife-character-degrees char)))
         (>= (emacslife-character-smarts char) min-smarts)
         (>= (emacslife-character-looks char) min-looks))))

;;; -----------------------------------------------------------------
;;; Job actions

(defun emacslife-apply-for-job (&optional track)
  "Apply for a job in TRACK; promotes/hires CHAR if eligible."
  (interactive)
  (emacslife-with-state char
    (when (emacslife-character-job char)
      (user-error "You already have a job — quit first"))
    (when (< (emacslife-character-age char) 16)
      (user-error "You're too young to work full-time"))
    (let* ((eligible (cl-remove-if-not
                      (lambda (cell) (emacslife--track-eligible-p char (car cell)))
                      emacslife--job-tracks))
           (rows (mapcar
                  (lambda (cell)
                    (let* ((tr (car cell))
                           (p (cdr cell))
                           (first (car (plist-get p :tiers))))
                      (cons (format "%-30s  %s  starts $%s"
                                    (plist-get p :industry)
                                    (car first)
                                    (emacslife-format-money (cdr first)))
                            tr)))
                  eligible))
           (_ (unless rows (user-error "No tracks you qualify for")))
           (track (or track
                      (cdr (assoc (completing-read "Apply to: "
                                                   (mapcar #'car rows) nil t)
                                  rows)))))
      (let* ((tiers (plist-get (emacslife--job-track track) :tiers))
             (first (car tiers)))
        (setf (emacslife-character-job char)
              (list :track track :title (car first)
                    :tier 0 :salary (cdr first)
                    :years 0 :performance 50))
        (emacslife-log char
                       (format "Hired as %s ($%s/yr)."
                               (car first)
                               (emacslife-format-money (cdr first)))
                       :good)
        (message "Hired."))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

(defun emacslife-work-harder ()
  "Boost performance, lose a bit of happiness."
  (interactive)
  (emacslife-with-state char
    (let ((job (emacslife-character-job char)))
      (unless job (user-error "You don't have a job"))
      (setf (plist-get job :performance)
            (emacslife-clamp (+ (plist-get job :performance) (+ 3 (random 5)))))
      (setf (emacslife-character-job char) job)
      (emacslife-bump-stat char :happiness -2)
      (emacslife-log char "Worked harder. Boss noticed." :neutral)
      (message "Performance bumped.")
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

(defun emacslife-ask-raise ()
  "Ask for a raise.  Chance scales with performance."
  (interactive)
  (emacslife-with-state char
    (let* ((job (emacslife-character-job char))
           (_ (unless job (user-error "You don't have a job")))
           (perf (plist-get job :performance))
           (chance (/ (- perf 30.0) 100.0)))
      (cond
       ((<= chance 0) (message "Boss laughed.") nil)
       ((emacslife-roll chance)
        (let ((new-salary (round (* (plist-get job :salary) 1.10))))
          (setf (plist-get job :salary) new-salary)
          (setf (emacslife-character-job char) job)
          (emacslife-log char (format "Got a 10%% raise — now $%s."
                                      (emacslife-format-money new-salary))
                         :good)
          (message "Cha-ching.")))
       (t
        (emacslife-bump-stat char :happiness -3)
        (emacslife-log char "Raise denied. Hello hostility." :bad)
        (message "Denied.")))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

(defun emacslife-ask-promotion ()
  "Try to climb to the next tier.  Requires high performance."
  (interactive)
  (emacslife-with-state char
    (let* ((job (emacslife-character-job char))
           (_ (unless job (user-error "You don't have a job")))
           (tiers (plist-get (emacslife--job-track
                              (plist-get job :track))
                             :tiers))
           (tier (plist-get job :tier))
           (next (nth (1+ tier) tiers))
           (_ (unless next (user-error "Already at the top tier")))
           (chance (/ (plist-get job :performance) 100.0)))
      (if (emacslife-roll (* chance 0.6))
          (progn
            (setf (plist-get job :tier) (1+ tier))
            (setf (plist-get job :title) (car next))
            (setf (plist-get job :salary) (cdr next))
            (setf (plist-get job :performance) 50)
            (setf (emacslife-character-job char) job)
            (emacslife-log char (format "PROMOTED to %s ($%s/yr)!"
                                        (car next)
                                        (emacslife-format-money (cdr next)))
                           :good)
            (message "Promoted."))
        (emacslife-bump-stat char :happiness -3)
        (emacslife-log char "Promotion denied." :bad)
        (message "Maybe next year."))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

(defun emacslife-quit-job ()
  "Quit your current job."
  (interactive)
  (emacslife-with-state char
    (let ((job (emacslife-character-job char)))
      (unless job (user-error "No job to quit"))
      (push (append job (list :ended-year
                              (+ (emacslife-character-birth-year char)
                                 (emacslife-character-age char))))
            (emacslife-character-job-history char))
      (setf (emacslife-character-job char) nil)
      (emacslife-bump-stat char :happiness 5)
      (emacslife-log char (format "Quit %s." (plist-get job :title)) :neutral)
      (message "Freedom.")
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

(defun emacslife-retire ()
  "Retire (only at 60+)."
  (interactive)
  (emacslife-with-state char
    (unless (>= (emacslife-character-age char) 60)
      (user-error "You can't retire until age 60"))
    (let ((job (emacslife-character-job char)))
      (when job
        (push (append job (list :ended-year
                                (+ (emacslife-character-birth-year char)
                                   (emacslife-character-age char))
                                :retired t))
              (emacslife-character-job-history char))
        (setf (emacslife-character-job char) nil))
      (emacslife-bump-stat char :happiness 15)
      (emacslife-bump-money char 50000)
      (emacslife-log char "Retired. Pension check delivered." :good)
      (message "Retired."))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; -----------------------------------------------------------------
;;; Yearly payroll & promotion-fire rolls (called from age-up)

(defun emacslife-process-job-year (char)
  "Apply one year of CHAR's job: salary paid, perf drift, possible fire,
then an automatic merit-and-COL raise scaled to current :performance.

The manual `emacslife-ask-raise' is still available on top of this — it
gives a bigger one-shot bump (10%) with a chance of denial; auto-raises
are the silent floor everyone gets just for showing up."
  (when-let* ((job (emacslife-character-job char)))
    (cl-incf (plist-get job :years))
    (emacslife-bump-money char (plist-get job :salary))
    ;; performance drifts toward 50 (mean reversion)
    (let ((p (plist-get job :performance)))
      (setf (plist-get job :performance)
            (emacslife-clamp (+ p (- (random 7) 3)))))
    ;; tiny chance of getting fired if perf < 25
    (when (and (< (plist-get job :performance) 25)
               (emacslife-roll 0.3))
      (push (append job (list :fired t
                              :ended-year (+ (emacslife-character-birth-year char)
                                             (emacslife-character-age char))))
            (emacslife-character-job-history char))
      (setf (emacslife-character-job char) nil)
      (emacslife-bump-stat char :happiness -10)
      (emacslife-log char (format "Fired from %s." (plist-get job :title))
                     :bad))
    ;; Auto annual raise — only if still employed AFTER the firing roll
    (when (emacslife-character-job char)
      (let* ((perf (plist-get job :performance))
             (raise-pct (+ emacslife-annual-col-raise
                           (* emacslife-annual-merit-raise-max
                              (/ perf 100.0))))
             (old-salary (plist-get job :salary))
             (new-salary (round (* old-salary (+ 1.0 raise-pct)))))
        (when (> new-salary old-salary)
          (setf (plist-get job :salary) new-salary)
          (emacslife-log
           char
           (format "Annual raise: +%.1f%% (perf %d) — salary now %s/yr."
                   (* 100 raise-pct) perf
                   (emacslife-format-money new-salary))
           :good)))
      (setf (emacslife-character-job char) job))))

;;; -----------------------------------------------------------------
;;; Sub-menu entry

(defun emacslife-career-menu ()
  "Show the career sub-menu."
  (interactive)
  (emacslife-with-state char
    (let* ((items (append
                   (unless (emacslife-character-job char)
                     '(("Apply for a job" . apply)))
                   (when (emacslife-character-job char)
                     '(("Work harder" . work)
                       ("Ask for a raise" . raise)
                       ("Ask for a promotion" . promote)
                       ("Quit" . quit)))
                   (when (>= (emacslife-character-age char) 60)
                     '(("Retire" . retire)))))
           (_ (unless items (user-error "No career actions right now")))
           (choice (completing-read "Career: " (mapcar #'car items) nil t))
           (action (cdr (assoc choice items))))
      (pcase action
        ('apply (emacslife-apply-for-job))
        ('work (emacslife-work-harder))
        ('raise (emacslife-ask-raise))
        ('promote (emacslife-ask-promotion))
        ('quit (emacslife-quit-job))
        ('retire (emacslife-retire))))))

(emacslife-register-action
 :career (lambda (_form) (emacslife-career-menu)))

(provide 'emacslife-career)
;;; emacslife-career.el ends here
