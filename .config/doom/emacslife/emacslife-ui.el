;;; emacslife-ui.el --- EmacsLife: dashboard + 4 org buffers -*- lexical-binding: t; -*-
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
;; Dashboard hub buffer (`*emacslife*'), the central place where
;; advancement happens.  Press `a' or RET to age up.  Four org-mode
;; buffers (stats / finances / relationships / recap) are accessible
;; via tab links and single-key shortcuts.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-family)
(require 'emacslife-relationships)
(require 'emacslife-activities)
(require 'emacslife-career)
(require 'emacslife-crime)
(require 'emacslife-assets)
(require 'emacslife-events)
(require 'emacslife-ribbons)
(require 'emacslife-save)
(require 'emacslife-instruments)
(require 'emacslife-skills)
(require 'emacslife-achievements)
(require 'emacslife-travel)

;;; -----------------------------------------------------------------
;;; Buffer names

(defconst emacslife-hub-buffer "*emacslife*")
(defconst emacslife-stats-buffer "*emacslife: stats*")
(defconst emacslife-finances-buffer "*emacslife: finances*")
(defconst emacslife-relationships-buffer "*emacslife: relationships*")
(defconst emacslife-recap-buffer "*emacslife: recap*")

(defvar emacslife--all-buffers
  (list emacslife-hub-buffer emacslife-stats-buffer
        emacslife-finances-buffer emacslife-relationships-buffer
        emacslife-recap-buffer))

;;; -----------------------------------------------------------------
;;; Per-share price formatter — fixed 2 decimals, comma separators.

(defun emacslife--fmt-price (n)
  "Format N as a per-share price: $1,253.28 style.
Adds thousands separators for legibility; always two decimals."
  (let* ((sign (if (< n 0) "-" ""))
         (abs (abs n))
         (whole (truncate abs))
         (frac (round (* 100 (- abs whole))))
         ;; Comma-separate the integer part
         (whole-str
          (let ((s (number-to-string whole))
                (out ""))
            (while (> (length s) 3)
              (setq out (concat "," (substring s -3) out))
              (setq s (substring s 0 -3)))
            (concat s out))))
    (format "%s%s.%02d" sign whole-str frac)))

;;; -----------------------------------------------------------------
;;; org-link generation helper (copied pattern from tramp-dashboard)

(defun emacslife--link (form label)
  "Build an `[[elisp:FORM][LABEL]]' org-link string."
  (format "[[elisp:%s][%s]]"
          (replace-regexp-in-string "\n" " " (prin1-to-string form))
          label))

;;; -----------------------------------------------------------------
;;; Hub buffer rendering

(defun emacslife--render-hub ()
  "Render the dashboard hub content into the current buffer."
  (let ((char emacslife--state))
    (unless char (error "No character loaded"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; ---------- HEADER ----------
      (insert (propertize "  ╔═══════════════════════════════════════════════╗\n"
                          'face 'emacslife-face-title))
      (insert (propertize "  ║                   EMACSLIFE                   ║\n"
                          'face 'emacslife-face-title))
      (insert (propertize "  ╚═══════════════════════════════════════════════╝\n"
                          'face 'emacslife-face-title))
      (insert "\n")
      ;; Name + age + country card
      (insert "  ")
      (insert (propertize (emacslife-character-full-name char)
                          'face 'emacslife-face-name))
      (insert "   ")
      (insert (propertize (format "Age %d"
                                  (emacslife-character-age char))
                          'face 'emacslife-face-age))
      (insert (format "   %s (%s)\n"
                      (emacslife-country-name char)
                      (emacslife-character-city char)))
      ;; Status flags
      (let ((flags '()))
        (cond
         ((not (emacslife-character-alive char)) (push "💀 DECEASED" flags))
         ((emacslife-in-prison-p char) (push "🔒 IN PRISON" flags))
         (t nil))
        (when (emacslife-character-spouse char) (push "💍 married" flags))
        (when (emacslife-character-children char)
          (push (format "👶 %d kid%s"
                        (length (emacslife-character-children char))
                        (if (= 1 (length (emacslife-character-children char))) "" "s"))
                flags))
        (when (emacslife-character-job char)
          (push (format "💼 %s" (plist-get (emacslife-character-job char) :title))
                flags))
        (when (> (emacslife-character-fame char) 0)
          (push (format "⭐ fame %d" (emacslife-character-fame char)) flags))
        (when flags
          (insert "  ")
          (insert (mapconcat #'identity (nreverse flags) "  ·  "))
          (insert "\n")))
      (insert "\n")
      ;; Stat bars
      (insert "  ")
      (insert (propertize "Happiness " 'face 'emacslife-face-stat-happiness))
      (insert (emacslife-bar (emacslife-character-happiness char) 12
                             'emacslife-face-stat-happiness))
      (insert (format "  %3d\n" (emacslife-character-happiness char)))
      (insert "  ")
      (insert (propertize "Health    " 'face 'emacslife-face-stat-health))
      (insert (emacslife-bar (emacslife-character-health char) 12
                             'emacslife-face-stat-health))
      (insert (format "  %3d\n" (emacslife-character-health char)))
      (insert "  ")
      (insert (propertize "Smarts    " 'face 'emacslife-face-stat-smarts))
      (insert (emacslife-bar (emacslife-character-smarts char) 12
                             'emacslife-face-stat-smarts))
      (insert (format "  %3d\n" (emacslife-character-smarts char)))
      (insert "  ")
      (insert (propertize "Looks     " 'face 'emacslife-face-stat-looks))
      (insert (emacslife-bar (emacslife-character-looks char) 12
                             'emacslife-face-stat-looks))
      (insert (format "  %3d\n" (emacslife-character-looks char)))
      ;; Money
      (insert "\n  ")
      (insert (propertize "Cash:  " 'face 'emacslife-face-section))
      (insert (propertize (emacslife-format-money (emacslife-character-cash char))
                          'face (if (>= (emacslife-character-cash char) 0)
                                    'emacslife-face-money
                                  'emacslife-face-debt)))
      (when (> (emacslife-character-debt char) 0)
        (insert "   ")
        (insert (propertize (format "Debt: %s"
                                    (emacslife-format-money
                                     (emacslife-character-debt char)))
                            'face 'emacslife-face-debt)))
      (insert "\n\n")
      ;; ---------- TAB BAR (clickable) ----------
      (insert "  ")
      (cond
       ((not (emacslife-character-alive char))
        (insert (emacslife--link '(emacslife-show-recap)
                                 "🪦 Recap"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-restart)
                                 "↻ New Life")))
       (t
        (insert (emacslife--link '(emacslife-age-up)
                                 (propertize " ➜  AGE UP "
                                             'face 'emacslife-face-tab)))
        (insert "   ")
        (insert (emacslife--link '(emacslife-show-stats) "📋 Stats"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-show-finances) "💰 Finances"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-show-relationships) "👥 People"))
        (insert "\n  ")
        (insert (emacslife--link
                 '(emacslife-leisure-menu)
                 "🎉 Leisure"))
        (insert "   ")
        (insert (emacslife--link
                 '(emacslife-pick-activity-and-run emacslife--state 'mind-body)
                 "🧘 Mind&Body"))
        (insert "   ")
        (insert (emacslife--link
                 '(emacslife-pick-activity-and-run emacslife--state 'education)
                 "🎓 School"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-career-menu) "💼 Career"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-crime-menu) "🔫 Crime"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-assets-menu) "🏦 Assets"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-skills-menu) "🎨 Skills"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-take-vacation) "✈️  Travel"))
        (when (emacslife-in-prison-p char)
          (insert "\n  ")
          (insert (emacslife--link '(emacslife-prison-action)
                                   (propertize "🔒 PRISON ACTIONS"
                                               'face 'emacslife-face-event-bad))))
        (when (emacslife-character-pets char)
          (insert "\n  ")
          (insert (emacslife--link
                   '(emacslife-pick-activity-and-run emacslife--state 'pet)
                   "🐾 Pets")))))
      (insert "\n  ")
      (insert (emacslife--link '(emacslife-save) "💾 Save"))
      (insert "   ")
      (insert (emacslife--link '(emacslife-quit) "🚪 Quit"))
      (insert "\n\n")
      (insert (propertize "  Navigation: " 'face 'emacslife-face-section))
      (insert "RET=age-up (or follow link)  ·  TAB/S-TAB=cycle links\n")
      (insert (propertize "  Doom shortcuts: " 'face 'emacslife-face-section))
      (insert "SPC m a=age  s=stats  f=finances  p=people  i=interact\n")
      (insert "                  l=leisure  m=mind&body  e=school  j=career  c=crime\n")
      (insert "                  b=assets  k=skills  t=travel  w=save  q=quit  r=refresh  ?=help\n")
      (insert (propertize "  Vim motions (hjkl, gg, G, w, $, …) " 'face 'emacslife-face-event-neutral))
      (insert (propertize "are NOT overridden.\n\n" 'face 'emacslife-face-event-neutral))
      ;; ---------- TIMELINE ----------
      (insert (propertize "  📜 Life Timeline\n"
                          'face 'emacslife-face-section))
      (insert (propertize "  ───────────────────────────────────────\n"
                          'face 'emacslife-face-section))
      (let ((entries (last (emacslife-character-timeline char) 40))
            (last-age nil))
        (dolist (e entries)
          (let ((age (car e))
                (text (plist-get (cdr e) :text))
                (tone (plist-get (cdr e) :tone))
                (year (plist-get (cdr e) :year)))
            (unless (equal age last-age)
              (insert "\n  ")
              (insert (propertize (format "Age %d (%d):"
                                          age year)
                                  'face 'emacslife-face-year))
              (insert "\n"))
            (setq last-age age)
            (insert "    • ")
            (insert (propertize text 'face
                                (pcase tone
                                  (:good 'emacslife-face-event-good)
                                  (:bad 'emacslife-face-event-bad)
                                  (:funny 'emacslife-face-event-funny)
                                  (_ 'emacslife-face-event-neutral))))
            (insert "\n"))))
      (insert "\n")
      (goto-char (point-min)))))

;;; -----------------------------------------------------------------
;;; Hub mode (org-mode derived for [[elisp:...]] links)
;;
;; Keybinding strategy — IMPORTANT for Doom/Evil compatibility:
;;
;;   * Do NOT shadow vim motions (hjkl, gg, G, w, b, e, $, ^, /, ?, n,
;;     N, p, P, y, d, c, s, a, i, o, q, m, …).  Under Doom/Evil those
;;     keys are how the user navigates; binding actions to them would
;;     hijack basic navigation.
;;
;;   * Primary interaction is via clickable org links (TAB / S-TAB to
;;     jump between them, RET to follow) plus our `RET' override that
;;     advances a year when point is not on a link.
;;
;;   * All explicit action shortcuts live under Doom's localleader
;;     (`SPC m'), wired via `map!' if Doom is loaded.  This way the
;;     vim user keeps motions and gets `SPC m a' = age, `SPC m s' =
;;     stats, etc.
;;
;;   * For plain (non-Evil) Emacs users, we also define the same
;;     single-key shortcuts on the buffer-local mode-map under a
;;     transient/dispatch keymap accessed via `?'.

(defvar emacslife-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    ;; Context-sensitive RET: follow link, else age up.
    (define-key map (kbd "RET")      #'emacslife--hub-ret)
    (define-key map (kbd "<return>") #'emacslife--hub-ret)
    ;; Help / dispatch — `?' is search-back in Evil normal state but
    ;; we explicitly avoid that binding below; here it's for plain
    ;; Emacs users only.
    (define-key map (kbd "?")        #'emacslife-show-keys)
    map)
  "Keymap for `emacslife-mode'.  Single-key shortcuts live under the
Doom localleader (`SPC m').")

(define-derived-mode emacslife-mode org-mode "EmacsLife"
  "Major mode for the EmacsLife dashboard hub buffer."
  (setq-local truncate-lines t)
  (setq-local org-link-elisp-confirm-function nil)
  (read-only-mode 1))

;; Evil-aware: bind ONLY `RET' / `<return>' in normal state so we
;; don't shadow any motion command.  Everything else goes through
;; Doom's `map!' localleader below.
(with-eval-after-load 'evil
  (evil-define-key* 'normal emacslife-mode-map
    (kbd "RET")      #'emacslife--hub-ret
    (kbd "<return>") #'emacslife--hub-ret))

;; Doom localleader bindings (`SPC m X' for any Doom user).  Wrapped
;; in `fboundp' so this file still loads cleanly under vanilla Emacs.
(when (fboundp 'map!)
  (eval
   '(map! :map emacslife-mode-map
          :localleader
          :desc "Age up"                "a" #'emacslife-age-up
          :desc "Stats sheet"           "s" #'emacslife-show-stats
          :desc "Finances"              "f" #'emacslife-show-finances
          :desc "Finances"              "$" #'emacslife-show-finances
          :desc "People (NPCs)"         "p" #'emacslife-show-relationships
          :desc "Interact with NPC"     "i" #'emacslife-interact
          :desc "Leisure / Going out"   "l" #'emacslife-leisure-menu
          :desc "Mind & Body"           "m" (lambda () (interactive)
                                              (emacslife-pick-activity-and-run
                                               emacslife--state 'mind-body))
          :desc "Education / School"    "e" (lambda () (interactive)
                                              (emacslife-pick-activity-and-run
                                               emacslife--state 'education))
          :desc "Career / Job"          "j" #'emacslife-career-menu
          :desc "Crime"                 "c" #'emacslife-crime-menu
          :desc "Assets / Bank"         "b" #'emacslife-assets-menu
          :desc "Skills / Hobbies"      "k" #'emacslife-skills-menu
          :desc "Travel / Vacation"     "t" #'emacslife-take-vacation
          :desc "Save"                  "w" #'emacslife-save
          :desc "Save (alt)"            "S" #'emacslife-save
          :desc "Quit EmacsLife"        "q" #'emacslife-quit
          :desc "Refresh dashboard"     "r" #'emacslife-ui-refresh
          :desc "Help / keybinds"       "?" #'emacslife-show-keys)))

(defun emacslife--hub-ret ()
  "If point is on an org link, follow it; otherwise advance age."
  (interactive)
  (let ((ctx (org-element-context)))
    (if (and ctx (memq (org-element-type ctx) '(link)))
        (org-open-at-point)
      (emacslife-age-up))))

;;; -----------------------------------------------------------------
;;; Help buffer — lists all keybinds (since we hide them from
;;; motions, we need a discoverable cheatsheet).

(defun emacslife-show-keys ()
  "Show all EmacsLife keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*emacslife: keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "EmacsLife keybindings\n"
                          'face 'emacslife-face-title))
      (insert (propertize "=====================\n\n"
                          'face 'emacslife-face-section))
      (insert "Primary interaction is via clickable org links — \n")
      (insert "use TAB / S-TAB to jump between actions, RET to follow.\n")
      (insert "RET on empty space advances a year.\n\n")
      (insert (propertize "Doom localleader actions (SPC m X):\n"
                          'face 'emacslife-face-section))
      (let ((rows
             '(("SPC m a" . "Age up one year")
               ("SPC m s" . "Open Stats sheet (org)")
               ("SPC m f" . "Open Finances (org)")
               ("SPC m $" . "Open Finances (alt key)")
               ("SPC m p" . "Open People / Relationships (org)")
               ("SPC m i" . "Pick an NPC to interact with")
               ("SPC m l" . "Leisure menu (movies, bar, club, hang out, …)")
               ("SPC m m" . "Mind & Body activities menu")
               ("SPC m e" . "Education / School menu")
               ("SPC m j" . "Career / Job menu")
               ("SPC m c" . "Crime menu")
               ("SPC m b" . "Assets / Bank menu")
               ("SPC m k" . "Skills / Hobbies menu")
               ("SPC m t" . "Travel / Vacation")
               ("SPC m w" . "Save current life")
               ("SPC m q" . "Quit EmacsLife (autosaves)")
               ("SPC m r" . "Refresh the dashboard")
               ("SPC m ?" . "This help buffer"))))
        (dolist (row rows)
          (insert (format "  %-10s  %s\n" (car row) (cdr row)))))
      (insert "\n")
      (insert (propertize "Always-on:\n" 'face 'emacslife-face-section))
      (insert "  RET / <return>   Follow link under point, else age up\n")
      (insert "  TAB / S-TAB      Cycle between action links (org)\n")
      (insert "  ?                This help (when not in evil normal state)\n")
      (insert "\n")
      (insert (propertize "Note: vim motions (hjkl, gg, G, w, $, …) are \n"
                          'face 'emacslife-face-event-neutral))
      (insert (propertize "all preserved — EmacsLife never shadows them.\n"
                          'face 'emacslife-face-event-neutral)))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*emacslife: keys*"))

;;; -----------------------------------------------------------------
;;; Refresh + show

(defun emacslife-ui-refresh ()
  "Re-render the hub (and any open emacslife buffers)."
  (interactive)
  (when (get-buffer emacslife-hub-buffer)
    (with-current-buffer emacslife-hub-buffer
      (emacslife--render-hub)))
  (dolist (b (list emacslife-stats-buffer emacslife-finances-buffer
                   emacslife-relationships-buffer))
    (when-let* ((buf (get-buffer b)))
      (with-current-buffer buf
        (when (boundp 'emacslife--render-fn)
          (funcall emacslife--render-fn))))))

(defun emacslife-show-hub ()
  "Open or focus the dashboard hub."
  (interactive)
  (let ((buf (get-buffer-create emacslife-hub-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'emacslife-mode)
        (emacslife-mode))
      (emacslife--render-hub))
    (pop-to-buffer buf)))

;;; -----------------------------------------------------------------
;;; Stats org buffer

(define-derived-mode emacslife-stats-mode org-mode "EmacsLife-Stats"
  "Major mode for the EmacsLife character-sheet org buffer."
  (setq-local truncate-lines nil)
  (setq-local org-link-elisp-confirm-function nil)
  (read-only-mode 1))

(defvar emacslife-stats-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for stats sheet — actions live under `SPC m'.")

;; Stats buffer: no normal-state shortcut shadowing.  The user reaches
;; back to the hub via the org link at the top, or `SPC m h'.
(when (fboundp 'map!)
  (eval
   '(map! :map emacslife-stats-mode-map
          :localleader
          :desc "Back to dashboard" "h" #'emacslife-show-hub
          :desc "Refresh"           "r" #'emacslife-show-stats
          :desc "Quit window"       "q" (lambda () (interactive) (quit-window))
          :desc "Help / keybinds"   "?" #'emacslife-show-keys)))

(defun emacslife--render-stats ()
  (let ((char emacslife--state)
        (inhibit-read-only t))
    (erase-buffer)
    (insert "#+title: " (emacslife-character-full-name char) " — Character Sheet\n")
    (insert "#+startup: showall indent overview\n\n")
    (insert (emacslife--link '(emacslife-show-hub) "← Back to dashboard"))
    (insert "    ")
    (insert (emacslife--link '(emacslife-show-stats) "↻ refresh"))
    (insert "\n\n")
    (insert "* Demographics\n")
    (insert "| Field        | Value |\n|--------------+-------|\n")
    (insert (format "| Name         | %s |\n" (emacslife-character-full-name char)))
    (insert (format "| Age          | %d |\n" (emacslife-character-age char)))
    (insert (format "| Born         | %d |\n" (emacslife-character-birth-year char)))
    (insert (format "| Gender       | %s |\n" (emacslife-character-gender char)))
    (insert (format "| Country      | %s |\n" (emacslife-country-name char)))
    (insert (format "| City         | %s |\n" (emacslife-character-city char)))
    (insert (format "| Alive        | %s |\n" (if (emacslife-character-alive char) "yes" "DECEASED")))
    (when-let* ((cause (emacslife-character-death-cause char)))
      (insert (format "| Cause        | %s |\n" cause)))
    (insert "\n* Stats\n")
    (dolist (stat '((:happiness . "Happiness") (:health . "Health")
                    (:smarts . "Smarts") (:looks . "Looks") (:karma . "Karma")))
      (insert (format "- %-10s %s  %d\n"
                      (cdr stat)
                      (emacslife-bar (emacslife-stat char (car stat)) 20)
                      (emacslife-stat char (car stat)))))
    (insert (format "- %-10s %s  %d\n"
                    "Fame"
                    (emacslife-bar (emacslife-character-fame char) 20)
                    (emacslife-character-fame char)))
    (insert "\n* Education\n")
    (insert (format "- Stage:    %s\n"
                    (emacslife-education-stage-name
                     (emacslife-character-education-stage char))))
    (insert (format "- GPA:      %.2f\n" (emacslife-character-gpa char)))
    (insert (format "- Major:    %s\n" (or (emacslife-character-major char) "—")))
    (insert (format "- Degrees:  %s\n"
                    (or (mapconcat #'symbol-name
                                   (emacslife-character-degrees char) ", ")
                        "none")))
    (insert "\n* Career\n")
    (if-let* ((j (emacslife-character-job char)))
        (insert (format "- Current:      %s ($%s/yr) — %d yrs, perf %d\n"
                        (plist-get j :title)
                        (emacslife-format-money (plist-get j :salary))
                        (plist-get j :years)
                        (plist-get j :performance)))
      (insert "- Current:      unemployed\n"))
    (when (emacslife-character-job-history char)
      (insert "** Job History\n")
      (dolist (j (emacslife-character-job-history char))
        (insert (format "- %s ($%s/yr)%s\n"
                        (plist-get j :title)
                        (emacslife-format-money (plist-get j :salary))
                        (cond ((plist-get j :fired) " — fired")
                              ((plist-get j :retired) " — retired")
                              ((plist-get j :laid-off) " — laid off")
                              (t ""))))))
    (insert "\n* Legal Record\n")
    (if (emacslife-character-jail-record char)
        (dolist (r (emacslife-character-jail-record char))
          (insert (format "- %s — %d yrs (%d)\n"
                          (plist-get r :crime)
                          (plist-get r :sentence)
                          (or (plist-get r :year) 0))))
      (insert "- (clean record)\n"))
    (insert (format "- Total prison years served: %d\n"
                    (emacslife-character-prison-time-served char)))
    (insert "\n* Skills\n")
    (dolist (cell emacslife-skill-names)
      (let ((lvl (emacslife-skill char (car cell))))
        (when (> lvl 0)
          (insert (format "- %-14s %s  %d\n"
                          (cdr cell)
                          (emacslife-bar lvl 20) lvl)))))
    (unless (cl-some (lambda (cell) (> (emacslife-skill char (car cell)) 0))
                     emacslife-skill-names)
      (insert "- (no skills yet — try SPC m k)\n"))
    (let ((spoken (emacslife-languages-spoken char)))
      (insert "\n* Languages\n")
      (if spoken
          (dolist (lang spoken)
            (insert (format "- %-12s  level %d\n"
                            (cdr (assq lang emacslife-language-names))
                            (emacslife-language-level char lang))))
        (insert "- (no fluent languages yet)\n")))
    (let ((stamps (emacslife-passport-stamps char)))
      (insert "\n* Passport Stamps\n")
      (if stamps
          (dolist (s stamps)
            (insert (format "- %s\n"
                            (plist-get (emacslife-country s) :name))))
        (insert "- (no stamps — try SPC m t)\n")))
    (insert "\n* Achievements\n")
    (if (emacslife-character-achievements char)
        (dolist (a (emacslife-character-achievements char))
          (let ((entry (emacslife-achievement a)))
            (when entry
              (insert (format "- 🏅 *%s* — %s\n"
                              (plist-get entry :name)
                              (plist-get entry :description))))))
      (insert "- (none yet — keep playing)\n"))
    (insert "\n* Ribbons (awarded at death)\n")
    (if (emacslife-character-ribbons char)
        (dolist (r (emacslife-character-ribbons char))
          (insert (format "- 🏆 %s\n" r)))
      (insert "- (will be awarded at end of life)\n"))
    (goto-char (point-min))))

(defvar emacslife--render-fn nil
  "Buffer-local — set in each org buffer to the renderer to re-run on refresh.")

(defun emacslife-show-stats ()
  "Open the character-sheet org buffer."
  (interactive)
  (emacslife-with-state _c
    (let ((buf (get-buffer-create emacslife-stats-buffer)))
      (with-current-buffer buf
        (unless (derived-mode-p 'emacslife-stats-mode)
          (emacslife-stats-mode))
        (setq-local emacslife--render-fn #'emacslife--render-stats)
        (emacslife--render-stats))
      (pop-to-buffer buf))))

;;; -----------------------------------------------------------------
;;; Finances org buffer

(define-derived-mode emacslife-finances-mode org-mode "EmacsLife-$"
  "Major mode for the EmacsLife financial-statement org buffer."
  (setq-local truncate-lines nil)
  (setq-local org-link-elisp-confirm-function nil)
  (read-only-mode 1))

(defvar emacslife-finances-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for finances buffer — actions live under `SPC m'.")

(when (fboundp 'map!)
  (eval
   '(map! :map emacslife-finances-mode-map
          :localleader
          :desc "Back to dashboard" "h" #'emacslife-show-hub
          :desc "Refresh"           "r" #'emacslife-show-finances
          :desc "Quit window"       "q" (lambda () (interactive) (quit-window))
          :desc "Help / keybinds"   "?" #'emacslife-show-keys)))

(defun emacslife--render-finances ()
  (let ((char emacslife--state)
        (inhibit-read-only t))
    (erase-buffer)
    (insert "#+title: " (emacslife-character-full-name char) " — Financial Statement\n")
    (insert "#+startup: showall indent overview\n\n")
    (insert (emacslife--link '(emacslife-show-hub) "← Back to dashboard"))
    (insert "    ")
    (insert (emacslife--link '(emacslife-show-finances) "↻ refresh"))
    (insert "\n\n")
    (insert "* Balance Sheet\n")
    (insert "| Item                | Value |\n|---------------------+-------|\n")
    (insert (format "| Cash                | %s |\n"
                    (emacslife-format-money (emacslife-character-cash char))))
    (insert (format "| Localized           | %s |\n"
                    (emacslife-localized-money
                     char (emacslife-character-cash char))))
    (let ((asset-total (apply #'+ (mapcar (lambda (a)
                                            (or (plist-get a :purchase-price) 0))
                                          (emacslife-character-assets char))))
          ;; Investments use position-based market value; legacy lump
          ;; entries are valued by `emacslife--position-value' too.
          (inv-total (emacslife-portfolio-value char)))
      (insert (format "| Assets (at value)   | %s |\n"
                      (emacslife-format-money asset-total)))
      (insert (format "| Investments         | %s |\n"
                      (emacslife-format-money inv-total)))
      (insert (format "| Debt                | %s |\n"
                      (emacslife-format-money (emacslife-character-debt char))))
      (insert (format "| =NET WORTH=         | =%s= |\n"
                      (emacslife-format-money (emacslife-net-worth char)))))
    (insert (format "\nCredit score: %d\n\n" (emacslife-character-credit-score char)))
    (insert "* Income (this year, estimate)\n")
    (insert "| Source        | Amount |\n|---------------+--------|\n")
    (insert (format "| Job salary    | %s |\n"
                    (if-let* ((j (emacslife-character-job char)))
                        (emacslife-format-money (plist-get j :salary))
                      "—")))
    (let ((rents (apply #'+ (mapcar (lambda (a) (or (plist-get a :rental-income) 0))
                                    (emacslife-character-assets char))))
          (divs (emacslife-portfolio-annual-income char)))
      (when (> rents 0)
        (insert (format "| Rental income | %s |\n"
                        (emacslife-format-money rents))))
      (when (> divs 0)
        (insert (format "| Portfolio yield | %s |\n"
                        (emacslife-format-money divs)))))
    (insert "\n* Assets Owned\n")
    (if (emacslife-character-assets char)
        (dolist (a (emacslife-character-assets char))
          (insert (format "- %s (%s) — value %s — bought %d for %s\n"
                          (plist-get a :name)
                          (plist-get a :kind)
                          (emacslife-format-money (plist-get a :purchase-price))
                          (plist-get a :purchase-year)
                          (emacslife-format-money (plist-get a :purchase-price)))))
      (insert "- (none)\n"))
    (insert "\n* Portfolio\n")
    (let* ((positions (emacslife-character-investments char))
           (cost (emacslife-portfolio-cost char))
           (value (emacslife-portfolio-value char))
           (income (emacslife-portfolio-annual-income char))
           (lifetime-divs (emacslife--lifetime-dividends char)))
      (cond
       ((null positions)
        (insert "- (no positions — buy some via Invest below)\n")
        (when (> lifetime-divs 0)
          (insert (format "\nLifetime dividends/distributions received: %s\n"
                          (emacslife-format-money lifetime-divs)))))
       (t
        ;; Write raw cells (one space padding for legibility); call
        ;; `org-table-align' afterwards so org handles all alignment.
        (let ((table-start (point)))
          (insert "| Ticker | Type | Shares | Basis | Price | Value | P/L | % | Yield | Income/yr |\n")
          (insert "|-\n")
          (dolist (p positions)
            (cond
             ((plist-get p :symbol)
              (let* ((sym (plist-get p :symbol))
                     (i (emacslife-instrument sym))
                     (type (or (plist-get i :type) 'unknown))
                     (sh (plist-get p :shares))
                     (basis (plist-get p :cost-basis))
                     (px (emacslife--current-price char sym))
                     (val (round (* sh px)))
                     (pl (round (* sh (- px basis))))
                     (pct (if (> basis 0)
                              (* 100.0 (/ (- px basis) basis)) 0.0))
                     (yld (or (plist-get i :dividend-yield) 0.0))
                     (inc (emacslife-position-annual-income char p)))
                (insert (format "| %s | %s | %d | $%s | $%s | %s | %s | %s%.1f%% | %.2f%% | %s |\n"
                                sym
                                (symbol-name type)
                                sh
                                (emacslife--fmt-price basis)
                                (emacslife--fmt-price px)
                                (emacslife-format-money val)
                                (emacslife-format-money pl)
                                (if (>= pct 0) "+" "") pct
                                (* 100 yld)
                                (emacslife-format-money inc)))))
             (t
              (insert (format "| LEGACY | — | — | — | — | %s | — | — | — | — |\n"
                              (emacslife-format-money
                               (emacslife--position-value char p)))))))
          ;; Totals row
          (insert "|-\n")
          (insert (format "| TOTAL | | | | | %s | %s | %s%.1f%% | | %s |\n"
                          (emacslife-format-money value)
                          (emacslife-format-money (- value cost))
                          (if (>= value cost) "+" "")
                          (if (> cost 0)
                              (* 100.0 (/ (- (float value) cost) cost))
                            0.0)
                          (emacslife-format-money income)))
          ;; Let org align the table for us — handles all column widths.
          (save-excursion
            (goto-char table-start)
            (forward-line 1)
            (when (and (fboundp 'org-table-align)
                       (org-at-table-p))
              (ignore-errors (org-table-align)))))
        (insert (format "\nLifetime dividends/distributions received: %s\n"
                        (emacslife-format-money lifetime-divs))))))
    (insert "\n* Quick Actions\n")
    (insert (emacslife--link '(emacslife-bank-menu) "🏦 Bank"))
    (insert "   ")
    (insert (emacslife--link '(emacslife-invest-buy) "📈 Buy"))
    (insert "   ")
    (insert (emacslife--link '(emacslife-invest-sell) "💸 Sell"))
    (insert "   ")
    (insert (emacslife--link '(emacslife-portfolio-summary) "📊 Summary"))
    (insert "   ")
    (insert (emacslife--link '(emacslife-buy-lottery-ticket) "🎰 Lottery"))
    (insert "   ")
    (insert (emacslife--link '(emacslife-casino-blackjack) "🃏 Blackjack"))
    (insert "   ")
    (insert (emacslife--link '(emacslife-casino-slots) "🎲 Slots"))
    (insert "\n")
    (goto-char (point-min))))

(defun emacslife-show-finances ()
  "Open the financial statement org buffer."
  (interactive)
  (emacslife-with-state _c
    (let ((buf (get-buffer-create emacslife-finances-buffer)))
      (with-current-buffer buf
        (unless (derived-mode-p 'emacslife-finances-mode)
          (emacslife-finances-mode))
        (setq-local emacslife--render-fn #'emacslife--render-finances)
        (emacslife--render-finances))
      (pop-to-buffer buf))))

;;; -----------------------------------------------------------------
;;; Relationships org buffer

(define-derived-mode emacslife-relationships-mode org-mode "EmacsLife-People"
  "Major mode for the EmacsLife relationships browser org buffer."
  (setq-local truncate-lines nil)
  (setq-local org-link-elisp-confirm-function nil)
  (read-only-mode 1))

(defvar emacslife-relationships-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for relationships browser — actions live under `SPC m'.")

(when (fboundp 'map!)
  (eval
   '(map! :map emacslife-relationships-mode-map
          :localleader
          :desc "Back to dashboard"   "h" #'emacslife-show-hub
          :desc "Refresh"             "r" #'emacslife-show-relationships
          :desc "Interact (pick NPC)" "i" #'emacslife-interact
          :desc "Quit window"         "q" (lambda () (interactive) (quit-window))
          :desc "Help / keybinds"     "?" #'emacslife-show-keys)))

(defun emacslife--render-relationships ()
  (let ((char emacslife--state)
        (inhibit-read-only t))
    (erase-buffer)
    (insert "#+title: " (emacslife-character-full-name char) " — Relationships\n")
    (insert "#+startup: showall indent overview\n\n")
    (insert (emacslife--link '(emacslife-show-hub) "← Back to dashboard"))
    (insert "    ")
    (insert (emacslife--link '(emacslife-show-relationships) "↻ refresh"))
    (insert "    ")
    (insert (emacslife--link '(emacslife-interact)
                             "💬 Pick someone to interact with"))
    (insert "\n\n")
    (dolist (group (emacslife-npcs-grouped char))
      (let ((name (car group))
            (npcs (cdr group)))
        (when npcs
          (insert (format "* %s\n" name))
          (dolist (npc npcs)
            (insert (format "** %s%s — %s, age %d  bar %s %d\n"
                            (emacslife-npc-name npc)
                            (if (emacslife-npc-surname npc)
                                (format " %s" (emacslife-npc-surname npc))
                              "")
                            (emacslife--relation-display-name
                             (emacslife-npc-relation npc))
                            (emacslife-npc-age npc)
                            (emacslife-bar (emacslife-npc-bar npc) 10)
                            (emacslife-npc-bar npc)))
            (when (emacslife-npc-job npc)
              (insert (format "   - Job: %s\n" (emacslife-npc-job npc))))
            (insert "   - ")
            (insert (emacslife--link
                     `(emacslife-interact ',(emacslife-npc-id npc))
                     "Interact"))
            (insert "\n")))))
    (when (cl-every (lambda (g) (null (cdr g)))
                    (emacslife-npcs-grouped char))
      (insert "(No relationships yet — get out there.)\n"))
    (goto-char (point-min))))

(defun emacslife-show-relationships ()
  "Open the relationships browser org buffer."
  (interactive)
  (emacslife-with-state _c
    (let ((buf (get-buffer-create emacslife-relationships-buffer)))
      (with-current-buffer buf
        (unless (derived-mode-p 'emacslife-relationships-mode)
          (emacslife-relationships-mode))
        (setq-local emacslife--render-fn #'emacslife--render-relationships)
        (emacslife--render-relationships))
      (pop-to-buffer buf))))

;;; -----------------------------------------------------------------
;;; Recap / death screen

(define-derived-mode emacslife-recap-mode org-mode "EmacsLife-Recap"
  "Major mode for the EmacsLife end-of-life recap buffer."
  (setq-local truncate-lines nil)
  (setq-local org-link-elisp-confirm-function nil)
  (read-only-mode 1))

(defvar emacslife-recap-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for life recap buffer — actions live under `SPC m'.")

(when (fboundp 'map!)
  (eval
   '(map! :map emacslife-recap-mode-map
          :localleader
          :desc "Start a new life"  "n" #'emacslife-restart
          :desc "Export recap"      "x" #'emacslife-export-recap
          :desc "Quit window"       "q" (lambda () (interactive) (quit-window))
          :desc "Help / keybinds"   "?" #'emacslife-show-keys)))

(defun emacslife-show-recap ()
  "Show the end-of-life recap buffer."
  (interactive)
  (emacslife-with-state char
    (let ((buf (get-buffer-create emacslife-recap-buffer))
          (inhibit-read-only t))
      (with-current-buffer buf
        (emacslife-recap-mode)
        (erase-buffer)
        (insert "#+title: " (emacslife-character-full-name char) " — In Memoriam\n")
        (insert "#+startup: showall indent\n\n")
        (insert "* 🪦 Obituary\n\n")
        (insert (format "%s of %s passed away at age %d.\n"
                        (emacslife-character-full-name char)
                        (emacslife-country-name char)
                        (emacslife-character-age char)))
        (when-let* ((cause (emacslife-character-death-cause char)))
          (insert (format "Cause: %s.\n" cause)))
        (insert "\n* 🎖️ Ribbon\n\n")
        (dolist (r (emacslife-character-ribbons char))
          (let ((entry (alist-get r emacslife--ribbons)))
            (insert (propertize (format "  🏆 %s — %s\n"
                                        (plist-get entry :name)
                                        (plist-get entry :description))
                                'face 'emacslife-face-ribbon))))
        (insert "\n* Final stats\n")
        (insert (format "- Happiness: %d\n" (emacslife-character-happiness char)))
        (insert (format "- Health:    %d\n" (emacslife-character-health char)))
        (insert (format "- Smarts:    %d\n" (emacslife-character-smarts char)))
        (insert (format "- Looks:     %d\n" (emacslife-character-looks char)))
        (insert (format "- Karma:     %d\n" (emacslife-character-karma char)))
        (insert (format "- Fame:      %d\n" (emacslife-character-fame char)))
        (insert (format "- Net worth: %s\n" (emacslife-format-money
                                             (emacslife-net-worth char))))
        (insert "\n* Notable timeline highlights\n")
        (dolist (e (last (emacslife-character-timeline char) 25))
          (insert (format "- Age %d: %s\n"
                          (car e)
                          (plist-get (cdr e) :text))))
        (insert "\n")
        (insert (emacslife--link '(emacslife-export-recap)
                                 "📤 Export full recap to notes"))
        (insert "   ")
        (insert (emacslife--link '(emacslife-restart)
                                 "↻ Start a new life"))
        (insert "\n")
        (goto-char (point-min)))
      (pop-to-buffer buf))))

;;; -----------------------------------------------------------------
;;; Age-up loop

(defun emacslife-age-up ()
  "Advance one year.  The central dashboard action."
  (interactive)
  (emacslife-with-state char
    (unless (emacslife-character-alive char)
      (user-error "You're dead.  Press n in the recap buffer or M-x emacslife"))
    (cl-incf (emacslife-character-age char))
    ;; Aging NPCs first so events can reference accurate ages
    (emacslife-age-npcs char)
    ;; Stat decay starting age 50ish
    (emacslife--natural-stat-drift char)
    ;; Education auto-progress
    (emacslife-auto-school-progress char)
    ;; Job year
    (emacslife-process-job-year char)
    ;; Prison year
    (emacslife-process-prison-year char)
    ;; Asset/debt processing
    (emacslife-process-assets-year char)
    ;; Market regime + dividends
    (emacslife-process-market-year char)
    ;; Random events
    (emacslife-fire-events char)
    ;; Achievements
    (emacslife-check-achievements char)
    ;; Death roll
    (emacslife--death-roll char)
    ;; Logged & refreshed
    (emacslife-autosave-maybe)
    (emacslife-ui-refresh)
    (cond
     ((not (emacslife-character-alive char))
      (emacslife-award-ribbon char)
      (emacslife-show-recap))
     (t (message "You are now %d." (emacslife-character-age char))))))

(defun emacslife--natural-stat-drift (char)
  "Apply small per-year natural changes to CHAR."
  (let ((age (emacslife-character-age char)))
    (cond
     ((> age 60)
      (emacslife-bump-stat char :health -1)
      (emacslife-bump-stat char :looks -1))
     ((> age 40)
      (when (emacslife-roll 0.3) (emacslife-bump-stat char :looks -1)))
     ((< age 18)
      (when (emacslife-roll 0.3) (emacslife-bump-stat char :health 1))))))

(defun emacslife--death-roll (char)
  "Possibly kill CHAR based on age and health."
  (let* ((age (emacslife-character-age char))
         (health (emacslife-character-health char))
         (base (cond
                ((< age 30) 0.001)
                ((< age 50) 0.005)
                ((< age 65) 0.02)
                ((< age 75) 0.06)
                ((< age 85) 0.15)
                ((< age 95) 0.35)
                ((< age 110) 0.55)
                (t 1.0)))
         (mult (max 0.2 (/ (- 110.0 health) 80.0))))
    (when (emacslife-roll (* base mult))
      (setf (emacslife-character-alive char) nil)
      (setf (emacslife-character-death-cause char)
            (emacslife-pick '("old age" "heart attack" "stroke"
                              "freak accident" "the flu" "complications"
                              "an unfortunate ladder incident"
                              "a bee sting")))
      (emacslife-log char
                     (format "You died at age %d (%s)."
                             age (emacslife-character-death-cause char))
                     :bad))))

;;; -----------------------------------------------------------------
;;; Entry / quit / restart

(defun emacslife-quit ()
  "Save and quit EmacsLife buffers."
  (interactive)
  (when emacslife--state (emacslife-autosave-maybe))
  (dolist (b emacslife--all-buffers)
    (when-let* ((buf (get-buffer b))) (kill-buffer buf)))
  (message "Saved.  Bye."))

(defun emacslife-restart ()
  "Start a new life (with slot picker)."
  (interactive)
  (setq emacslife--state nil
        emacslife--current-slot nil)
  (emacslife-pick-slot-and-load)
  (emacslife-show-hub))

;;; Dispatcher hooks
(emacslife-register-action :age  (lambda (_) (emacslife-age-up)))
(emacslife-register-action :tab  (lambda (form)
                                   (pcase (cadr form)
                                     ('stats (emacslife-show-stats))
                                     ('finances (emacslife-show-finances))
                                     ('people (emacslife-show-relationships))
                                     ('recap (emacslife-show-recap))
                                     ('hub (emacslife-show-hub)))))
(emacslife-register-action :save (lambda (_) (emacslife-save)))
(emacslife-register-action :quit (lambda (_) (emacslife-quit)))

(provide 'emacslife-ui)
;;; emacslife-ui.el ends here
