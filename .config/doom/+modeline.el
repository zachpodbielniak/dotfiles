;;; +modeline.el -*- lexical-binding: t; -*-
;;
;; +modeline.el - tmux-style doom-modeline customization (Catppuccin)
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
;; Custom doom-modeline that mirrors the user's tmux status bar:
;;   left:  evil-state | buffer | git-branch | major-mode | diagnostics |
;;          encoding/line-endings | battery
;;   right: line:col | time | pomodoro | days-since trackers
;;
;; Custom segments:
;;   `days-since' — calls the `days_since' shell script for three
;;     trackers (carnivore / soda / coffee).  Cached and refreshed every
;;     60 s via a single timer.
;;   `pomodoro'  — calls the `pomo' script.  Cached and refreshed every
;;     15 s (used to be 5 s — too aggressive at idle).
;;
;; Both segments use Catppuccin hex literals inline rather than theme
;; faces so colors stay stable across theme reloads.

;;; Code:

(after! doom-modeline
  ;; Enable battery and time display (skip battery on headless/servers)
  (when (and (display-graphic-p)
             (not (string-empty-p (string-trim (shell-command-to-string "upower -e 2>/dev/null")))))
    (display-battery-mode 1))
  (display-time-mode 1)

  ;; General appearance
  (setq doom-modeline-height 28
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 25
        doom-modeline-time-icon t
        ;; No "live" animated clock glyph — it ticks at 1 Hz, invalidates
        ;; the modeline, triggers redisplay + a wl_surface commit on every
        ;; tick, and you can't actually tell the difference at a glance.
        doom-modeline-time-live-icon nil
        doom-modeline-battery (display-graphic-p)
        doom-modeline-env-version t
        doom-modeline-modal-icon t
        doom-modeline-modal-modern-icon t)

  ;; Time format (matches tmux  %H:%M)
  (setq display-time-format " %H:%M"
        display-time-default-load-average nil)

  ;; ---------------------------------------------------------------------------
  ;; Custom segment: days-since trackers (cached, updates every 60s)
  ;; Matches tmux right status: 🥩:days 🥤:days ☕:days
  ;; ---------------------------------------------------------------------------
  (defvar zach-modeline--days-cache ""
    "Cached string for days-since trackers.")

  (defvar zach-modeline--days-timer nil
    "Timer for updating days-since cache.")

  (defun zach-modeline--update-days ()
    "Update the days-since cache by calling the days_since script."
    (let ((carnivore (string-trim (shell-command-to-string "days_since 2024-11-24")))
          (soda      (string-trim (shell-command-to-string "days_since 2025-07-14")))
          (coffee    (string-trim (shell-command-to-string "days_since 2025-09-20"))))
      (setq zach-modeline--days-cache
            (concat
             (propertize (format " 🥩:%s" carnivore) 'face '(:foreground "#f38ba8"))
             (propertize (format " 🥤:%s" soda)      'face '(:foreground "#89b4fa"))
             (propertize (format " ☕:%s" coffee)     'face '(:foreground "#a6e3a1"))
             " "))))

  ;; Update immediately at startup, then every 60 seconds
  (zach-modeline--update-days)
  (when zach-modeline--days-timer (cancel-timer zach-modeline--days-timer))
  (setq zach-modeline--days-timer
        (run-with-timer 60 60 #'zach-modeline--update-days))

  (doom-modeline-def-segment days-since
    "Days-since trackers: carnivore, soda, coffee."
    zach-modeline--days-cache)

  ;; ---------------------------------------------------------------------------
  ;; Custom segment: pomodoro timer (cached, updates every 5s)
  ;; ---------------------------------------------------------------------------
  (defvar zach-modeline--pomo-cache ""
    "Cached string for pomodoro status.")

  (defvar zach-modeline--pomo-timer nil
    "Timer for updating pomodoro cache.")

  (defun zach-modeline--update-pomo ()
    "Update the pomodoro cache by calling the pomo script."
    (let ((pomo (string-trim (shell-command-to-string "pomo"))))
      (setq zach-modeline--pomo-cache
            (if (string-empty-p pomo) ""
              (propertize (format " %s" pomo) 'face '(:foreground "#fab387"))))))

  (zach-modeline--update-pomo)
  (when zach-modeline--pomo-timer (cancel-timer zach-modeline--pomo-timer))
  ;; 15 s is plenty for a pomodoro display — every 5 s was spawning
  ;; a subprocess 12×/minute at idle (which also hit the redisplay +
  ;; GC path each time).
  (setq zach-modeline--pomo-timer
        (run-with-timer 15 15 #'zach-modeline--update-pomo))

  (doom-modeline-def-segment pomodoro
    "Pomodoro timer status."
    zach-modeline--pomo-cache)

  ;; ---------------------------------------------------------------------------
  ;; Custom modeline layout
  ;; Left:  evil-state | buffer | git-branch | major-mode | diagnostics |
  ;;        encoding/line-endings | battery
  ;; Right: line:col | time | pomodoro | days-since trackers
  ;; ---------------------------------------------------------------------------
  (doom-modeline-def-modeline 'zach-modeline
    '(modals matches buffer-info remote-host vcs major-mode check
      buffer-encoding battery)
    '(misc-info buffer-position time pomodoro days-since))

  ;; Apply to all buffers
  (add-hook 'doom-modeline-mode-hook
            (lambda () (doom-modeline-set-modeline 'zach-modeline 'default))))

(provide '+modeline)
;;; +modeline.el ends here
