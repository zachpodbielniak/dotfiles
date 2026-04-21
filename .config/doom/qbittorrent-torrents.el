;;; qbittorrent-torrents.el --- Live torrent list UI for qBittorrent -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zach Podbielniak
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.

;;; Commentary:

;; Tabulated-list UI for qBittorrent torrents.  Fetches data via the
;; WebUI API (see qbittorrent-webui.el), auto-refreshes on a timer, and
;; exposes single-key actions for pause / resume / delete / recheck.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)
(require 'qbittorrent-webui)

(defgroup qbittorrent-torrents nil
  "qBittorrent torrents-list UI."
  :group 'qbittorrent-webui
  :prefix "qbittorrent-torrents-")

(defcustom qbittorrent-torrents-refresh-interval 3
  "Seconds between auto-refreshes of the torrents list buffer.
Set to nil to disable auto-refresh."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'qbittorrent-torrents)

(defcustom qbittorrent-torrents-confirm-delete t
  "When non-nil, prompt for confirmation before deleting a torrent."
  :type 'boolean
  :group 'qbittorrent-torrents)

(defcustom qbittorrent-torrents-buffer-name "*qbittorrent*"
  "Buffer name used by `qbittorrent-torrents'."
  :type 'string
  :group 'qbittorrent-torrents)

(defvar-local qbittorrent-torrents--timer nil
  "Per-buffer auto-refresh timer.")

(defvar-local qbittorrent-torrents--entries nil
  "Raw per-torrent alists for the current buffer (indexed by hash).")

;;; ---------------------------------------------------------------------
;;; Formatting helpers

(defun qbittorrent-torrents--human-size (bytes)
  (cond
   ((not (numberp bytes)) "?")
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fK" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fM" (/ bytes 1024.0 1024.0)))
   (t (format "%.2fG" (/ bytes 1024.0 1024.0 1024.0)))))

(defun qbittorrent-torrents--human-speed (bytes)
  (if (and (numberp bytes) (zerop bytes))
      "-"
    (concat (qbittorrent-torrents--human-size bytes) "/s")))

(defun qbittorrent-torrents--human-eta (seconds)
  (cond
   ((not (numberp seconds)) "?")
   ;; qBittorrent uses 8640000 (100 days) as a "never" sentinel.
   ((>= seconds 8640000) "---")
   ((< seconds 60)    (format "%ds" seconds))
   ((< seconds 3600)  (format "%dm" (/ seconds 60)))
   ((< seconds 86400) (format "%dh%02dm" (/ seconds 3600) (/ (mod seconds 3600) 60)))
   (t                 (format "%dd" (/ seconds 86400)))))

(defun qbittorrent-torrents--state-rank (state)
  "Priority rank for STATE: lower = higher in the list.
Groups: active downloads → stalled/queued DL → paused DL → checking/moving
→ errors → active uploads → stalled/queued UP → paused UP."
  (pcase state
    ("downloading"         10)
    ("forcedDL"            11)
    ("metaDL"              12)
    ("allocating"          13)
    ("stalledDL"           20)
    ("queuedDL"            21)
    ("pausedDL"            30)
    ("stoppedDL"           31)
    ("checkingDL"          40)
    ("checkingResumeData"  41)
    ("moving"              42)
    ("error"               50)
    ("missingFiles"        51)
    ("uploading"           60)
    ("forcedUP"            61)
    ("stalledUP"           70)
    ("queuedUP"            71)
    ("pausedUP"            80)
    ("stoppedUP"           81)
    ("checkingUP"          90)
    (_                    100)))

(defun qbittorrent-torrents--state-label (state)
  "Short abbreviated label for a qBittorrent STATE string."
  (pcase state
    ("downloading"         "Down")
    ("metaDL"              "Meta")
    ("stalledDL"           "StDL")
    ("pausedDL"            "Paus")
    ("stoppedDL"           "Stop")
    ("queuedDL"            "Queu")
    ("checkingDL"          "Chk")
    ("forcedDL"            "FDL")
    ("uploading"           "Seed")
    ("stalledUP"           "StUP")
    ("pausedUP"            "Paus")
    ("stoppedUP"           "Stop")
    ("queuedUP"            "Queu")
    ("checkingUP"          "Chk")
    ("forcedUP"            "FUP")
    ("allocating"          "Aloc")
    ("checkingResumeData"  "Chk")
    ("moving"              "Move")
    ("error"               "ERR")
    ("missingFiles"        "Miss")
    (_ (if (and (stringp state) (> (length state) 0))
           (substring state 0 (min 5 (length state)))
         "?"))))

;;; ---------------------------------------------------------------------
;;; Row construction

(defun qbittorrent-torrents--entry-to-row (entry)
  (let* ((hash     (alist-get 'hash entry))
         (state    (or (alist-get 'state entry) ""))
         (progress (or (alist-get 'progress entry) 0))
         (dlspeed  (or (alist-get 'dlspeed entry) 0))
         (upspeed  (or (alist-get 'upspeed entry) 0))
         (size     (or (alist-get 'size entry)
                       (alist-get 'total_size entry)
                       0))
         (eta      (or (alist-get 'eta entry) 0))
         (name     (or (alist-get 'name entry) "")))
    (list hash
          (vector (qbittorrent-torrents--state-label state)
                  (format "%3d%%" (round (* 100 progress)))
                  (qbittorrent-torrents--human-speed dlspeed)
                  (qbittorrent-torrents--human-speed upspeed)
                  (qbittorrent-torrents--human-eta eta)
                  (qbittorrent-torrents--human-size size)
                  name))))

(defun qbittorrent-torrents--sort-by-pct (a b)
  (< (string-to-number (aref (cadr a) 1))
     (string-to-number (aref (cadr b) 1))))

(defun qbittorrent-torrents--entry-for-hash (hash)
  (seq-find (lambda (e) (equal (alist-get 'hash e) hash))
            qbittorrent-torrents--entries))

(defun qbittorrent-torrents--sort-by-state (a b)
  "Sort column predicate: rank in-progress states above paused/seeding."
  (let* ((ea (qbittorrent-torrents--entry-for-hash (car a)))
         (eb (qbittorrent-torrents--entry-for-hash (car b)))
         (ra (qbittorrent-torrents--state-rank (alist-get 'state ea)))
         (rb (qbittorrent-torrents--state-rank (alist-get 'state eb))))
    (< ra rb)))

;;; ---------------------------------------------------------------------
;;; Major mode

(defvar qbittorrent-torrents-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "p" #'qbittorrent-torrents-pause-at-point)
    (define-key map "r" #'qbittorrent-torrents-resume-at-point)
    (define-key map "d" #'qbittorrent-torrents-delete-at-point)
    (define-key map "D" #'qbittorrent-torrents-delete-with-files-at-point)
    (define-key map "c" #'qbittorrent-torrents-recheck-at-point)
    (define-key map "w" #'qbittorrent-torrents-copy-hash)
    (define-key map "o" #'qbittorrent-torrents-open-webui)
    (define-key map "g" #'qbittorrent-torrents-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'qbittorrent-torrents-show-keys)
    map)
  "Keymap for `qbittorrent-torrents-mode'.")

(define-derived-mode qbittorrent-torrents-mode tabulated-list-mode "qBit"
  "Major mode for listing qBittorrent torrents."
  (setq tabulated-list-format
        `[("State" 5 ,#'qbittorrent-torrents--sort-by-state)
          ("%"     5 ,#'qbittorrent-torrents--sort-by-pct :right-align t)
          ("DL"    9 nil :right-align t)
          ("UL"    9 nil :right-align t)
          ("ETA"   8 nil :right-align t)
          ("Size"  9 nil :right-align t)
          ("Name"  0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("State" . nil))
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'qbittorrent-torrents--kill-timer nil t))

;;; ---------------------------------------------------------------------
;;; Auto-refresh timer

(defun qbittorrent-torrents--kill-timer ()
  (when qbittorrent-torrents--timer
    (cancel-timer qbittorrent-torrents--timer)
    (setq qbittorrent-torrents--timer nil)))

(defun qbittorrent-torrents--start-timer ()
  (qbittorrent-torrents--kill-timer)
  (when qbittorrent-torrents-refresh-interval
    (setq qbittorrent-torrents--timer
          (run-at-time qbittorrent-torrents-refresh-interval
                       qbittorrent-torrents-refresh-interval
                       #'qbittorrent-torrents--tick
                       (current-buffer)))))

(defun qbittorrent-torrents--tick (buffer)
  "Timer callback: refresh BUFFER unless the minibuffer is active."
  (when (and (buffer-live-p buffer)
             (not (active-minibuffer-window)))
    (with-current-buffer buffer
      (condition-case err
          (qbittorrent-torrents-refresh 'quiet)
        (error (message "qbittorrent-torrents refresh error: %s"
                        (error-message-string err)))))))

;;; ---------------------------------------------------------------------
;;; Action helpers

(defun qbittorrent-torrents--hash-at-point ()
  (or (tabulated-list-get-id)
      (user-error "No torrent at point")))

(defun qbittorrent-torrents--entry-at-point ()
  (let ((hash (qbittorrent-torrents--hash-at-point)))
    (or (seq-find (lambda (e) (equal (alist-get 'hash e) hash))
                  qbittorrent-torrents--entries)
        (user-error "Stale entry; press g to refresh"))))

;;; ---------------------------------------------------------------------
;;; Actions

(defun qbittorrent-torrents-pause-at-point ()
  "Pause the torrent at point."
  (interactive)
  (qbittorrent-webui-torrents-pause (qbittorrent-torrents--hash-at-point))
  (message "Paused")
  (qbittorrent-torrents-refresh 'quiet))

(defun qbittorrent-torrents-resume-at-point ()
  "Resume the torrent at point."
  (interactive)
  (qbittorrent-webui-torrents-resume (qbittorrent-torrents--hash-at-point))
  (message "Resumed")
  (qbittorrent-torrents-refresh 'quiet))

(defun qbittorrent-torrents-recheck-at-point ()
  "Force a hash-recheck on the torrent at point."
  (interactive)
  (qbittorrent-webui-torrents-recheck (qbittorrent-torrents--hash-at-point))
  (message "Rechecking")
  (qbittorrent-torrents-refresh 'quiet))

(defun qbittorrent-torrents--delete (with-files-p)
  (let* ((entry  (qbittorrent-torrents--entry-at-point))
         (hash   (alist-get 'hash entry))
         (name   (alist-get 'name entry))
         (prompt (format "Delete torrent %s%s? "
                         name
                         (if with-files-p " AND its data files" ""))))
    (when (or (not qbittorrent-torrents-confirm-delete)
              (yes-or-no-p prompt))
      (qbittorrent-webui-torrents-delete hash with-files-p)
      (message "Deleted %s" name)
      (qbittorrent-torrents-refresh 'quiet))))

(defun qbittorrent-torrents-delete-at-point ()
  "Delete the torrent at point, keeping its data files on disk."
  (interactive)
  (qbittorrent-torrents--delete nil))

(defun qbittorrent-torrents-delete-with-files-at-point ()
  "Delete the torrent at point AND its data files."
  (interactive)
  (qbittorrent-torrents--delete t))

(defun qbittorrent-torrents-copy-hash ()
  "Copy the hash of the torrent at point to the kill ring."
  (interactive)
  (let ((hash (qbittorrent-torrents--hash-at-point)))
    (kill-new hash)
    (message "Hash: %s" hash)))

(defun qbittorrent-torrents-open-webui ()
  "Open the qBittorrent WebUI in the default browser."
  (interactive)
  (browse-url (qbittorrent-webui--base-url)))

(defun qbittorrent-torrents-show-keys ()
  "Display a help buffer listing `qbittorrent-torrents-mode' keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*qbittorrent-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "qBittorrent Torrents Keybindings\n"
              "================================\n\n"
              "Actions on torrent at point:\n"
              "  p    Pause\n"
              "  r    Resume\n"
              "  c    Force hash recheck\n"
              "  d    Delete torrent (keep data files)\n"
              "  D    Delete torrent AND data files\n"
              "  w    Copy hash to kill ring\n\n"
              "View / navigation:\n"
              "  g    Refresh now\n"
              "  S    Sort by column at point (tabulated-list)\n"
              "  o    Open WebUI in browser\n"
              "  q    Quit window\n"
              "  ?    This help\n"))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*qbittorrent-keys*"))

;;; ---------------------------------------------------------------------
;;; Refresh

(defun qbittorrent-torrents-refresh (&optional quiet)
  "Re-fetch and re-render the torrents list.
When QUIET is non-nil, don't echo a message on success."
  (interactive)
  (let* ((torrents (qbittorrent-webui-torrents-info))
         (rows     (mapcar #'qbittorrent-torrents--entry-to-row torrents)))
    (setq qbittorrent-torrents--entries torrents)
    (setq tabulated-list-entries rows)
    ;; `t' keeps point on the same entry id across refresh.
    (tabulated-list-print t)
    (unless quiet
      (message "qbittorrent: %d torrent%s"
               (length torrents)
               (if (= 1 (length torrents)) "" "s")))))

;;; ---------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun qbittorrent-torrents ()
  "Show a live-updating list of qBittorrent torrents."
  (interactive)
  (let ((buf (get-buffer-create qbittorrent-torrents-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'qbittorrent-torrents-mode)
        (qbittorrent-torrents-mode))
      (qbittorrent-torrents-refresh 'quiet)
      (qbittorrent-torrents--start-timer))
    (pop-to-buffer buf)))

(provide 'qbittorrent-torrents)
;;; qbittorrent-torrents.el ends here
