;;; arr-queue.el --- Unified *arr queue viewer -*- lexical-binding: t; -*-

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

;; Tabulated-list UI that merges GET /queue from every service in
;; `arr-services' into a single buffer, with auto-refresh and action keys.

;;; Code:

(require 'tabulated-list)
(require 'subr-x)
(require 'seq)
(require 'arr)

(defgroup arr-queue nil
  "Unified queue viewer for *arr services."
  :group 'arr
  :prefix "arr-queue-")

(defcustom arr-queue-refresh-interval 5
  "Seconds between auto-refreshes of the queue buffer.  Nil disables."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'arr-queue)

(defcustom arr-queue-confirm-remove t
  "When non-nil, prompt before removing a queue entry."
  :type 'boolean
  :group 'arr-queue)

(defcustom arr-queue-buffer-name "*arr-queue*"
  "Buffer name used by `arr-queue'."
  :type 'string
  :group 'arr-queue)

(defcustom arr-queue-page-size 500
  "`pageSize' parameter passed to /queue requests."
  :type 'integer
  :group 'arr-queue)

(defvar-local arr-queue--timer nil
  "Per-buffer auto-refresh timer.")

(defvar-local arr-queue--entries nil
  "List of (SVC-NAME . QUEUE-ENTRY-ALIST) pairs for the current buffer.")

;;; ---------------------------------------------------------------------
;;; Formatting helpers

(defun arr-queue--human-size (bytes)
  (cond
   ((not (numberp bytes)) "?")
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fK" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fM" (/ bytes 1024.0 1024.0)))
   (t (format "%.2fG" (/ bytes 1024.0 1024.0 1024.0)))))

(defun arr-queue--parse-timeleft (s)
  "Parse *arr `timeleft' string into seconds.
Supports \"HH:MM:SS\" and \"D.HH:MM:SS\" forms.  Returns nil on failure."
  (when (and s (stringp s) (not (string-empty-p s)))
    (when (string-match
           "^\\(?:\\([0-9]+\\)\\.\\)?\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)"
           s)
      (+ (* 86400 (string-to-number (or (match-string 1 s) "0")))
         (* 3600  (string-to-number (match-string 2 s)))
         (* 60    (string-to-number (match-string 3 s)))
         (string-to-number (match-string 4 s))))))

(defun arr-queue--human-eta (seconds)
  (cond
   ((not (numberp seconds)) "?")
   ((< seconds 60)    (format "%ds" seconds))
   ((< seconds 3600)  (format "%dm" (/ seconds 60)))
   ((< seconds 86400) (format "%dh%02dm"
                              (/ seconds 3600)
                              (/ (mod seconds 3600) 60)))
   (t                 (format "%dd" (/ seconds 86400)))))

(defun arr-queue--status-label (status)
  (pcase status
    ("downloading"               "Down")
    ("queued"                    "Queu")
    ("paused"                    "Paus")
    ("completed"                 "Done")
    ("warning"                   "Warn")
    ("failed"                    "Fail")
    ("delay"                     "Dlay")
    ("downloadClientUnavailable" "NoCli")
    ("importPending"             "ImpP")
    ("importing"                 "Impt")
    (_ (if (and (stringp status) (> (length status) 0))
           (substring status 0 (min 5 (length status)))
         "?"))))

(defun arr-queue--service-label (name)
  (let ((svc (ignore-errors (arr--service-get name))))
    (or (and svc (plist-get svc :label))
        (symbol-name name))))

;;; ---------------------------------------------------------------------
;;; Row construction

(defun arr-queue--entry-to-row (pair)
  "PAIR is (SVC-NAME . QUEUE-ENTRY-ALIST).  Return a tabulated-list row."
  (let* ((svc-name (car pair))
         (entry    (cdr pair))
         (id       (alist-get 'id entry))
         (status   (alist-get 'status entry))
         (title    (or (alist-get 'title entry) ""))
         (size     (or (alist-get 'size entry) 0))
         (sizeleft (or (alist-get 'sizeleft entry) 0))
         (timeleft (alist-get 'timeleft entry))
         (pct      (if (and (numberp size) (> size 0))
                       (round (* 100 (/ (- size sizeleft) (float size))))
                     0))
         (eta      (arr-queue--parse-timeleft timeleft)))
    (list (cons svc-name id)
          (vector (arr-queue--service-label svc-name)
                  (arr-queue--status-label status)
                  (format "%3d%%" pct)
                  (arr-queue--human-size size)
                  (if eta (arr-queue--human-eta eta) "-")
                  title))))

(defun arr-queue--sort-by-pct (a b)
  (< (string-to-number (aref (cadr a) 2))
     (string-to-number (aref (cadr b) 2))))

;;; ---------------------------------------------------------------------
;;; Major mode

(defvar arr-queue-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'arr-queue-refresh)
    (define-key map "d" #'arr-queue-remove-at-point)
    (define-key map "D" #'arr-queue-remove-and-blocklist-at-point)
    (define-key map "o" #'arr-queue-open-webui-at-point)
    (define-key map "w" #'arr-queue-copy-title)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'arr-queue-show-keys)
    map)
  "Keymap for `arr-queue-mode'.")

(define-derived-mode arr-queue-mode tabulated-list-mode "arr-queue"
  "Major mode for the unified *arr queue viewer."
  (setq tabulated-list-format
        `[("Svc"    4 t)
          ("Status" 6 t)
          ("%"      5 ,#'arr-queue--sort-by-pct :right-align t)
          ("Size"   8 nil :right-align t)
          ("ETA"    8 nil :right-align t)
          ("Title"  0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Svc" . nil))
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'arr-queue--kill-timer nil t))

;;; ---------------------------------------------------------------------
;;; Auto-refresh

(defun arr-queue--kill-timer ()
  (when arr-queue--timer
    (cancel-timer arr-queue--timer)
    (setq arr-queue--timer nil)))

(defun arr-queue--start-timer ()
  (arr-queue--kill-timer)
  (when arr-queue-refresh-interval
    (setq arr-queue--timer
          (run-at-time arr-queue-refresh-interval
                       arr-queue-refresh-interval
                       #'arr-queue--tick
                       (current-buffer)))))

(defun arr-queue--tick (buffer)
  (when (and (buffer-live-p buffer)
             (not (active-minibuffer-window)))
    (with-current-buffer buffer
      (condition-case err
          (arr-queue-refresh 'quiet)
        (error (message "arr-queue refresh error: %s"
                        (error-message-string err)))))))

;;; ---------------------------------------------------------------------
;;; Action helpers

(defun arr-queue--id-at-point ()
  (or (tabulated-list-get-id)
      (user-error "No queue entry at point")))

(defun arr-queue--entry-at-point ()
  (let ((id (arr-queue--id-at-point)))
    (or (seq-find (lambda (pair)
                    (and (eq (car pair) (car id))
                         (equal (alist-get 'id (cdr pair)) (cdr id))))
                  arr-queue--entries)
        (user-error "Stale entry; press g to refresh"))))

;;; ---------------------------------------------------------------------
;;; Actions

(defun arr-queue--remove (blocklist-p)
  (let* ((pair   (arr-queue--entry-at-point))
         (svc    (car pair))
         (entry  (cdr pair))
         (id     (alist-get 'id entry))
         (title  (or (alist-get 'title entry) "(untitled)"))
         (prompt (format "Remove %s%s? "
                         title
                         (if blocklist-p " AND blocklist the release" ""))))
    (when (or (not arr-queue-confirm-remove)
              (yes-or-no-p prompt))
      (arr-delete svc (format "/queue/%s" id)
                  `(("removeFromClient" . "true")
                    ("blocklist"        . ,(if blocklist-p "true" "false"))))
      (message "Removed")
      (arr-queue-refresh 'quiet))))

(defun arr-queue-remove-at-point ()
  "Remove the queue entry at point (keep the series/movie in the library)."
  (interactive)
  (arr-queue--remove nil))

(defun arr-queue-remove-and-blocklist-at-point ()
  "Remove the queue entry at point AND blocklist its release."
  (interactive)
  (arr-queue--remove t))

(defun arr-queue-copy-title ()
  "Copy the title of the queue entry at point to the kill ring."
  (interactive)
  (let ((title (alist-get 'title (cdr (arr-queue--entry-at-point)))))
    (kill-new title)
    (message "Title: %s" title)))

(defun arr-queue-open-webui-at-point ()
  "Open the *arr service's web UI for the queue entry at point."
  (interactive)
  (let ((svc (car (arr-queue--entry-at-point))))
    (browse-url (arr--base-url svc))))

(defun arr-queue-show-keys ()
  "Display a help buffer listing `arr-queue-mode' keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*arr-queue-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "arr-queue Keybindings\n"
              "=====================\n\n"
              "Actions on queue entry at point:\n"
              "  d    Remove from queue (keep in library)\n"
              "  D    Remove AND blocklist the release\n"
              "  w    Copy title to kill ring\n\n"
              "View / navigation:\n"
              "  g    Refresh now\n"
              "  S    Sort by column at point (tabulated-list)\n"
              "  o    Open the service's web UI\n"
              "  q    Quit window\n"
              "  ?    This help\n"))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*arr-queue-keys*"))

;;; ---------------------------------------------------------------------
;;; Refresh

(defun arr-queue--fetch-one (svc-name)
  "Return a list of (SVC-NAME . QUEUE-RECORD) from /queue for SVC-NAME.
Errors are caught and messaged; the result is an empty list on failure."
  (condition-case err
      (let* ((resp (arr-get svc-name
                            (format "/queue?pageSize=%d"
                                    arr-queue-page-size)))
             (records (or (alist-get 'records resp) '())))
        (mapcar (lambda (r) (cons svc-name r)) records))
    (error
     (message "arr-queue: %s failed: %s"
              svc-name (error-message-string err))
     nil)))

(defun arr-queue-refresh (&optional quiet)
  "Re-fetch /queue from every configured service and re-render."
  (interactive)
  (let* ((pairs (mapcan #'arr-queue--fetch-one
                        (arr-configured-services)))
         (rows  (mapcar #'arr-queue--entry-to-row pairs)))
    (setq arr-queue--entries pairs)
    (setq tabulated-list-entries rows)
    (tabulated-list-print t)
    (unless quiet
      (message "arr-queue: %d entr%s"
               (length pairs)
               (if (= 1 (length pairs)) "y" "ies")))))

;;; ---------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun arr-queue ()
  "Open a live-updating unified queue view for all configured *arr services."
  (interactive)
  (let ((buf (get-buffer-create arr-queue-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'arr-queue-mode)
        (arr-queue-mode))
      (arr-queue-refresh 'quiet)
      (arr-queue--start-timer))
    (pop-to-buffer buf)))

(provide 'arr-queue)
;;; arr-queue.el ends here
