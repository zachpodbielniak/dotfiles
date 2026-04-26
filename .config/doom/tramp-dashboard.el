;;; tramp-dashboard.el --- Org-style dashboard for TRAMP -*- lexical-binding: t; -*-

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

;; M-x tramp-dashboard pops an org buffer listing every reachable TRAMP
;; target across providers: active connections, SSH hosts, podman
;; containers, distrobox containers, running flatpak instances, ADB
;; devices, gvfs-mounted MTP devices, and pinned paths.  Each row shows
;; inline links: open dired (`/' and `~'), spawn an eshell, pin/unpin,
;; or disconnect.  Bound to SPC o D.
;;
;; Extending: write a `tramp-dashboard--enum-FOO' that returns a list
;; of (TRAMP-PATH . LABEL) cons cells, register it in
;; `tramp-dashboard-provider-alist', and append the symbol to
;; `tramp-dashboard-providers'.  Buffer rendering needs no changes.

;;; Code:

(require 'tramp)
(require 'tramp-cache)
(require 'subr-x)
(require 'cl-lib)

(declare-function eshell                        "eshell"     (&optional arg))
(declare-function tramp-parse-sconfig           "tramp"      (filename))
(declare-function tramp-cleanup-connection      "tramp-cmds" (vec &optional keep-debug keep-password))
(declare-function tramp-cleanup-all-connections "tramp-cmds" ())
(declare-function evil-define-key*              "evil-core"  (state keymap key def &rest bindings))
(declare-function org-set-startup-visibility    "org"        ())

;;; ----------------------------------------------------------------------
;;; Customization

(defgroup tramp-dashboard nil
  "Org-style dashboard for TRAMP targets."
  :group 'tramp
  :prefix "tramp-dashboard-")

(defcustom tramp-dashboard-providers
  '(active ssh podman distrobox flatpak adb mtp pinned)
  "Ordered list of provider symbols rendered in the dashboard.
Each must be a key in `tramp-dashboard-provider-alist'."
  :type '(repeat symbol)
  :group 'tramp-dashboard)

(defcustom tramp-dashboard-pinned nil
  "Persisted list of pinned TRAMP destinations.
Each entry is (PATH . LABEL) where PATH is a TRAMP file string and
LABEL is the displayed name."
  :type '(repeat (cons string string))
  :group 'tramp-dashboard)

(defcustom tramp-dashboard-podman-include-stopped nil
  "When non-nil, include stopped containers in the Podman section."
  :type 'boolean
  :group 'tramp-dashboard)

(defcustom tramp-dashboard-ssh-config-path "~/.ssh/config"
  "Path to the SSH config file to parse for the SSH section."
  :type 'string
  :group 'tramp-dashboard)

(defcustom tramp-dashboard-enumerate-timeout 3
  "Per-enumerator timeout in seconds.  Hung commands are skipped."
  :type 'number
  :group 'tramp-dashboard)

(defcustom tramp-dashboard-auto-refresh t
  "When non-nil, re-enumerate every section each time the buffer is shown."
  :type 'boolean
  :group 'tramp-dashboard)

(defcustom tramp-dashboard-buffer-name "*TRAMP Dashboard*"
  "Name of the dashboard buffer."
  :type 'string
  :group 'tramp-dashboard)

;;; ----------------------------------------------------------------------
;;; Provider registry

(defvar tramp-dashboard-provider-alist
  '((active    :label "Active connections"
               :enumerate tramp-dashboard--enum-active)
    (ssh       :label "SSH hosts"
               :enumerate tramp-dashboard--enum-ssh)
    (podman    :label "Podman containers"
               :enumerate tramp-dashboard--enum-podman
               :requires  "podman")
    (distrobox :label "Distrobox containers"
               :enumerate tramp-dashboard--enum-distrobox
               :requires  "distrobox")
    (flatpak   :label "Flatpak (running)"
               :enumerate tramp-dashboard--enum-flatpak
               :requires  "flatpak")
    (adb       :label "ADB devices"
               :enumerate tramp-dashboard--enum-adb
               :requires  "adb")
    (mtp       :label "MTP devices"
               :enumerate tramp-dashboard--enum-mtp
               :requires  "gio")
    (pinned    :label "Pinned"
               :enumerate tramp-dashboard--enum-pinned))
  "Alist mapping provider symbols to plists.
Plist keys: :label STRING, :enumerate FUNCTION-SYMBOL,
:requires BINARY-NAME-OR-NIL.

The :enumerate function takes no arguments and returns a list of
(TRAMP-PATH . LABEL) cons cells.  Path is a fully-formed TRAMP file
string ending in `:'; label is the human-readable display.

To add a new provider, write a `:enumerate' function and push a new
entry here, then add the symbol to `tramp-dashboard-providers'.")

;;; ----------------------------------------------------------------------
;;; Shell helper

(defun tramp-dashboard--lines (program &rest args)
  "Run PROGRAM with ARGS, return list of stdout lines (or nil).
Errors and timeouts are swallowed; bounded by
`tramp-dashboard-enumerate-timeout'."
  (with-timeout (tramp-dashboard-enumerate-timeout nil)
    (condition-case nil
        (apply #'process-lines program args)
      (error nil))))

;;; ----------------------------------------------------------------------
;;; Enumerators

(defun tramp-dashboard--enum-active ()
  "List currently-connected TRAMP destinations from open buffers."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (buf (buffer-list))
      (let ((dir (buffer-local-value 'default-directory buf)))
        (when (and dir (tramp-tramp-file-p dir))
          (let* ((vec (tramp-dissect-file-name dir))
                 (method (tramp-file-name-method vec))
                 (host (tramp-file-name-host vec))
                 (key (format "/%s:%s:" method host)))
            (puthash key (1+ (gethash key seen 0)) seen)))))
    (let (entries)
      (maphash (lambda (path count)
                 (push (cons path
                             (format "%s — %d buffer%s"
                                     path count (if (= count 1) "" "s")))
                       entries))
               seen)
      (sort entries (lambda (a b) (string< (car a) (car b)))))))

(defun tramp-dashboard--enum-ssh ()
  "Parse `tramp-dashboard-ssh-config-path' for non-wildcard Host entries."
  (let* ((cfg (expand-file-name tramp-dashboard-ssh-config-path))
         (raw (when (file-readable-p cfg)
                (tramp-parse-sconfig cfg))))
    (cl-loop for entry in raw
             for host = (cadr entry)
             when (and host
                       (not (string-empty-p host))
                       (not (string-match-p "[*?]" host)))
             collect (cons (format "/ssh:%s:" host) host))))

(defun tramp-dashboard--enum-podman ()
  "List podman containers via `podman ps'."
  (let* ((args (append '("ps" "--format" "{{.ID}}|{{.Names}}|{{.Status}}")
                       (when tramp-dashboard-podman-include-stopped '("-a"))))
         (lines (apply #'tramp-dashboard--lines "podman" args)))
    (cl-loop for line in lines
             for parts = (split-string line "|")
             when (= (length parts) 3)
             collect (let ((id (nth 0 parts))
                           (name (nth 1 parts))
                           (status (nth 2 parts)))
                       (cons (format "/podman:%s:" name)
                             (format "%s (%s) — %s"
                                     name
                                     (substring id 0 (min 12 (length id)))
                                     status))))))

(defun tramp-dashboard--enum-distrobox ()
  "List distrobox containers via `distrobox list --no-color'."
  (let ((lines (tramp-dashboard--lines "distrobox" "list" "--no-color")))
    ;; First line is the header (`ID | NAME | STATUS | IMAGE'); skip it.
    (cl-loop for line in (cdr lines)
             for parts = (mapcar #'string-trim (split-string line "|"))
             when (and (>= (length parts) 4)
                       (not (string-empty-p (nth 1 parts))))
             collect (let ((name (nth 1 parts))
                           (status (nth 2 parts)))
                       (cons (format "/distrobox:%s:" name)
                             (format "%s — %s" name status))))))

(defun tramp-dashboard--enum-flatpak ()
  "List running flatpak instances via `flatpak ps'.
Output is two TAB-separated columns: instance ID, application ID."
  (let ((lines (tramp-dashboard--lines
                "flatpak" "ps" "--columns=instance,application")))
    (cl-loop for line in lines
             for parts = (split-string line "\t")
             when (= (length parts) 2)
             collect (let ((instance (nth 0 parts))
                           (app (nth 1 parts)))
                       (cons (format "/flatpak:%s:" instance)
                             (format "%s (%s)" app instance))))))

(defun tramp-dashboard--enum-adb ()
  "List connected ADB devices via `adb devices'.
Drops the `List of devices attached' header and any non-`device' lines."
  (let ((lines (cdr (tramp-dashboard--lines "adb" "devices"))))
    (cl-loop for line in lines
             for parts = (split-string line "\t")
             when (and (= (length parts) 2)
                       (string= (nth 1 parts) "device"))
             collect (let ((serial (nth 0 parts)))
                       (cons (format "/adb:%s:" serial) serial)))))

(defun tramp-dashboard--enum-mtp ()
  "List gvfs-mounted MTP devices by parsing `gio mount -li'."
  (let ((lines (tramp-dashboard--lines "gio" "mount" "-li"))
        entries)
    (dolist (line lines)
      (when (string-match
             "Mount(\\([0-9]+\\)): \\(.+?\\) -> mtp://\\([^/]+\\)/?"
             line)
        (let ((label (match-string 2 line))
              (host  (match-string 3 line)))
          (push (cons (format "/mtp:%s:" host)
                      (format "%s [%s]" label host))
                entries))))
    (nreverse entries)))

(defun tramp-dashboard--enum-pinned ()
  "Return user's pinned destinations from `tramp-dashboard-pinned'."
  (mapcar (lambda (cell)
            (let ((path (car cell)) (label (cdr cell)))
              (cons path (format "%s (%s)" label path))))
          tramp-dashboard-pinned))

;;; ----------------------------------------------------------------------
;;; Rendering

(defun tramp-dashboard--quote-arg (s)
  "Render S as an elisp string literal, safe for embedding in `[[elisp:…]]'."
  (prin1-to-string s))

(defun tramp-dashboard--link (form label)
  "Build an `[[elisp:FORM][LABEL]]' org link string."
  (format "[[elisp:%s][%s]]" form label))

(defun tramp-dashboard--row-actions-default (path)
  "Inline action links for a non-active, non-pinned PATH."
  (string-join
   (list
    (tramp-dashboard--link
     (format "(tramp-dashboard-open %s)"
             (tramp-dashboard--quote-arg path))
     "/")
    (tramp-dashboard--link
     (format "(tramp-dashboard-open %s)"
             (tramp-dashboard--quote-arg (concat path "~")))
     "~")
    (tramp-dashboard--link
     (format "(tramp-dashboard-eshell %s)"
             (tramp-dashboard--quote-arg path))
     "eshell")
    (tramp-dashboard--link
     (format "(tramp-dashboard-pin %s)"
             (tramp-dashboard--quote-arg path))
     "pin"))
   " · "))

(defun tramp-dashboard--row-actions-active (path)
  "Inline action links for an active-connection PATH."
  (string-join
   (list
    (tramp-dashboard--link
     (format "(tramp-dashboard-open %s)"
             (tramp-dashboard--quote-arg path))
     "open")
    (tramp-dashboard--link
     (format "(tramp-dashboard-eshell %s)"
             (tramp-dashboard--quote-arg path))
     "eshell")
    (tramp-dashboard--link
     (format "(tramp-dashboard-disconnect %s)"
             (tramp-dashboard--quote-arg path))
     "disconnect"))
   " · "))

(defun tramp-dashboard--row-actions-pinned (path)
  "Inline action links for a pinned PATH."
  (string-join
   (list
    (tramp-dashboard--link
     (format "(tramp-dashboard-open %s)"
             (tramp-dashboard--quote-arg path))
     "open")
    (tramp-dashboard--link
     (format "(tramp-dashboard-eshell %s)"
             (tramp-dashboard--quote-arg path))
     "eshell")
    (tramp-dashboard--link
     (format "(tramp-dashboard-unpin %s)"
             (tramp-dashboard--quote-arg path))
     "unpin"))
   " · "))

(defun tramp-dashboard--render-section (provider plist)
  "Render the section for PROVIDER (symbol) given its PLIST as a string."
  (let* ((label     (plist-get plist :label))
         (req       (plist-get plist :requires))
         (enum-fn   (plist-get plist :enumerate))
         (action-fn (cond ((eq provider 'active) #'tramp-dashboard--row-actions-active)
                          ((eq provider 'pinned) #'tramp-dashboard--row-actions-pinned)
                          (t                     #'tramp-dashboard--row-actions-default)))
         (entries   (cond
                     ((and req (not (executable-find req))) :missing)
                     ((not (fboundp enum-fn))               :no-fn)
                     (t (or (funcall enum-fn) :empty)))))
    (with-temp-buffer
      (cond
       ((eq entries :missing)
        (insert (format "* %s\n- (%s not installed)\n\n" label req)))
       ((eq entries :no-fn)
        (insert (format "* %s\n- (enumerator %s undefined)\n\n" label enum-fn)))
       ((eq entries :empty)
        (insert (format "* %s\n- (none)\n\n" label)))
       (t
        (insert (format "* %s (%d)\n" label (length entries)))
        (dolist (cell entries)
          (let ((path  (car cell))
                (label (cdr cell)))
            (insert "- " label " — " (funcall action-fn path) "\n")))
        (insert "\n")))
      (buffer-string))))

(defun tramp-dashboard--render ()
  "Return the full dashboard buffer text."
  (concat
   "#+title: TRAMP Dashboard\n"
   "#+startup: showall indent\n\n"
   (string-join
    (list
     (tramp-dashboard--link "(tramp-dashboard-refresh)"     "↻ refresh")
     (tramp-dashboard--link "(tramp-dashboard-cleanup-all)" "⚠ cleanup all")
     (tramp-dashboard--link "(tramp-dashboard-multi-hop)"   "→ multi-hop")
     (tramp-dashboard--link "(tramp-dashboard-add-pin)"     "+ pin"))
    " · ")
   "\n\n"
   (mapconcat
    (lambda (provider)
      (let ((plist (cdr (assq provider tramp-dashboard-provider-alist))))
        (if plist
            (tramp-dashboard--render-section provider plist)
          (format "* (unknown provider %s)\n\n" provider))))
    tramp-dashboard-providers
    "")))

(defun tramp-dashboard--repaint ()
  "Erase and re-render the dashboard buffer in-place.
Caller must already be inside the dashboard buffer."
  (let ((inhibit-read-only t)
        (pt (point)))
    (erase-buffer)
    (insert (tramp-dashboard--render))
    (goto-char (min pt (point-max)))
    (when (derived-mode-p 'org-mode)
      (org-set-startup-visibility))))

;;; ----------------------------------------------------------------------
;;; Actions (used by the inline org links)

;;;###autoload
(defun tramp-dashboard-open (path)
  "Open dired at PATH."
  (interactive "sTRAMP path: ")
  (dired path))

;;;###autoload
(defun tramp-dashboard-eshell (path)
  "Spawn eshell with `default-directory' set to PATH."
  (interactive "sTRAMP path: ")
  (let ((default-directory (file-name-as-directory path)))
    (eshell)))

(defun tramp-dashboard--save-pinned ()
  "Persist `tramp-dashboard-pinned' via Customize."
  (customize-save-variable 'tramp-dashboard-pinned tramp-dashboard-pinned))

(defun tramp-dashboard--maybe-repaint ()
  "If the dashboard buffer is live, repaint it."
  (let ((buf (get-buffer tramp-dashboard-buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (tramp-dashboard--repaint)))))

(defun tramp-dashboard-pin (path &optional label)
  "Add PATH (with optional LABEL) to `tramp-dashboard-pinned' and save."
  (interactive "sTRAMP path: ")
  (let ((lbl (or label
                 (read-string (format "Label for %s: " path) path))))
    (cl-pushnew (cons path lbl) tramp-dashboard-pinned
                :test (lambda (a b) (string= (car a) (car b))))
    (tramp-dashboard--save-pinned)
    (tramp-dashboard--maybe-repaint)
    (message "Pinned: %s" path)))

(defun tramp-dashboard-unpin (path)
  "Remove PATH from `tramp-dashboard-pinned' and save."
  (interactive "sTRAMP path: ")
  (setq tramp-dashboard-pinned
        (cl-remove-if (lambda (cell) (string= (car cell) path))
                      tramp-dashboard-pinned))
  (tramp-dashboard--save-pinned)
  (tramp-dashboard--maybe-repaint)
  (message "Unpinned: %s" path))

(defun tramp-dashboard-add-pin ()
  "Prompt for a TRAMP path and label, then pin."
  (interactive)
  (let* ((path  (read-string "TRAMP path to pin (e.g. /ssh:host:): "))
         (label (read-string (format "Label for %s: " path) path)))
    (tramp-dashboard-pin path label)))

(defun tramp-dashboard-disconnect (path)
  "Drop TRAMP connection cache for PATH."
  (interactive "sTRAMP path: ")
  (let ((vec (tramp-dissect-file-name path)))
    (tramp-cleanup-connection vec))
  (tramp-dashboard--maybe-repaint)
  (message "Disconnected: %s" path))

(defun tramp-dashboard-cleanup-all ()
  "Drop every TRAMP connection (with confirmation)."
  (interactive)
  (when (yes-or-no-p "Drop all TRAMP connections? ")
    (tramp-cleanup-all-connections)
    (tramp-dashboard--maybe-repaint)
    (message "All TRAMP connections cleared.")))

(defun tramp-dashboard-refresh ()
  "Re-render the dashboard."
  (interactive)
  (tramp-dashboard--maybe-repaint)
  (message "Dashboard refreshed."))

(defun tramp-dashboard-multi-hop ()
  "Build a multi-hop TRAMP path interactively, then open dired."
  (interactive)
  (let ((hops (list (read-string "First hop (e.g. ssh:bastion): ")))
        hop)
    (while (not (string-empty-p
                 (setq hop (read-string "Next hop (empty to finish): " ""))))
      (push hop hops))
    (let* ((joined (mapconcat #'identity (nreverse hops) "|"))
           (default-path (format "/%s:/" joined))
           (final (read-string "Path: " default-path)))
      (dired final))))

;;; ----------------------------------------------------------------------
;;; At-point helpers

(defun tramp-dashboard--path-at-point ()
  "Find the first TRAMP path on the current line, or nil."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-match
           "\\(/[A-Za-z0-9_-]+:[^:[:space:]\"\n|]+:[^[:space:]\"\n]*\\)"
           line)
      (match-string 1 line))))

(defun tramp-dashboard-toggle-pin-at-point ()
  "Pin or unpin the entry on the current line."
  (interactive)
  (let ((path (tramp-dashboard--path-at-point)))
    (cond
     ((not path) (message "No TRAMP path on this line"))
     ((cl-find path tramp-dashboard-pinned
               :test (lambda (a b) (string= a (car b))))
      (tramp-dashboard-unpin path))
     (t (tramp-dashboard-pin path)))))

(defun tramp-dashboard-disconnect-at-point ()
  "Disconnect the entry on the current line."
  (interactive)
  (let ((path (tramp-dashboard--path-at-point)))
    (if path (tramp-dashboard-disconnect path)
      (message "No TRAMP path on this line"))))

(defun tramp-dashboard-show-keys ()
  "Display the dashboard keybinding cheatsheet."
  (interactive)
  (with-current-buffer (get-buffer-create "*tramp-dashboard-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert
       "TRAMP Dashboard keys (Evil normal state)\n"
       "========================================\n\n"
       "  RET    open link / action under point\n"
       "  g r    refresh dashboard\n"
       "  g p    toggle pin/unpin entry on this line\n"
       "  g d    disconnect entry on this line\n"
       "  g h    multi-hop builder\n"
       "  q      quit window\n"
       "  ?      this help\n")
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer)
                    '(display-buffer-at-bottom
                      . ((window-height . fit-window-to-buffer))))))

;;; ----------------------------------------------------------------------
;;; Mode

(defvar tramp-dashboard-mode-map (make-sparse-keymap)
  "Keymap for `tramp-dashboard-mode'.")

(define-derived-mode tramp-dashboard-mode org-mode "TRAMP-Dash"
  "Major mode for the TRAMP dashboard buffer."
  (setq-local truncate-lines t)
  ;; Skip the y/n confirmation org-mode normally fires before running an
  ;; `[[elisp:…]]' link.  Safe here because every link in this buffer is
  ;; produced by our own renderer.
  (setq-local org-link-elisp-confirm-function nil)
  (read-only-mode 1))

(with-eval-after-load 'evil
  (evil-define-key* 'normal tramp-dashboard-mode-map
    (kbd "g r") #'tramp-dashboard-refresh
    (kbd "g p") #'tramp-dashboard-toggle-pin-at-point
    (kbd "g d") #'tramp-dashboard-disconnect-at-point
    (kbd "g h") #'tramp-dashboard-multi-hop
    (kbd "q")   #'quit-window
    (kbd "?")   #'tramp-dashboard-show-keys))

;;; ----------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun tramp-dashboard ()
  "Open the TRAMP dashboard buffer."
  (interactive)
  (let ((buf (get-buffer-create tramp-dashboard-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'tramp-dashboard-mode)
        (tramp-dashboard-mode))
      (when (or (= (buffer-size) 0) tramp-dashboard-auto-refresh)
        (tramp-dashboard--repaint)))
    (let ((display-buffer-overriding-action
           '(display-buffer-same-window)))
      (pop-to-buffer buf))))

(provide 'tramp-dashboard)
;;; tramp-dashboard.el ends here
