;;; cmacs.el --- CMacs-only features: RTSP/RTSPS camera dashboard -*- lexical-binding: t; -*-

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
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is loaded only when running on cmacs — Doom's `config.el'
;; guards `(load! "cmacs")' with `IS-CMACS', which is set from
;; `(cmacs-feature-p 'video)'.  Loading under vanilla Emacs is a no-op.
;;
;; Provides `cmacs-cams-dashboard' — a live-generated `org-mode' buffer
;; containing one `#+BEGIN_VIDEO' block per feed configured in
;; `cmacs-cams-feeds'.  Feeds are RTSP/RTSPS streams (NVR, doorbell,
;; etc.).  Because the buffer is in `org-mode' you get TAB cycling,
;; headline navigation, folding, and search for free.
;;
;; Feeds are configured below via `setq cmacs-cams-feeds' (just after
;; the defcustom).  Example shape:
;;
;;   (setq cmacs-cams-feeds
;;         '((:name "front-door"
;;            :uri  "rtsps://nvr.lan:7441/AbC"
;;            :insecure t)
;;           (:name "garage"
;;            :uri  "rtsp://10.0.0.50:554/stream1"
;;            :width 480 :height 270 :latency 400)))
;;
;; Entry point: `M-x cmacs-cams-dashboard' (also `SPC o v').
;;
;; Inside the dashboard (`org-mode'), the localleader prefix `SPC m c'
;; binds:
;;   p / s / r — play / stop / restart the feed under point
;;   c         — snapshot the feed under point to `cmacs-cams-snapshot-dir'
;;   P / S     — play / stop the entire wall
;;   g         — refresh (rebuild from `cmacs-cams-feeds')

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'org)

;; Forward declarations — these come from cmacs's built-in lisp.  We
;; require `cmacs-video-org' explicitly below, but byte-compilation
;; under vanilla emacs (or a build without the video feature) would
;; otherwise complain about free variables / undefined functions.
(declare-function cmacs-video-open               "cmacs-video"     (uri &rest plist))
(declare-function cmacs-video-close              "cmacs-video"     (handle))
(declare-function cmacs-video-snapshot-to-file   "cmacs-video"     (handle path))
(declare-function cmacs-video-org-render-buffer  "cmacs-video-org" ())
(declare-function cmacs-video-org-unrender-buffer "cmacs-video-org" ())
(declare-function cmacs-video-org--block-at-point "cmacs-video-org" ())
(declare-function cmacs-video-org--make-overlay  "cmacs-video-org" (blk))
(declare-function cmacs-video-org--block-begin-bol "cmacs-video-org" (blk))
(declare-function cmacs-video-org--block-end-bol   "cmacs-video-org" (blk))
(declare-function cmacs-video-org--block-body-begin "cmacs-video-org" (blk))
(declare-function cmacs-video-org--block-body-end  "cmacs-video-org" (blk))
(declare-function cmacs-video-org--block-src      "cmacs-video-org" (blk))
(defvar cmacs-video-default-width)
(defvar cmacs-video-default-height)
(defvar cmacs-video-default-latency-ms)

;; Pull in the cmacs video org integration.  cmacs.el is only loaded
;; when `IS-CMACS' is non-nil (see config.el), so the require is safe.
(require 'cmacs-video)
(require 'cmacs-video-org)

(defgroup cmacs-cams nil
  "RTSP/RTSPS camera-feed dashboard backed by `cmacs-video'."
  :group 'applications
  :prefix "cmacs-cams-")

(defcustom cmacs-cams-feeds nil
  "List of camera-feed plists rendered by `cmacs-cams-dashboard'.
Each entry is a plist of the form:

  (:name STRING :uri STRING
   [:width N] [:height N] [:latency N] [:insecure BOOL]
   [:notes STRING])

`:width', `:height', and `:latency' fall back to
`cmacs-video-default-width', `cmacs-video-default-height', and
`cmacs-video-default-latency-ms' respectively when omitted.  `:notes'
is a free-form caption inserted as plain text inside the org subtree."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'cmacs-cams)

;;; ------------------------------------------------------ configured feeds
;;
;; UniFi Protect NVR at 10.0.0.1:7441 — RTSPS streams.  :insecure t skips
;; TLS validation (NVR uses a self-signed cert on the local LAN).
(setq cmacs-cams-feeds
      '((:name "bullet-driveway"
         :uri  "rtsps://10.0.0.1:7441/Gm8LdzdVBouuawmy"
         :insecure t)
        (:name "bullet-frontyard"
         :uri  "rtsps://10.0.0.1:7441/BwjDqph50zITBBkd"
         :insecure t)
        (:name "bullet-garage"
         :uri  "rtsps://10.0.0.1:7441/Fsr6Nyd5HONv6GLb"
         :insecure t)
        (:name "garage-rear"
         :uri  "rtsps://10.0.0.1:7441/9FWkt7N1B8P5uN7U"
         :insecure t)
        (:name "bullet-shed"
         :uri  "rtsps://10.0.0.1:7441/dwMqzkK29A5xwOdS"
         :insecure t)
        (:name "doorbell-frontdoor"
         :uri  "rtsps://10.0.0.1:7441/LwCOnMwfokHtwgKS"
         :insecure t)
        (:name "doorbell-frontdoor-package"
         :uri  "rtsps://10.0.0.1:7441/yFsgwIfFHyJQ388C"
         :insecure t)))

(defcustom cmacs-cams-buffer-name "*Cams*"
  "Name of the buffer that hosts the dashboard."
  :type 'string
  :group 'cmacs-cams)

(defcustom cmacs-cams-snapshot-dir
  (expand-file-name "~/Pictures/cams/")
  "Directory where `cmacs-cams-snapshot-at-point' writes PNG frames."
  :type 'directory
  :group 'cmacs-cams)

;;; ------------------------------------------------------ faces (catppuccin-mocha)

(defface cmacs-cams-title-face
  '((t (:foreground "#cba6f7" :weight bold :height 1.2)))
  "Face for the dashboard's top-level title."
  :group 'cmacs-cams)

(defface cmacs-cams-feed-face
  '((t (:foreground "#f9e2af" :weight bold)))
  "Face for individual feed-name headlines."
  :group 'cmacs-cams)

(defface cmacs-cams-meta-face
  '((t (:foreground "#6c7086" :slant italic)))
  "Face for dimmed metadata (URI, notes) in the dashboard."
  :group 'cmacs-cams)

;;; ------------------------------------------------------ helpers

(defun cmacs-cams--sanitize (s)
  "Sanitize S for use in a filename: lowercase, alnum + `-' only."
  (let ((trimmed (string-trim (or s ""))))
    (replace-regexp-in-string
     "[^a-z0-9-]+" "-"
     (downcase trimmed))))

(defun cmacs-cams--feed-block (feed)
  "Return the org-block text for a single FEED plist."
  (let* ((name     (or (plist-get feed :name) "Unnamed"))
         (uri      (plist-get feed :uri))
         (width    (or (plist-get feed :width)   cmacs-video-default-width))
         (height   (or (plist-get feed :height)  cmacs-video-default-height))
         (latency  (or (plist-get feed :latency) cmacs-video-default-latency-ms))
         (insecure (plist-get feed :insecure))
         (notes    (plist-get feed :notes))
         (kw (format ":src \"%s\" :width %d :height %d :latency %d%s"
                     uri width height latency
                     (if insecure " :insecure t" ""))))
    (concat
     (format "** %s :cam:\n" name)
     (when notes (format "%s\n" notes))
     (format "#+BEGIN_VIDEO %s\n" kw)
     (format "%s\n" name)
     "#+END_VIDEO\n\n")))

(defun cmacs-cams--apply-faces ()
  "Highlight the dashboard title and feed headlines with custom faces."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Camera Feeds.*$" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'face 'cmacs-cams-title-face)
        (overlay-put ov 'cmacs-cams t)))
    (while (re-search-forward "^\\*\\* .*$" nil t)
      (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov 'face 'cmacs-cams-feed-face)
        (overlay-put ov 'cmacs-cams t)))))

(defun cmacs-cams--overlay-at-point ()
  "Return the cmacs-video overlay for the block at point, or nil.
Cmacs places its overlay over the block *body* only, so this
locates the enclosing `#+BEGIN_VIDEO' first and then searches that
range — letting the user invoke commands from anywhere inside the
block, including the header line."
  (when-let* ((blk (cmacs-video-org--block-at-point)))
    (let ((bb (cmacs-video-org--block-body-begin blk))
          (be (cmacs-video-org--block-body-end blk)))
      (cl-find-if (lambda (ov) (overlay-get ov 'cmacs-video))
                  (overlays-in bb be)))))

(defun cmacs-cams--handle-at-point ()
  "Return the cmacs-video handle for the block at point, or nil."
  (when-let* ((ov (cmacs-cams--overlay-at-point)))
    (overlay-get ov 'cmacs-video-handle)))

;;; ------------------------------------------------------ dashboard

;;;###autoload
(defun cmacs-cams-dashboard ()
  "Build (or rebuild) the `*Cams*' dashboard from `cmacs-cams-feeds'.
The buffer is in `org-mode', so TAB cycling, headline navigation,
and folding all work normally.  Video blocks are rendered as live
GStreamer overlays via `cmacs-video-org-render-buffer'."
  (interactive)
  (unless (display-graphic-p)
    (user-error "cmacs-cams-dashboard requires a graphical frame"))
  (let ((buf (get-buffer-create cmacs-cams-buffer-name)))
    (with-current-buffer buf
      ;; Tear down any prior overlays before we erase the buffer.
      (when (derived-mode-p 'org-mode)
        (ignore-errors (cmacs-video-org-unrender-buffer)))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "#+TITLE: Camera Feeds\n"
                "#+STARTUP: showall\n"
                "#+OPTIONS: toc:nil num:nil\n\n"
                "* Camera Feeds\n")
        (if (null cmacs-cams-feeds)
            (insert "\nNo feeds configured.  Set `cmacs-cams-feeds' in config.el.\n")
          (dolist (feed cmacs-cams-feeds)
            (insert (cmacs-cams--feed-block feed)))))
      ;; org-mode activation runs `org-install-agenda-files-menu', which
      ;; iterates `org-agenda-files' and calls `expand-file-name' on each.
      ;; In environments with a pathological regex in
      ;; `file-name-handler-alist' (e.g. some TRAMP setups) this can hang.
      ;; The dashboard buffer doesn't need agenda integration, so suppress
      ;; both during activation.
      (let ((org-agenda-files nil)
            (file-name-handler-alist nil))
        (org-mode))
      (cmacs-cams--apply-faces)
      ;; The buffer isn't file-backed, so `find-file-hook' (which is
      ;; what `cmacs-video-org--maybe-render' uses) never fires.
      ;; Trigger the render ourselves.
      (cmacs-video-org-render-buffer)
      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defalias 'cmacs-cams-refresh #'cmacs-cams-dashboard
  "Alias: rebuild the dashboard from the current `cmacs-cams-feeds'.")

;;; ------------------------------------------------------ per-feed actions

(defun cmacs-cams-stop-at-point ()
  "Stop the camera feed whose block contains point.
Closes the underlying stream and removes its display overlay."
  (interactive)
  (if-let* ((ov (cmacs-cams--overlay-at-point)))
      (let ((handle (overlay-get ov 'cmacs-video-handle)))
        (when handle (ignore-errors (cmacs-video-close handle)))
        (delete-overlay ov)
        (message "cmacs-cams: stopped feed at point"))
    (user-error "No active camera feed at point")))

(defun cmacs-cams-play-at-point ()
  "Start (or restart) the camera feed whose `#+BEGIN_VIDEO' block contains point.
If a feed is already rendered here, it is torn down first."
  (interactive)
  (when-let* ((ov (cmacs-cams--overlay-at-point)))
    (let ((handle (overlay-get ov 'cmacs-video-handle)))
      (when handle (ignore-errors (cmacs-video-close handle))))
    (delete-overlay ov))
  (if-let* ((blk (cmacs-video-org--block-at-point)))
      (progn (cmacs-video-org--make-overlay blk)
             (message "cmacs-cams: playing %s"
                      (cmacs-video-org--block-src blk)))
    (user-error "Point is not inside a #+BEGIN_VIDEO block")))

(defun cmacs-cams-restart-at-point ()
  "Restart the camera feed whose block contains point."
  (interactive)
  (cmacs-cams-play-at-point))

(defun cmacs-cams-snapshot-at-point ()
  "Capture a frame from the feed at point to `cmacs-cams-snapshot-dir'.
The filename is `<feed-name>-<YYYYMMDD-HHMMSS>.png'.  Returns the
filesystem path written."
  (interactive)
  (let* ((handle (or (cmacs-cams--handle-at-point)
                     (user-error "No active camera feed at point")))
         (blk    (cmacs-video-org--block-at-point))
         (name   (or (when blk
                       (string-trim
                        (buffer-substring-no-properties
                         (cmacs-video-org--block-body-begin blk)
                         (cmacs-video-org--block-body-end blk))))
                     "feed"))
         (stamp  (format-time-string "%Y%m%d-%H%M%S"))
         (file   (expand-file-name
                  (format "%s-%s.png"
                          (cmacs-cams--sanitize name)
                          stamp)
                  cmacs-cams-snapshot-dir)))
    (unless (file-directory-p cmacs-cams-snapshot-dir)
      (make-directory cmacs-cams-snapshot-dir t))
    (cmacs-video-snapshot-to-file handle file)
    (message "cmacs-cams: snapshot → %s" file)
    file))

;;; ------------------------------------------------------ bulk actions

(defun cmacs-cams-play-all ()
  "Start every camera feed in the current dashboard buffer."
  (interactive)
  (cmacs-video-org-render-buffer)
  (message "cmacs-cams: started all feeds"))

(defun cmacs-cams-stop-all ()
  "Stop every camera feed in the current dashboard buffer."
  (interactive)
  (cmacs-video-org-unrender-buffer)
  (message "cmacs-cams: stopped all feeds"))

;;; ------------------------------------------------------ keybindings

;; SPC o c is already taken by +reading.el for `+calendar/open-calendar'.
;; SPC o C (capital) is technically free, but Doom's leader / which-key
;; pipeline appears to case-fold the second key after SPC o, so capital C
;; resolves back to the lowercase calendar binding.  Use `v' (video feeds)
;; — unused and mnemonic.
(map! :leader
      :desc "Cams dashboard" "o v" #'cmacs-cams-dashboard)

(after! org
  (map! :map org-mode-map
        :localleader
        (:prefix ("c" . "cams")
         :desc "Play feed at point"    "p" #'cmacs-cams-play-at-point
         :desc "Stop feed at point"    "s" #'cmacs-cams-stop-at-point
         :desc "Restart feed at point" "r" #'cmacs-cams-restart-at-point
         :desc "Snapshot at point"     "c" #'cmacs-cams-snapshot-at-point
         :desc "Play ALL"              "P" #'cmacs-cams-play-all
         :desc "Stop ALL"              "S" #'cmacs-cams-stop-all
         :desc "Refresh dashboard"     "g" #'cmacs-cams-refresh)))

;;; ------------------------------------------------------ libreclaw chat archive
;;
;; Archive every libreclaw conversation to a readable .org file under
;; ~/Documents/notes/02_areas/libreclaw_chats/.  Both the embedded
;; ("libreclaw") and remote ("libreclaw-remote") subsystems write here;
;; they have independent settings but we point both at the same dir.
;;
;; File names use the default format "%y%m%d-%H%M%S-<agent-name>.org"
;; (e.g. 260522-143015-claude.org) — the timestamp is the conversation
;; start time and <agent-name> is the libreclaw agent.name.
;;
;; These are plain `setq's: `defcustom' is idempotent and keeps a value
;; already set, so this works whether or not cmacs-libreclaw is loaded
;; yet.  The directory itself is created lazily on first write.  The
;; `defvar's just declare the symbols special so byte-compilation is
;; clean before cmacs-libreclaw{,-remote}.el define them for real.
(defvar cmacs-libreclaw-save-conversations-dir)
(defvar cmacs-libreclaw-remote-save-conversations-dir)
(let ((libreclaw-chats
       (expand-file-name "~/Documents/notes/02_areas/libreclaw_chats/")))
  (setq cmacs-libreclaw-save-conversations-dir        libreclaw-chats
        cmacs-libreclaw-remote-save-conversations-dir libreclaw-chats))

(provide 'cmacs)

;;; cmacs.el ends here
