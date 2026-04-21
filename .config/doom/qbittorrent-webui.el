;;; qbittorrent-webui.el --- Talk to a remote qBittorrent via its WebUI API -*- lexical-binding: t; -*-

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

;; Minimal client for the qBittorrent WebUI API (v2).  Supports login,
;; adding torrents by URL/magnet or by local .torrent file, listing and
;; managing torrents (pause / resume / delete / recheck).
;;
;; Credentials: put your WebUI password in ~/.authinfo as:
;;   machine <qbittorrent-webui-auth-host> login <user> password <pw>

;;; Code:

(require 'url)
(require 'url-http)
(require 'auth-source)
(require 'json)
(require 'subr-x)

(defgroup qbittorrent-webui nil
  "Client for the qBittorrent WebUI API."
  :group 'applications
  :prefix "qbittorrent-webui-")

(defcustom qbittorrent-webui-host "localhost"
  "Hostname of the qBittorrent WebUI."
  :type 'string :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-port 8080
  "Port of the qBittorrent WebUI."
  :type 'integer :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-scheme "http"
  "URL scheme (\"http\" or \"https\")."
  :type '(choice (const "http") (const "https"))
  :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-user "admin"
  "WebUI username."
  :type 'string :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-password nil
  "WebUI password.  If nil, looked up via `auth-source'."
  :type '(choice (const :tag "Use auth-source" nil) string)
  :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-auth-host nil
  "`machine' identifier used for authinfo lookup.
Defaults to `qbittorrent-webui-host' when nil."
  :type '(choice (const :tag "Use webui host" nil) string)
  :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-auth-sources '("~/.authinfo")
  "Value of `auth-sources' to use for WebUI credential lookup."
  :type '(repeat (choice file (const default)))
  :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-debug nil
  "When non-nil, log WebUI requests and responses to *qbittorrent-webui*."
  :type 'boolean
  :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-curl-program "curl"
  "Path to the curl binary used for file uploads (multipart/form-data)."
  :type 'string
  :group 'qbittorrent-webui)

(defcustom qbittorrent-webui-api-style 'pause-resume
  "qBittorrent API naming style for state-change endpoints.
`pause-resume' — qBittorrent 4.x endpoint names (/torrents/pause, /resume).
`start-stop'   — qBittorrent 5.0+ endpoint names (/torrents/stop, /start).
The 4.x names still work as aliases in 5.x, so leave this at the default
unless your server has dropped them."
  :type '(choice (const pause-resume) (const start-stop))
  :group 'qbittorrent-webui)

(defun qbittorrent-webui--log (fmt &rest args)
  (when qbittorrent-webui-debug
    (with-current-buffer (get-buffer-create "*qbittorrent-webui*")
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))))

(defvar qbittorrent-webui--sid nil
  "Cached SID cookie from the last successful login.")

;;; ---------------------------------------------------------------------
;;; Helpers

(defun qbittorrent-webui--base-url ()
  (format "%s://%s:%d"
          qbittorrent-webui-scheme
          qbittorrent-webui-host
          qbittorrent-webui-port))

(defun qbittorrent-webui--password ()
  (or qbittorrent-webui-password
      (let* ((auth-sources qbittorrent-webui-auth-sources)
             (host (or qbittorrent-webui-auth-host qbittorrent-webui-host))
             (found (car (auth-source-search
                          :host host
                          :user qbittorrent-webui-user
                          :require '(:secret)
                          :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret) (funcall secret) secret))))
      (user-error
       "No qBittorrent WebUI password: set `qbittorrent-webui-password' or add an authinfo entry for %s"
       (or qbittorrent-webui-auth-host qbittorrent-webui-host))))

(defun qbittorrent-webui--form-encode (alist)
  "URL-form-encode ALIST of (KEY . VALUE) pairs."
  (mapconcat
   (lambda (kv)
     (format "%s=%s"
             (url-hexify-string (car kv))
             (url-hexify-string (or (cdr kv) ""))))
   alist "&"))

(defun qbittorrent-webui--response-body ()
  "Return the response body (everything after the HTTP headers)."
  (goto-char (point-min))
  (when (re-search-forward "\r?\n\r?\n" nil t)
    (buffer-substring-no-properties (point) (point-max))))

(defun qbittorrent-webui--status-code ()
  "Return the numeric HTTP status code of the current response buffer."
  (goto-char (point-min))
  (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun qbittorrent-webui--post (path &optional data extra-headers)
  "POST DATA (string, already encoded) to PATH.  Return the response buffer."
  (let* ((url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers
          (append extra-headers
                  (when qbittorrent-webui--sid
                    `(("Cookie" . ,(format "SID=%s" qbittorrent-webui--sid))))
                  `(("Referer" . ,(qbittorrent-webui--base-url))))))
    (url-retrieve-synchronously
     (concat (qbittorrent-webui--base-url) path) t t)))

;;; ---------------------------------------------------------------------
;;; Login

(defun qbittorrent-webui--extract-sid (buffer)
  "Extract the SID cookie value from a login response BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "Set-Cookie: *SID=\\([^;[:space:]]+\\)" nil t)
      (match-string 1))))

(defun qbittorrent-webui-login ()
  "Authenticate against the qBittorrent WebUI and cache the SID."
  (interactive)
  (let* ((body (qbittorrent-webui--form-encode
                `(("username" . ,qbittorrent-webui-user)
                  ("password" . ,(qbittorrent-webui--password)))))
         (buf  (let ((url-request-method "POST")
                     (url-request-data body)
                     (url-request-extra-headers
                      `(("Content-Type" . "application/x-www-form-urlencoded")
                        ("Referer"      . ,(qbittorrent-webui--base-url)))))
                 (url-retrieve-synchronously
                  (concat (qbittorrent-webui--base-url) "/api/v2/auth/login")
                  t t))))
    (unwind-protect
        (with-current-buffer buf
          (let ((code (qbittorrent-webui--status-code))
                (body (qbittorrent-webui--response-body)))
            (unless (eq code 200)
              (error "qBittorrent login failed (HTTP %s)" code))
            (when (and body (string-match-p "Fails" body))
              (error "qBittorrent login rejected: %s" (string-trim body)))
            (let ((sid (qbittorrent-webui--extract-sid buf)))
              (unless sid
                (error "qBittorrent login succeeded but no SID cookie returned"))
              (setq qbittorrent-webui--sid sid)
              (when (called-interactively-p 'interactive)
                (message "qBittorrent: logged in to %s" (qbittorrent-webui--base-url)))
              sid)))
      (kill-buffer buf))))

(defun qbittorrent-webui--ensure-session ()
  (unless qbittorrent-webui--sid
    (qbittorrent-webui-login)))

;;; ---------------------------------------------------------------------
;;; Add torrents

(defun qbittorrent-webui--multipart-body (boundary fields)
  "Build a multipart/form-data body.

FIELDS is a list of entries, each either:
  (NAME . VALUE)                              — text field
  (NAME :file PATH :content-type CT)          — file field (CT defaults to
                                                application/x-bittorrent)"
  (let ((parts '()))
    (dolist (f fields)
      (let ((name (car f))
            (val  (cdr f)))
        (cond
         ((and (listp val) (eq (car val) :file))
          (let* ((path (plist-get val :file))
                 (ct   (or (plist-get val :content-type)
                           "application/x-bittorrent"))
                 (contents
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (insert-file-contents-literally path)
                    (buffer-string))))
            (push (concat
                   "--" boundary "\r\n"
                   (format "Content-Disposition: form-data; name=\"%s\"; filename=\"%s\"\r\n"
                           name (file-name-nondirectory path))
                   (format "Content-Type: %s\r\n\r\n" ct)
                   contents
                   "\r\n")
                  parts)))
         (t
          (push (concat
                 "--" boundary "\r\n"
                 (format "Content-Disposition: form-data; name=\"%s\"\r\n\r\n" name)
                 (or val "")
                 "\r\n")
                parts)))))
    (push (concat "--" boundary "--\r\n") parts)
    (mapconcat #'identity (nreverse parts) "")))

(defun qbittorrent-webui--check-add-response (buf retry-fn)
  "Inspect add-torrent response in BUF; call RETRY-FN on session expiry."
  (with-current-buffer buf
    (let* ((code (qbittorrent-webui--status-code))
           (body (string-trim (or (qbittorrent-webui--response-body) ""))))
      (qbittorrent-webui--log "add response: code=%s body=%S" code body)
      (cond
       ((eq code 403)
        (setq qbittorrent-webui--sid nil)
        (qbittorrent-webui-login)
        (funcall retry-fn))
       ((not (eq code 200))
        (error "qBittorrent add failed (HTTP %s): %s" code body))
       ;; qBittorrent returns "Ok." on success, "Fails." on failure — both with HTTP 200.
       ((string-match-p "\\`Fails\\." body)
        (error "qBittorrent rejected the torrent (body: %S). Check host-header validation / duplicate torrent / invalid magnet" body))
       ((and (not (string-empty-p body))
             (not (string-match-p "\\`Ok\\." body)))
        ;; Unexpected body — surface it.
        (error "qBittorrent add: unexpected response body: %S" body))
       (t t)))))

;;;###autoload
(defun qbittorrent-webui-add-url (url &optional options)
  "Add URL (magnet link or .torrent HTTP URL) to qBittorrent.
OPTIONS is an optional alist of extra form fields, e.g.
  ((\"category\" . \"movies\") (\"paused\" . \"true\"))."
  (interactive "sMagnet or URL: ")
  (qbittorrent-webui--ensure-session)
  (let* ((fields (cons (cons "urls" url) options))
         (body   (qbittorrent-webui--form-encode fields))
         (buf    (qbittorrent-webui--post
                  "/api/v2/torrents/add" body
                  '(("Content-Type" . "application/x-www-form-urlencoded")))))
    (qbittorrent-webui--log "add-url: %s" url)
    (unwind-protect
        (let ((result (qbittorrent-webui--check-add-response
                       buf (lambda () (qbittorrent-webui-add-url url options)))))
          (when (and result (called-interactively-p 'interactive))
            (message "qBittorrent: added %s" url))
          result)
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;###autoload
(defun qbittorrent-webui-add-file (path &optional options)
  "Add a local .torrent file at PATH to qBittorrent.
OPTIONS is an optional alist of extra form fields.

Uses curl (see `qbittorrent-webui-curl-program') for the multipart
upload, because Emacs's built-in `url' library mangles binary data
when building multipart/form-data bodies."
  (interactive "fTorrent file: ")
  (qbittorrent-webui--ensure-session)
  (unless (file-readable-p path)
    (user-error "Cannot read torrent file: %s" path))
  (unless (executable-find qbittorrent-webui-curl-program)
    (user-error "curl not found (set `qbittorrent-webui-curl-program')"))
  (let* ((abs-path (expand-file-name path))
         (url  (concat (qbittorrent-webui--base-url) "/api/v2/torrents/add"))
         (args (append
                (list "--silent" "--show-error"
                      "--write-out" "\n%{http_code}"
                      "--referer" (qbittorrent-webui--base-url)
                      "--cookie" (format "SID=%s" qbittorrent-webui--sid)
                      "--form" (format "torrents=@%s;type=application/x-bittorrent"
                                       abs-path))
                (mapcan (lambda (kv)
                          (list "--form" (format "%s=%s" (car kv) (cdr kv))))
                        options)
                (list url))))
    (qbittorrent-webui--log "add-file: %s (curl)" abs-path)
    (with-temp-buffer
      (let* ((exit (apply #'call-process qbittorrent-webui-curl-program
                          nil t nil args))
             (out  (buffer-string))
             ;; We appended "\n%{http_code}" via --write-out, so the last
             ;; line is the HTTP status code.
             (split (and (string-match "\\(?:\\`\\|\n\\)\\([0-9]\\{3\\}\\)\\s-*\\'" out)
                         (cons (substring out 0 (match-beginning 0))
                               (string-to-number (match-string 1 out)))))
             (body (string-trim (or (car-safe split) out)))
             (code (cdr-safe split)))
        (qbittorrent-webui--log "curl exit=%s code=%s body=%S" exit code body)
        (unless (eq exit 0)
          (error "curl failed (exit %s): %s" exit body))
        (cond
         ((eq code 403)
          (setq qbittorrent-webui--sid nil)
          (qbittorrent-webui-login)
          (qbittorrent-webui-add-file path options))
         ((not (eq code 200))
          (error "qBittorrent add failed (HTTP %s): %s" code body))
         ((string-match-p "\\`Fails\\." body)
          (error "qBittorrent rejected the torrent (body: %S). Check host-header validation, duplicate torrent, or invalid file" body))
         (t
          (when (called-interactively-p 'interactive)
            (message "qBittorrent: added %s" (file-name-nondirectory path)))
          t))))))

;;; ---------------------------------------------------------------------
;;; Generic API helpers (GET/POST returning parsed body)

(defun qbittorrent-webui--get (path)
  "GET PATH and return the response buffer."
  (let ((url-request-method "GET")
        (url-request-data nil)
        (url-request-extra-headers
         (append
          (when qbittorrent-webui--sid
            `(("Cookie" . ,(format "SID=%s" qbittorrent-webui--sid))))
          `(("Referer" . ,(qbittorrent-webui--base-url))))))
    (url-retrieve-synchronously
     (concat (qbittorrent-webui--base-url) path) t t)))

(defun qbittorrent-webui--try-call (method path &optional data)
  "Issue one HTTP request.  Return (CODE . BODY)."
  (let ((buf (if (equal method "GET")
                 (qbittorrent-webui--get path)
               (qbittorrent-webui--post
                path
                (when data (qbittorrent-webui--form-encode data))
                '(("Content-Type" . "application/x-www-form-urlencoded"))))))
    (unwind-protect
        (with-current-buffer buf
          (cons (qbittorrent-webui--status-code)
                (or (qbittorrent-webui--response-body) "")))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun qbittorrent-webui--api-call (method path &optional data)
  "Call METHOD PATH with optional DATA (alist of form fields).
Return the response body string on HTTP 200.  Re-login and retry once
on HTTP 403.  Signal an error for any other status."
  (qbittorrent-webui--ensure-session)
  (let* ((res  (qbittorrent-webui--try-call method path data))
         (code (car res))
         (body (cdr res)))
    (qbittorrent-webui--log "%s %s: code=%s" method path code)
    (when (eq code 403)
      (setq qbittorrent-webui--sid nil)
      (qbittorrent-webui-login)
      (setq res (qbittorrent-webui--try-call method path data)
            code (car res)
            body (cdr res))
      (qbittorrent-webui--log "%s %s (retry): code=%s" method path code))
    (if (eq code 200)
        body
      (error "qBittorrent %s %s failed (HTTP %s): %s"
             method path code (string-trim (or body ""))))))

(defun qbittorrent-webui--hashes-param (hashes)
  "Return qBittorrent's `hashes' parameter from HASHES (string or list)."
  (cond
   ((stringp hashes) hashes)
   ((listp hashes)   (mapconcat #'identity hashes "|"))
   (t (error "hashes must be string or list: %S" hashes))))

;;; ---------------------------------------------------------------------
;;; Torrent listing and management

;;;###autoload
(defun qbittorrent-webui-torrents-info (&optional filter category)
  "Return a list of torrent alists from `/api/v2/torrents/info'.
FILTER is an optional state filter (\"all\", \"downloading\", \"completed\",
\"paused\", \"active\", \"inactive\", \"resumed\", \"stalled\", \"errored\").
CATEGORY optionally restricts results to a category name."
  (let* ((params (delq nil
                       (list (when filter   (cons "filter" filter))
                             (when category (cons "category" category)))))
         (path   (if params
                     (format "/api/v2/torrents/info?%s"
                             (qbittorrent-webui--form-encode params))
                   "/api/v2/torrents/info"))
         (body   (qbittorrent-webui--api-call "GET" path)))
    (json-parse-string body
                       :object-type 'alist
                       :array-type  'list
                       :null-object nil
                       :false-object nil)))

(defun qbittorrent-webui--state-endpoint (action)
  "Return the endpoint path for ACTION (`pause' or `resume')."
  (pcase (list action qbittorrent-webui-api-style)
    (`(pause  pause-resume) "/api/v2/torrents/pause")
    (`(resume pause-resume) "/api/v2/torrents/resume")
    (`(pause  start-stop)   "/api/v2/torrents/stop")
    (`(resume start-stop)   "/api/v2/torrents/start")
    (_ (error "Unknown action: %S" action))))

;;;###autoload
(defun qbittorrent-webui-torrents-pause (hashes)
  "Pause (or in v5+, stop) torrent(s) by HASHES (string or list)."
  (qbittorrent-webui--api-call
   "POST" (qbittorrent-webui--state-endpoint 'pause)
   `(("hashes" . ,(qbittorrent-webui--hashes-param hashes))))
  t)

;;;###autoload
(defun qbittorrent-webui-torrents-resume (hashes)
  "Resume (or in v5+, start) torrent(s) by HASHES (string or list)."
  (qbittorrent-webui--api-call
   "POST" (qbittorrent-webui--state-endpoint 'resume)
   `(("hashes" . ,(qbittorrent-webui--hashes-param hashes))))
  t)

;;;###autoload
(defun qbittorrent-webui-torrents-delete (hashes &optional delete-files)
  "Delete torrent(s) by HASHES.  When DELETE-FILES is non-nil, also wipe data."
  (qbittorrent-webui--api-call
   "POST" "/api/v2/torrents/delete"
   `(("hashes"      . ,(qbittorrent-webui--hashes-param hashes))
     ("deleteFiles" . ,(if delete-files "true" "false"))))
  t)

;;;###autoload
(defun qbittorrent-webui-torrents-recheck (hashes)
  "Force a hash-recheck on torrent(s) by HASHES."
  (qbittorrent-webui--api-call
   "POST" "/api/v2/torrents/recheck"
   `(("hashes" . ,(qbittorrent-webui--hashes-param hashes))))
  t)

(provide 'qbittorrent-webui)
;;; qbittorrent-webui.el ends here
