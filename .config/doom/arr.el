;;; arr.el --- Generic client for Sonarr/Radarr/Lidarr/Readarr -*- lexical-binding: t; -*-

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

;; Minimal shared REST client for the *arr family (Sonarr, Radarr, Lidarr,
;; Readarr).  Services are declared as plists in `arr-services'; missing
;; services simply aren't there — commands auto-skip prompts when only one
;; is configured and never error on unconfigured services.
;;
;; Credentials: put each service's API key in ~/.authinfo as:
;;   machine <auth-host> login <arr-auth-user> password <api-key>
;;
;; See arr-queue.el for the unified queue UI built on top.

;;; Code:

(require 'url)
(require 'url-http)
(require 'auth-source)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup arr nil
  "Generic client for the *arr family (Sonarr/Radarr/Lidarr/Readarr)."
  :group 'applications
  :prefix "arr-")

(defcustom arr-services nil
  "List of configured *arr services as plists.

Each plist supports these keys:
  :name      — identifier symbol (e.g. `sonarr').
  :label     — short column badge for UIs (e.g. \"TV\").
  :host      — hostname.
  :port      — port number.
  :scheme    — \"http\" (default) or \"https\".
  :api-path  — API base path (\"/api/v3\" for Sonarr 4+/Radarr,
               \"/api/v1\" for Lidarr/Readarr).
  :auth-host — authinfo `machine' field (defaults to :host).
  :api-key   — optional key override; nil triggers auth-source lookup.
  :resource  — library resource symbol (`series', `movie', `artist',
               `book'); unused in this iteration, reserved for later
               search/library UIs."
  :type '(repeat (plist :key-type keyword :value-type sexp))
  :group 'arr)

(defcustom arr-auth-sources '("~/.authinfo")
  "Value of `auth-sources' used for *arr API-key lookup."
  :type '(repeat (choice file (const default)))
  :group 'arr)

(defcustom arr-auth-user "arr"
  "`login' field used when searching authinfo for an API key."
  :type 'string
  :group 'arr)

(defcustom arr-debug nil
  "When non-nil, log HTTP requests and responses to *arr*."
  :type 'boolean
  :group 'arr)

(defun arr--log (fmt &rest args)
  (when arr-debug
    (with-current-buffer (get-buffer-create "*arr*")
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))))

;;; ---------------------------------------------------------------------
;;; Service registry

(defun arr-configured-services ()
  "Return the list of `:name' symbols currently in `arr-services'."
  (mapcar (lambda (svc) (plist-get svc :name)) arr-services))

(defun arr--service-get (name)
  "Return the service plist with `:name' NAME, or signal `user-error'."
  (or (seq-find (lambda (svc) (eq (plist-get svc :name) name))
                arr-services)
      (user-error "No *arr service configured with name `%s'" name)))

(defun arr-pick-service (&optional prompt)
  "Prompt the user for a configured service and return its `:name'.
If only one service is configured, return it without prompting."
  (let ((names (arr-configured-services)))
    (cond
     ((null names)
      (user-error "No *arr services configured; set `arr-services'"))
     ((= 1 (length names)) (car names))
     (t (intern (completing-read (or prompt "*arr service: ")
                                 (mapcar #'symbol-name names)
                                 nil t))))))

;;; ---------------------------------------------------------------------
;;; URLs / credentials

(defun arr--plist-of (svc)
  (if (symbolp svc) (arr--service-get svc) svc))

(defun arr--base-url (svc)
  (let ((p (arr--plist-of svc)))
    (format "%s://%s:%d"
            (or (plist-get p :scheme) "http")
            (plist-get p :host)
            (plist-get p :port))))

(defun arr--api-url (svc path)
  (let* ((p (arr--plist-of svc))
         (api-path (or (plist-get p :api-path) "/api/v3")))
    (concat (arr--base-url p) api-path path)))

(defun arr--api-key (svc)
  (let* ((p (arr--plist-of svc))
         (override (plist-get p :api-key)))
    (or override
        (let* ((auth-sources arr-auth-sources)
               (host (or (plist-get p :auth-host)
                         (plist-get p :host)))
               (found (car (auth-source-search
                            :host host
                            :user arr-auth-user
                            :require '(:secret)
                            :max 1))))
          (when found
            (let ((secret (plist-get found :secret)))
              (if (functionp secret) (funcall secret) secret))))
        (user-error
         "No *arr API key for %s (set :api-key on the service, or add an authinfo entry for `%s')"
         (plist-get p :name)
         (or (plist-get p :auth-host) (plist-get p :host))))))

;;; ---------------------------------------------------------------------
;;; HTTP

(defun arr--form-encode (alist)
  "URL-form-encode ALIST of (KEY . VALUE) pairs, formatting non-strings."
  (mapconcat
   (lambda (kv)
     (format "%s=%s"
             (url-hexify-string (format "%s" (car kv)))
             (url-hexify-string (format "%s" (or (cdr kv) "")))))
   alist "&"))

(defun arr--response-body ()
  (goto-char (point-min))
  (when (re-search-forward "\r?\n\r?\n" nil t)
    (buffer-substring-no-properties (point) (point-max))))

(defun arr--status-code ()
  (goto-char (point-min))
  (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun arr--json-encode (data)
  "JSON-encode DATA (an alist), mapping nil→null only when explicit.
Uses `:json-false' for booleans so cons-cell alists work cleanly."
  (let ((json-false :json-false)
        (json-null :json-null))
    (json-encode data)))

(defun arr--request (method svc path &optional data mode)
  "Send METHOD request for SVC at PATH with optional DATA.

MODE controls body/param handling:
  nil / `form' — default.  For POST/PUT, DATA is an alist encoded as
                 application/x-www-form-urlencoded.  For GET/DELETE,
                 DATA is appended as a query string.
  `json'       — DATA is an alist, encoded as application/json.
                 Only meaningful for body methods (POST/PUT).

Returns parsed JSON (alist for objects, list for arrays) on HTTP 2xx;
signals an error on non-2xx with the response body."
  (let* ((p    (arr--plist-of svc))
         (mode (or mode 'form))
         (body-method-p (member method '("POST" "PUT")))
         (query (and data (not body-method-p) (arr--form-encode data)))
         (body  (and data body-method-p
                     (pcase mode
                       ('json (arr--json-encode data))
                       (_     (arr--form-encode data)))))
         (final-path (if query
                         (concat path (if (string-match-p "\\?" path) "&" "?") query)
                       path))
         (url (arr--api-url p final-path))
         (url-request-method method)
         (url-request-data (when body (encode-coding-string body 'utf-8)))
         (content-type (pcase mode
                         ('json "application/json")
                         (_     "application/x-www-form-urlencoded")))
         (url-request-extra-headers
          (append
           `(("X-Api-Key" . ,(arr--api-key p))
             ("Accept"    . "application/json"))
           (when body
             `(("Content-Type" . ,content-type))))))
    (arr--log "%s %s (svc=%s mode=%s)" method url (plist-get p :name) mode)
    (let ((buf (url-retrieve-synchronously url t t)))
      (unwind-protect
          (with-current-buffer buf
            (let* ((code (arr--status-code))
                   (resp (or (arr--response-body) "")))
              (arr--log "  => code=%s bodylen=%d" code (length resp))
              (unless (and (numberp code) (>= code 200) (< code 300))
                (error "arr %s %s failed (HTTP %s): %s"
                       method path code
                       (if (> (length resp) 300)
                           (concat (substring resp 0 300) "…")
                         (string-trim resp))))
              (if (string-empty-p (string-trim resp))
                  nil
                (json-parse-string resp
                                   :object-type 'alist
                                   :array-type  'list
                                   :null-object nil
                                   :false-object nil))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(defun arr-get       (svc path &optional params) (arr--request "GET"    svc path params))
(defun arr-post      (svc path &optional data)   (arr--request "POST"   svc path data))
(defun arr-post-json (svc path data)             (arr--request "POST"   svc path data 'json))
(defun arr-put       (svc path &optional data)   (arr--request "PUT"    svc path data))
(defun arr-put-json  (svc path data)             (arr--request "PUT"    svc path data 'json))
(defun arr-delete    (svc path &optional params) (arr--request "DELETE" svc path params))

;;; ---------------------------------------------------------------------
;;; Ping

;;;###autoload
(defun arr-ping (&optional svc)
  "Ping a configured service's /system/status endpoint.
Echoes the app name and version on success; errors otherwise."
  (interactive)
  (let* ((name (or svc (arr-pick-service "Ping which *arr: ")))
         (info (arr-get name "/system/status"))
         (app  (or (alist-get 'appName info)
                   (alist-get 'instanceName info)
                   (symbol-name name)))
         (ver  (or (alist-get 'version info) "?")))
    (message "%s v%s (%s)" app ver name)
    info))

(provide 'arr)
;;; arr.el ends here
