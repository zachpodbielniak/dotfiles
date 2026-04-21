;;; jackett.el --- Jackett torrent search from Emacs -*- lexical-binding: t; -*-

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

;; Query a remote Jackett instance, display results in a tabulated-list
;; buffer, and hand the selected magnet/.torrent off to qbittorrent-transient.
;;
;; Credentials: store the API key in ~/.authinfo as:
;;   machine <jackett-host> login jackett port <jackett-port> password <api-key>

;;; Code:

(require 'url)
(require 'json)
(require 'auth-source)
(require 'tabulated-list)
(require 'subr-x)

(declare-function qbittorrent-webui-add-url "qbittorrent-webui" (url &optional options))
(declare-function qbittorrent-webui-add-file "qbittorrent-webui" (path &optional options))
(declare-function qbittorrent-transient-url "qbittorrent-transient" (url &rest arguments))
(declare-function qbittorrent-transient-filepath "qbittorrent-transient" (filepath &rest arguments))

(defgroup jackett nil
  "Jackett torrent indexer client."
  :group 'applications
  :prefix "jackett-")

(defcustom jackett-host "jackett.local"
  "Hostname (or Tailscale MagicDNS name) of the Jackett server."
  :type 'string
  :group 'jackett)

(defcustom jackett-port 9117
  "Port the Jackett Web UI / API listens on."
  :type 'integer
  :group 'jackett)

(defcustom jackett-scheme "http"
  "URL scheme for reaching Jackett (\"http\" or \"https\")."
  :type '(choice (const "http") (const "https"))
  :group 'jackett)

(defcustom jackett-indexer "all"
  "Indexer ID to query, or \"all\" to aggregate across every configured indexer."
  :type 'string
  :group 'jackett)

(defcustom jackett-api-key nil
  "Jackett API key.  If nil, looked up via `auth-source'."
  :type '(choice (const :tag "Use auth-source" nil) string)
  :group 'jackett)

(defcustom jackett-auth-sources '("~/.authinfo")
  "Value of `auth-sources' to use for Jackett credential lookup.
Defaults to plain ~/.authinfo (not the encrypted .gpg variant)."
  :type '(repeat (choice file (const default)))
  :group 'jackett)

(defcustom jackett-auth-host nil
  "Identifier (`machine' field) used to look up the Jackett key in authinfo.
If nil, `jackett-host' is used.  Set this when your authinfo entry uses a
logical name (e.g. \"nas-main_jackett\") rather than the actual hostname."
  :type '(choice (const :tag "Use `jackett-host'" nil) string)
  :group 'jackett)

(defcustom jackett-auth-user "jackett"
  "`login' field used when searching authinfo for the Jackett key."
  :type 'string
  :group 'jackett)

(defcustom jackett-torrent-download-dir
  (expand-file-name "jackett-torrents" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Directory to store .torrent files downloaded via the \"d\" action.
Defaults to ~/.cache/jackett-torrents (created on first use)."
  :type 'directory
  :group 'jackett)

(defcustom jackett-result-limit 100
  "Maximum number of results to render in the results buffer."
  :type 'integer
  :group 'jackett)

(defcustom jackett-add-backend 'webui
  "Which qBittorrent interface to use when adding torrents.
  `webui'     — POST to a remote qBittorrent WebUI via `qbittorrent-webui'.
  `transient' — hand off to the local `qbittorrent-transient' CLI wrapper.
  `auto'      — prefer `webui' if loaded, else `transient', else raw binary."
  :type '(choice (const webui) (const transient) (const auto))
  :group 'jackett)

(defvar jackett--last-query nil
  "Query string used for the most recent search.  Used by \\='g\\='.")

(defvar-local jackett--entries nil
  "Raw result alists for the current buffer (one per row).")

;;; ---------------------------------------------------------------------
;;; Credentials

(defun jackett--api-key ()
  "Return the Jackett API key, consulting `auth-source' if needed."
  (or jackett-api-key
      (let* ((auth-sources jackett-auth-sources)
             (host (or jackett-auth-host jackett-host))
             (found (car (auth-source-search
                          :host host
                          :user jackett-auth-user
                          :require '(:secret)
                          :max 1))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret) (funcall secret) secret))))
      (user-error
       "No Jackett API key: set `jackett-api-key' or add an authinfo entry for %s"
       (or jackett-auth-host jackett-host))))

;;; ---------------------------------------------------------------------
;;; HTTP

(defun jackett--base-url ()
  (format "%s://%s:%d" jackett-scheme jackett-host jackett-port))

(defun jackett--search-url (query)
  (format "%s/api/v2.0/indexers/%s/results?apikey=%s&Query=%s"
          (jackett--base-url)
          (url-hexify-string jackett-indexer)
          (url-hexify-string (jackett--api-key))
          (url-hexify-string query)))

(defun jackett--parse-json-buffer ()
  "Parse JSON in the current `url-retrieve' response buffer."
  (goto-char (point-min))
  (unless (re-search-forward "\n\n" nil t)
    (error "Malformed HTTP response from Jackett"))
  (json-parse-buffer :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

;;; ---------------------------------------------------------------------
;;; Rendering

(defun jackett--human-size (bytes)
  (cond
   ((not (numberp bytes)) "?")
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fK" (/ bytes 1024.0)))
   ((< bytes (* 1024 1024 1024)) (format "%.1fM" (/ bytes 1024.0 1024.0)))
   (t (format "%.2fG" (/ bytes 1024.0 1024.0 1024.0)))))

(defun jackett--field (entry key)
  (alist-get key entry))

(defun jackett--entry-to-row (entry)
  "Convert one Jackett result ENTRY alist into a tabulated-list row."
  (let ((seeders (or (jackett--field entry 'Seeders) 0))
        (peers   (or (jackett--field entry 'Peers) 0))
        (size    (jackett--field entry 'Size))
        (tracker (or (jackett--field entry 'Tracker) ""))
        (title   (or (jackett--field entry 'Title) "")))
    (list entry
          (vector (format "%d" seeders)
                  (format "%d" peers)
                  (jackett--human-size size)
                  tracker
                  title))))

(defun jackett--sort-numeric (col)
  "Return a sort predicate for tabulated-list column COL interpreted as a number."
  (lambda (a b)
    (< (string-to-number (aref (cadr a) col))
       (string-to-number (aref (cadr b) col)))))

(define-derived-mode jackett-results-mode tabulated-list-mode "Jackett"
  "Major mode for Jackett torrent search results."
  (setq tabulated-list-format
        `[("Seed" 5 ,(jackett--sort-numeric 0) :right-align t)
          ("Peer" 5 ,(jackett--sort-numeric 1) :right-align t)
          ("Size" 8 nil :right-align t)
          ("Tracker" 14 t)
          ("Title" 0 t)])
  (setq tabulated-list-sort-key (cons "Seed" t))
  (tabulated-list-init-header))

;;; ---------------------------------------------------------------------
;;; Actions

(defun jackett--current-entry ()
  (or (tabulated-list-get-id)
      (user-error "No result at point")))

(defun jackett--magnet-or-link (entry)
  "Return a magnet URI from ENTRY, falling back to the .torrent Link."
  (or (let ((m (jackett--field entry 'MagnetUri)))
        (and (stringp m) (not (string-empty-p m)) m))
      (let ((l (jackett--field entry 'Link)))
        (and (stringp l) (not (string-empty-p l)) l))
      (user-error "Result has neither MagnetUri nor Link")))

(defun jackett--dispatch-add-url (uri)
  "Send URI (magnet or http .torrent URL) to the configured qBittorrent backend."
  (pcase jackett-add-backend
    ('webui
     (require 'qbittorrent-webui)
     (qbittorrent-webui-add-url uri))
    ('transient
     (if (fboundp 'qbittorrent-transient-url)
         (qbittorrent-transient-url uri)
       (user-error "qbittorrent-transient not loaded")))
    ('auto
     (cond
      ((featurep 'qbittorrent-webui) (qbittorrent-webui-add-url uri))
      ((fboundp 'qbittorrent-transient-url) (qbittorrent-transient-url uri))
      ((executable-find "qbittorrent")
       (start-process "qbittorrent" nil (executable-find "qbittorrent") uri))
      (t (user-error "No qBittorrent backend available"))))))

(defun jackett--dispatch-add-file (path)
  "Send a local .torrent at PATH to the configured qBittorrent backend."
  (pcase jackett-add-backend
    ('webui
     (require 'qbittorrent-webui)
     (qbittorrent-webui-add-file path))
    ('transient
     (if (fboundp 'qbittorrent-transient-filepath)
         (qbittorrent-transient-filepath path)
       (user-error "qbittorrent-transient not loaded")))
    ('auto
     (cond
      ((featurep 'qbittorrent-webui) (qbittorrent-webui-add-file path))
      ((fboundp 'qbittorrent-transient-filepath) (qbittorrent-transient-filepath path))
      ((executable-find "qbittorrent")
       (start-process "qbittorrent" nil (executable-find "qbittorrent") path))
      (t (user-error "No qBittorrent backend available"))))))

(defun jackett--magnet-p (uri)
  (and (stringp uri) (string-prefix-p "magnet:" uri)))

(defun jackett--download-link-to-file (entry)
  "Fetch ENTRY's `Link' .torrent URL to the local cache; return the file path."
  (let* ((link  (or (jackett--field entry 'Link)
                    (user-error "Result has no .torrent Link")))
         (title (or (jackett--field entry 'Title) "torrent"))
         (safe  (replace-regexp-in-string "[^A-Za-z0-9._-]+" "_" title))
         (dest  (expand-file-name (concat safe ".torrent")
                                  jackett-torrent-download-dir)))
    (unless (file-directory-p jackett-torrent-download-dir)
      (make-directory jackett-torrent-download-dir t))
    (url-copy-file link dest t)
    dest))

(defun jackett-add-to-qbittorrent ()
  "Add the result at point to qBittorrent.
Uses the `MagnetUri' directly if present; otherwise downloads the
`.torrent' via Jackett's `Link' and uploads it as a file (since Jackett
download URLs are often unreachable or unauthenticated from the qBittorrent
container)."
  (interactive)
  (let* ((entry  (jackett--current-entry))
         (magnet (let ((m (jackett--field entry 'MagnetUri)))
                   (and (stringp m) (not (string-empty-p m)) m))))
    (cond
     ((jackett--magnet-p magnet)
      (jackett--dispatch-add-url magnet))
     (t
      (let ((file (jackett--download-link-to-file entry)))
        (jackett--dispatch-add-file file))))
    (message "Sent to qBittorrent: %s" (jackett--field entry 'Title))))

(defun jackett-download-torrent-file ()
  "Download the .torrent file for the result at point, then add it."
  (interactive)
  (let* ((entry (jackett--current-entry))
         (link  (or (jackett--field entry 'Link)
                    (user-error "Result has no .torrent Link")))
         (title (or (jackett--field entry 'Title) "torrent"))
         (safe  (replace-regexp-in-string "[^A-Za-z0-9._-]+" "_" title))
         (dest  (expand-file-name (concat safe ".torrent")
                                  jackett-torrent-download-dir)))
    (unless (file-directory-p jackett-torrent-download-dir)
      (make-directory jackett-torrent-download-dir t))
    (url-copy-file link dest t)
    (jackett--dispatch-add-file dest)
    (message "Downloaded and added: %s" dest)))

(defun jackett-copy-magnet ()
  "Copy the magnet URI for the result at point to the kill ring.
Falls back to the `.torrent' Link with a warning when no magnet is available."
  (interactive)
  (let* ((entry  (jackett--current-entry))
         (magnet (let ((m (jackett--field entry 'MagnetUri)))
                   (and (stringp m) (not (string-empty-p m)) m))))
    (cond
     ((jackett--magnet-p magnet)
      (kill-new magnet)
      (message "Copied magnet: %s" magnet))
     (t
      (let ((link (jackett--field entry 'Link)))
        (when link (kill-new link))
        (message "No magnet — copied .torrent download URL (may not be usable directly)"))))))

(defun jackett-refresh ()
  "Re-run the last search."
  (interactive)
  (if jackett--last-query
      (jackett-search jackett--last-query)
    (call-interactively #'jackett-search)))

;;; ---------------------------------------------------------------------
;;; Entry point

(defvar jackett-results-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'jackett-add-to-qbittorrent)
    (define-key map (kbd "a")   #'jackett-add-to-qbittorrent)
    (define-key map (kbd "d")   #'jackett-download-torrent-file)
    (define-key map (kbd "w")   #'jackett-copy-magnet)
    (define-key map (kbd "g")   #'jackett-refresh)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for `jackett-results-mode'.")

(defun jackett--render (query results)
  (let ((buf (get-buffer-create (format "*jackett: %s*" query))))
    (with-current-buffer buf
      (jackett-results-mode)
      (let* ((truncated (seq-take results jackett-result-limit))
             (rows (mapcar #'jackett--entry-to-row truncated)))
        (setq jackett--entries truncated)
        (setq tabulated-list-entries rows)
        (tabulated-list-print t)))
    (pop-to-buffer buf)))

;;;###autoload
(defun jackett-search (query)
  "Search Jackett for QUERY and display results."
  (interactive "sJackett search: ")
  (setq jackett--last-query query)
  (let* ((url (jackett--search-url query))
         (url-request-extra-headers '(("Accept" . "application/json"))))
    (message "Searching Jackett for %s..." query)
    (url-retrieve
     url
     (lambda (status &rest _)
       (let ((err (plist-get status :error)))
         (when err
           (kill-buffer (current-buffer))
           (error "Jackett request failed: %S" err)))
       (let* ((payload (jackett--parse-json-buffer))
              (results (alist-get 'Results payload)))
         (kill-buffer (current-buffer))
         (if (null results)
             (message "No results for %s" query)
           (jackett--render query results))))
     nil t t)))

(provide 'jackett)
;;; jackett.el ends here
