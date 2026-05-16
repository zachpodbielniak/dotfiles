;;; container-registry-browse.el --- Search and browse container image registries -*- lexical-binding: t; -*-

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

;; Search container image registries (docker.io, quay.io, ghcr.io,
;; registry.fedoraproject.org, lscr.io, registry.access.redhat.com,
;; mcr.microsoft.com, public.ecr.aws, registry.k8s.io, ...) and list
;; tags of arbitrary OCI image references, from inside Emacs.
;;
;; The registry table is data-driven: each registry is a plist with
;; :id :name :host :search-fn :tags-fn.  Add new ones with
;; `container-registry-browse-register'.  See the call list at the
;; bottom of this file for examples.
;;
;; Entry points (under SPC s c):
;;   container-registry-browse-search       — SPC s c s
;;   container-registry-browse-search-all   — SPC s c S
;;   container-registry-browse-tags         — SPC s c t
;;   container-registry-browse-inspect      — SPC s c i
;;
;; Buffers:
;;   *container-registry: <id> <query>*   results — RET opens tags
;;   *container-tags: <host>/<repo>*      tag listing
;;   *container-inspect: <ref>*           skopeo inspect output
;;
;; Dependencies:
;;   * `skopeo' on PATH — used for OCI-standard list-tags / inspect.
;;     Install:   dnf install skopeo   or   brew install skopeo
;;
;; Adding a new registry (example — GCR):
;;
;;   (container-registry-browse-register
;;    :id 'gcr :name "Google" :host "gcr.io"
;;    :search-fn nil      ;; no public search
;;    :tags-fn   'skopeo) ;; generic OCI fallback
;;
;; A :search-fn of nil means the registry is callable for tags only
;; (it will not appear in the search prompts).  A :tags-fn value of
;; the symbol `skopeo' uses the generic fallback; alternatively pass
;; a function of (HOST REPO) returning a list of plists with
;; (:tag :pushed :size :digest).

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'url)
(require 'url-util)
(require 'json)
(require 'browse-url)

;; Forward declaration to silence byte-compile when loaded outside doom.
(declare-function map! "doom-keybinds")

;;; ----------------------------------------------------------------- defcustoms

(defgroup container-registry-browse nil
  "Search and browse container image registries."
  :group 'applications
  :prefix "container-registry-browse-")

(defcustom container-registry-browse-tag-limit 200
  "Maximum number of tags to render in the tags buffer.
Repositories with more tags are truncated; press G in
`container-registry-browse-tags-mode' to load them all."
  :type 'integer
  :group 'container-registry-browse)

(defcustom container-registry-browse-search-limit 50
  "Maximum number of search results to request per registry."
  :type 'integer
  :group 'container-registry-browse)

(defcustom container-registry-browse-skopeo-program "skopeo"
  "Name of the skopeo executable used for OCI operations."
  :type 'string
  :group 'container-registry-browse)

(defcustom container-registry-browse-http-timeout 20
  "Seconds to wait for an HTTP response from a registry API."
  :type 'integer
  :group 'container-registry-browse)

;;; ----------------------------------------------------------------- faces (catppuccin-mocha)

(defface container-registry-browse-registry-face
  '((t (:foreground "#cba6f7" :weight bold)))
  "Face for registry names in result listings."
  :group 'container-registry-browse)

(defface container-registry-browse-repo-face
  '((t (:foreground "#89b4fa")))
  "Face for repository names."
  :group 'container-registry-browse)

(defface container-registry-browse-tag-face
  '((t (:foreground "#f9e2af" :weight bold)))
  "Face for tag names."
  :group 'container-registry-browse)

(defface container-registry-browse-meta-face
  '((t (:foreground "#7f849c" :slant italic)))
  "Face for secondary metadata (stars, dates, sizes, digests)."
  :group 'container-registry-browse)

(defface container-registry-browse-description-face
  '((t (:foreground "#cdd6f4")))
  "Face for descriptions in result listings."
  :group 'container-registry-browse)

;;; ----------------------------------------------------------------- registry table

(defvar container-registry-browse--registries nil
  "Alist of (ID . PLIST) describing known container registries.
Each PLIST has keys :id :name :host :search-fn :tags-fn :doc-url.
Use `container-registry-browse-register' to add entries.")

(cl-defun container-registry-browse-register
    (&key id name host search-fn (tags-fn 'skopeo) doc-url)
  "Register a container registry.

ID is a symbol unique to this registry.
NAME is a human-readable label shown in prompts and result listings.
HOST is the registry hostname (as appears in image references).
SEARCH-FN is a function of one argument (the query string) returning
  a list of plists (:host :repo :description :stars :pulls), or nil
  if the registry does not support search.
TAGS-FN is either a function of (HOST REPO) returning a list of
  plists (:tag :pushed :size :digest), or the symbol `skopeo' to use
  the generic skopeo-based implementation (the default).
DOC-URL is an optional URL passed to `browse-url' from the results
  buffer."
  (unless (and (symbolp id) id)
    (error "container-registry-browse: :id must be a non-nil symbol"))
  (unless (stringp name)
    (error "container-registry-browse: :name must be a string"))
  (unless (stringp host)
    (error "container-registry-browse: :host must be a string"))
  (let ((entry (list :id id :name name :host host
                     :search-fn search-fn :tags-fn tags-fn
                     :doc-url doc-url)))
    (setf (alist-get id container-registry-browse--registries) entry))
  id)

(defun container-registry-browse--registry-by-id (id)
  (alist-get id container-registry-browse--registries))

(defun container-registry-browse--registry-by-host (host)
  (cdr (seq-find (lambda (cell)
                   (string-equal host (plist-get (cdr cell) :host)))
                 container-registry-browse--registries)))

(defun container-registry-browse--searchable-registries ()
  (seq-filter (lambda (cell) (plist-get (cdr cell) :search-fn))
              container-registry-browse--registries))

;;; ----------------------------------------------------------------- HTTP

(defun container-registry-browse--http-status-code ()
  (goto-char (point-min))
  (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (string-to-number (match-string 1))))

(defun container-registry-browse--http-response-body-pos ()
  (goto-char (point-min))
  (when (re-search-forward "\r?\n\r?\n" nil t)
    (point)))

(defun container-registry-browse--http-json (url &optional headers)
  "GET URL with optional HEADERS alist, return parsed JSON (alist/list)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          (append '(("Accept" . "application/json")
                    ("User-Agent" . "emacs-container-registry-browse"))
                  headers))
         (buf (url-retrieve-synchronously
               url t t container-registry-browse-http-timeout)))
    (unless buf
      (user-error "container-registry-browse: HTTP request to %s timed out" url))
    (unwind-protect
        (with-current-buffer buf
          (let* ((code (container-registry-browse--http-status-code))
                 (body-pos (container-registry-browse--http-response-body-pos)))
            (unless (and (numberp code) (>= code 200) (< code 300))
              (user-error "container-registry-browse: HTTP %s from %s" code url))
            (unless body-pos
              (user-error "container-registry-browse: empty response from %s" url))
            (goto-char body-pos)
            (json-parse-buffer :object-type 'alist
                               :array-type  'list
                               :null-object nil
                               :false-object nil)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun container-registry-browse--http-json-post (url body-alist &optional headers)
  "POST JSON-encoded BODY-ALIST to URL, return parsed JSON response."
  (let* ((url-request-method "POST")
         (url-request-data
          (encode-coding-string (json-encode body-alist) 'utf-8))
         (url-request-extra-headers
          (append '(("Accept" . "application/json")
                    ("Content-Type" . "application/json")
                    ("User-Agent" . "emacs-container-registry-browse"))
                  headers))
         (buf (url-retrieve-synchronously
               url t t container-registry-browse-http-timeout)))
    (unless buf
      (user-error "container-registry-browse: HTTP POST to %s timed out" url))
    (unwind-protect
        (with-current-buffer buf
          (let* ((code (container-registry-browse--http-status-code))
                 (body-pos (container-registry-browse--http-response-body-pos)))
            (unless (and (numberp code) (>= code 200) (< code 300))
              (user-error "container-registry-browse: HTTP %s from %s" code url))
            (goto-char (or body-pos (point-min)))
            (json-parse-buffer :object-type 'alist
                               :array-type  'list
                               :null-object nil
                               :false-object nil)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; ----------------------------------------------------------------- skopeo

(defun container-registry-browse--require-skopeo ()
  (unless (executable-find container-registry-browse-skopeo-program)
    (user-error
     "container-registry-browse needs `%s' on PATH.  Install: `dnf install skopeo' / `brew install skopeo'"
     container-registry-browse-skopeo-program)))

(defun container-registry-browse--skopeo-run (&rest args)
  "Run skopeo with ARGS; return stdout as string or signal user-error with stderr."
  (container-registry-browse--require-skopeo)
  (let ((errfile (make-temp-file "skopeo-err-")))
    (unwind-protect
        (with-temp-buffer
          (let ((code (apply #'call-process
                             container-registry-browse-skopeo-program
                             nil (list (current-buffer) errfile) nil args)))
            (if (= code 0)
                (buffer-string)
              (user-error
               "skopeo %s failed: %s"
               (string-join args " ")
               (with-temp-buffer
                 (insert-file-contents errfile)
                 (string-trim (buffer-string)))))))
      (when (file-exists-p errfile)
        (delete-file errfile)))))

(defun container-registry-browse--skopeo-list-tags (host repo)
  "Return list of (:tag NAME) plists for HOST/REPO via skopeo list-tags."
  (let* ((raw (container-registry-browse--skopeo-run
               "list-tags" (format "docker://%s/%s" host repo)))
         (parsed (json-parse-string raw
                                    :object-type 'alist
                                    :array-type  'list
                                    :null-object nil
                                    :false-object nil))
         (tags (alist-get 'Tags parsed)))
    (mapcar (lambda (name) (list :tag name)) tags)))

(defun container-registry-browse--skopeo-inspect-json (ref)
  "Return parsed JSON inspect data for REF (`host/repo[:tag]')."
  (let ((raw (container-registry-browse--skopeo-run
              "inspect" (format "docker://%s" ref))))
    (json-parse-string raw
                       :object-type 'alist
                       :array-type  'list
                       :null-object nil
                       :false-object nil)))

(defun container-registry-browse--call-tags-fn (registry repo)
  "Dispatch to REGISTRY's :tags-fn for REPO; default to skopeo."
  (let ((host    (plist-get registry :host))
        (tags-fn (plist-get registry :tags-fn)))
    (cond
     ((or (eq tags-fn 'skopeo) (null tags-fn))
      (container-registry-browse--skopeo-list-tags host repo))
     ((functionp tags-fn)
      (funcall tags-fn host repo))
     (t
      (user-error "container-registry-browse: invalid :tags-fn for registry %s"
                  (plist-get registry :id))))))

;;; ----------------------------------------------------------------- per-registry: docker.io

(defun container-registry-browse--docker-search (query)
  (let* ((url (format "https://hub.docker.com/v2/search/repositories/?query=%s&page_size=%d"
                      (url-hexify-string query)
                      container-registry-browse-search-limit))
         (data (container-registry-browse--http-json url))
         (results (alist-get 'results data)))
    (mapcar
     (lambda (r)
       (list :host "docker.io"
             :repo (alist-get 'repo_name r)
             :description (or (alist-get 'short_description r) "")
             :stars (or (alist-get 'star_count r) 0)
             :pulls (or (alist-get 'pull_count r) 0)))
     results)))

(defun container-registry-browse--docker-tags (_host repo)
  "Docker Hub v2 tag listing — returns rich metadata (size, last_updated)."
  (let* ((canon (if (string-match-p "/" repo) repo (concat "library/" repo)))
         (url (format "https://hub.docker.com/v2/repositories/%s/tags/?page_size=100"
                      canon))
         (data (container-registry-browse--http-json url))
         (results (alist-get 'results data)))
    (mapcar
     (lambda (entry)
       (list :tag    (alist-get 'name entry)
             :pushed (alist-get 'last_updated entry)
             :size   (alist-get 'full_size entry)
             :digest (alist-get 'digest entry)))
     results)))

;;; ----------------------------------------------------------------- per-registry: quay.io

(defun container-registry-browse--quay-search (query)
  "Search quay.io.  The find/repositories endpoint returns mixed kinds
(repository, application, ...); we keep only `repository' rows.  Each
row's `namespace' is an object alist whose `name' field is the actual
namespace string."
  (let* ((url (format "https://quay.io/api/v1/find/repositories?query=%s&includeUsage=false&page=1"
                      (url-hexify-string query)))
         (data (container-registry-browse--http-json url))
         (results (alist-get 'results data)))
    (delq nil
          (mapcar
           (lambda (r)
             (let* ((kind   (alist-get 'kind r))
                    (ns-raw (alist-get 'namespace r))
                    (ns     (cond ((stringp ns-raw) ns-raw)
                                  ((listp   ns-raw) (alist-get 'name ns-raw))))
                    (name   (alist-get 'name r))
                    (score  (or (alist-get 'popularity r)
                                (alist-get 'score r) 0)))
               (when (or (null kind) (string-equal kind "repository"))
                 (list :host "quay.io"
                       :repo (if ns (format "%s/%s" ns name) name)
                       :description (or (alist-get 'description r) "")
                       :stars (if (numberp score) (truncate score) 0)
                       :pulls 0))))
           results))))

(defun container-registry-browse--quay-tags (_host repo)
  (let* ((url (format "https://quay.io/api/v1/repository/%s/tag/?limit=100&onlyActiveTags=true"
                      repo))
         (data (container-registry-browse--http-json url))
         (results (alist-get 'tags data)))
    (mapcar
     (lambda (entry)
       (list :tag    (alist-get 'name entry)
             :pushed (alist-get 'last_modified entry)
             :size   (alist-get 'size entry)
             :digest (alist-get 'manifest_digest entry)))
     results)))

;;; ----------------------------------------------------------------- per-registry: fedora

(defun container-registry-browse--fedora-search (query)
  "Fetch Fedora's _catalog and filter client-side by QUERY substring."
  (let* ((url "https://registry.fedoraproject.org/v2/_catalog?n=2000")
         (data (container-registry-browse--http-json url))
         (repos (alist-get 'repositories data))
         (q (downcase query)))
    (cl-loop for r in repos
             when (string-match-p (regexp-quote q) (downcase r))
             collect (list :host "registry.fedoraproject.org"
                           :repo r
                           :description ""
                           :stars 0
                           :pulls 0))))

;;; ----------------------------------------------------------------- per-registry: lscr.io

(defvar container-registry-browse--lscr-cache nil
  "Cached list of LinuxServer fleet images (alist objects).")

(defun container-registry-browse--lscr-fetch-fleet ()
  (or container-registry-browse--lscr-cache
      (setq container-registry-browse--lscr-cache
            (let* ((data (container-registry-browse--http-json
                          "https://fleet.linuxserver.io/api/v1/images")))
              (or (alist-get 'data data)
                  ;; Some shapes return the array directly.
                  (and (listp data) data))))))

(defun container-registry-browse--lscr-search (query)
  (let ((images (container-registry-browse--lscr-fetch-fleet))
        (q (downcase query)))
    (cl-loop for img in images
             for name = (or (alist-get 'name img) "")
             for desc = (or (alist-get 'description img) "")
             when (or (string-match-p (regexp-quote q) (downcase name))
                      (string-match-p (regexp-quote q) (downcase desc)))
             collect (list :host "lscr.io"
                           :repo (format "linuxserver/%s" name)
                           :description desc
                           :stars (or (alist-get 'stars img) 0)
                           :pulls (or (alist-get 'downloads img) 0)))))

;;; ----------------------------------------------------------------- per-registry: redhat

(defun container-registry-browse--redhat-search (query)
  (let* ((q (url-hexify-string query))
         (url (format "https://catalog.redhat.com/api/containers/v1/repositories?filter=repository%%3D%%7E%s&page_size=%d"
                      q
                      container-registry-browse-search-limit))
         (data (container-registry-browse--http-json url))
         (results (alist-get 'data data)))
    (mapcar
     (lambda (r)
       (list :host "registry.access.redhat.com"
             :repo (or (alist-get 'repository r) "")
             :description (or (alist-get 'display_data r)
                              (alist-get 'description r)
                              "")
             :stars 0
             :pulls 0))
     results)))

;;; ----------------------------------------------------------------- per-registry: public.ecr.aws

(defun container-registry-browse--ecr-public-search (query)
  (let* ((data (container-registry-browse--http-json-post
                "https://api.us-east-1.gallery.ecr.aws/searchRepositoryCatalogData"
                `((searchTerm . ,query)
                  (maxResults . ,container-registry-browse-search-limit))))
         (results (alist-get 'repositoryCatalogSearchResultList data)))
    (mapcar
     (lambda (r)
       (let ((alias (alist-get 'primaryRegistryAliasName r))
             (name  (alist-get 'repositoryName r)))
         (list :host "public.ecr.aws"
               :repo (if alias (format "%s/%s" alias name) name)
               :description (or (alist-get 'shortDescription r) "")
               :stars 0
               :pulls (or (alist-get 'downloadCount r) 0))))
     results)))

;;; ----------------------------------------------------------------- built-in registry registrations

(container-registry-browse-register
 :id 'docker :name "Docker Hub" :host "docker.io"
 :search-fn #'container-registry-browse--docker-search
 :tags-fn   #'container-registry-browse--docker-tags
 :doc-url   "https://hub.docker.com")

(container-registry-browse-register
 :id 'quay :name "Quay.io" :host "quay.io"
 :search-fn #'container-registry-browse--quay-search
 :tags-fn   #'container-registry-browse--quay-tags
 :doc-url   "https://quay.io")

(container-registry-browse-register
 :id 'ghcr :name "GitHub Container Registry" :host "ghcr.io"
 :search-fn nil
 :tags-fn   'skopeo
 :doc-url   "https://ghcr.io")

(container-registry-browse-register
 :id 'fedora :name "Fedora" :host "registry.fedoraproject.org"
 :search-fn #'container-registry-browse--fedora-search
 :tags-fn   'skopeo
 :doc-url   "https://registry.fedoraproject.org")

(container-registry-browse-register
 :id 'lscr :name "LinuxServer.io" :host "lscr.io"
 :search-fn #'container-registry-browse--lscr-search
 :tags-fn   'skopeo
 :doc-url   "https://fleet.linuxserver.io")

(container-registry-browse-register
 :id 'redhat :name "Red Hat" :host "registry.access.redhat.com"
 :search-fn #'container-registry-browse--redhat-search
 :tags-fn   'skopeo
 :doc-url   "https://catalog.redhat.com")

(container-registry-browse-register
 :id 'mcr :name "Microsoft" :host "mcr.microsoft.com"
 :search-fn nil
 :tags-fn   'skopeo
 :doc-url   "https://mcr.microsoft.com")

(container-registry-browse-register
 :id 'ecr-public :name "AWS Public ECR" :host "public.ecr.aws"
 :search-fn #'container-registry-browse--ecr-public-search
 :tags-fn   'skopeo
 :doc-url   "https://gallery.ecr.aws")

(container-registry-browse-register
 :id 'k8s :name "Kubernetes" :host "registry.k8s.io"
 :search-fn nil
 :tags-fn   'skopeo
 :doc-url   "https://registry.k8s.io")

;;; ----------------------------------------------------------------- formatting helpers

(defun container-registry-browse--parse-ref (ref)
  "Parse REF (e.g. \"docker.io/library/nginx:1.25\") into (:host :repo :tag).
Signals if REF lacks a `/'."
  (unless (string-match-p "/" ref)
    (user-error "Invalid image reference %S — expected host/repo" ref))
  (let* ((slash (string-match "/" ref))
         (host  (substring ref 0 slash))
         (rest  (substring ref (1+ slash))))
    (if (string-match "\\`\\(.+\\):\\([^/:]+\\)\\'" rest)
        (list :host host
              :repo (match-string 1 rest)
              :tag  (match-string 2 rest))
      (list :host host :repo rest :tag nil))))

(defun container-registry-browse--format-size (bytes)
  (cond ((null bytes) "—")
        ((not (numberp bytes)) (format "%s" bytes))
        ((< bytes 1024) (format "%dB" bytes))
        ((< bytes (* 1024 1024)) (format "%.0fK" (/ bytes 1024.0)))
        ((< bytes (* 1024 1024 1024)) (format "%.1fM" (/ bytes 1048576.0)))
        (t (format "%.2fG" (/ bytes 1073741824.0)))))

(defun container-registry-browse--format-pushed (pushed)
  (cond ((null pushed) "—")
        ((stringp pushed) (substring pushed 0 (min 10 (length pushed))))
        (t (format "%s" pushed))))

(defun container-registry-browse--format-int (n)
  (if (numberp n) (number-to-string n) "0"))

(defun container-registry-browse--tag-cmp-name (a b)
  "Sort numeric/semver-style tag names first, then by reverse name order."
  (let* ((semver-p (lambda (s) (string-match-p "\\`v?[0-9]" (or s "")))))
    (cond
     ((and (funcall semver-p a) (not (funcall semver-p b))) t)
     ((and (funcall semver-p b) (not (funcall semver-p a))) nil)
     (t (string> (or a "") (or b ""))))))

(defun container-registry-browse--sort-tags (tags)
  "Sort TAGS by :pushed descending (newest first), fallback to name.
When no entry has :pushed, sort name-only with `--tag-cmp-name'."
  (let ((dated  (seq-filter (lambda (p) (plist-get p :pushed)) tags))
        (undated (seq-filter (lambda (p) (not (plist-get p :pushed))) tags)))
    (if dated
        (append
         (sort (copy-sequence dated)
               (lambda (a b) (string> (or (plist-get a :pushed) "")
                                      (or (plist-get b :pushed) ""))))
         (sort (copy-sequence undated)
               (lambda (a b)
                 (container-registry-browse--tag-cmp-name
                  (plist-get a :tag) (plist-get b :tag)))))
      (sort (copy-sequence tags)
            (lambda (a b)
              (container-registry-browse--tag-cmp-name
               (plist-get a :tag) (plist-get b :tag)))))))

;;; ----------------------------------------------------------------- results buffer

(defvar-local container-registry-browse--current-query nil)
(defvar-local container-registry-browse--current-registry-id nil)
(defvar-local container-registry-browse--current-rows nil)

(defvar container-registry-browse-results-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")      #'container-registry-browse-results-show-tags)
    (define-key map (kbd "<return>") #'container-registry-browse-results-show-tags)
    (define-key map "g" #'container-registry-browse-results-refresh)
    (define-key map "c" #'container-registry-browse-results-copy-ref)
    (define-key map "o" #'container-registry-browse-results-open-doc)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `container-registry-browse-results-mode'.")

;; In doom/evil-collection, tabulated-list-mode buffers land in evil's
;; `normal' state, where RET (`evil-ret') and `g' (the gg/gd prefix)
;; are bound by evil itself and shadow our mode-map.  Mirror the keys
;; under `'normal' state so they win.  Use `evil-define-key*' (the
;; immediate, non-deferred variant) to match the pattern in `sf.el'.
(with-eval-after-load 'evil
  (evil-define-key* 'normal container-registry-browse-results-mode-map
    (kbd "RET")      #'container-registry-browse-results-show-tags
    (kbd "<return>") #'container-registry-browse-results-show-tags
    "g"              #'container-registry-browse-results-refresh
    "c"              #'container-registry-browse-results-copy-ref
    "o"              #'container-registry-browse-results-open-doc
    "q"              #'quit-window))

(defun container-registry-browse--stars-sort (a b)
  (< (string-to-number (aref (cadr a) 2))
     (string-to-number (aref (cadr b) 2))))

(defun container-registry-browse--pulls-sort (a b)
  (< (string-to-number (aref (cadr a) 3))
     (string-to-number (aref (cadr b) 3))))

(define-derived-mode container-registry-browse-results-mode tabulated-list-mode
  "container-results"
  "Major mode for container registry search results."
  (setq tabulated-list-format
        `[("Registry"   18 t)
          ("Repo"       42 t)
          ("Stars"       7 ,#'container-registry-browse--stars-sort :right-align t)
          ("Pulls"      10 ,#'container-registry-browse--pulls-sort :right-align t)
          ("Description" 0 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Stars" . t))
  (tabulated-list-init-header))

(defun container-registry-browse--results-row (idx row)
  (list idx
        (vector
         (propertize (or (plist-get row :registry-name)
                         (plist-get row :host)
                         "")
                     'face 'container-registry-browse-registry-face)
         (propertize (or (plist-get row :repo) "")
                     'face 'container-registry-browse-repo-face)
         (propertize (container-registry-browse--format-int (plist-get row :stars))
                     'face 'container-registry-browse-meta-face)
         (propertize (container-registry-browse--format-int (plist-get row :pulls))
                     'face 'container-registry-browse-meta-face)
         (propertize (truncate-string-to-width
                      (or (plist-get row :description) "") 200 nil nil "…")
                     'face 'container-registry-browse-description-face))))

(defun container-registry-browse--row-at-point ()
  (let ((idx (tabulated-list-get-id)))
    (or (and (numberp idx) (nth idx container-registry-browse--current-rows))
        (user-error "No row at point — press g to refresh"))))

(defun container-registry-browse-results-show-tags ()
  "Open a tags buffer for the repository at point."
  (interactive)
  (let* ((row (container-registry-browse--row-at-point)))
    (container-registry-browse--show-tags
     (plist-get row :host) (plist-get row :repo))))

(defun container-registry-browse-results-copy-ref ()
  "Copy `host/repo' for the row at point to the kill ring."
  (interactive)
  (let* ((row (container-registry-browse--row-at-point))
         (ref (format "%s/%s" (plist-get row :host) (plist-get row :repo))))
    (kill-new ref)
    (message "%s" ref)))

(defun container-registry-browse-results-open-doc ()
  "Open the registry's documentation URL for the row at point."
  (interactive)
  (let* ((row (container-registry-browse--row-at-point))
         (reg (container-registry-browse--registry-by-host (plist-get row :host)))
         (url (and reg (plist-get reg :doc-url))))
    (if url (browse-url url)
      (user-error "No doc-url registered for %s" (plist-get row :host)))))

(defun container-registry-browse-results-refresh ()
  "Re-run the search that produced this results buffer."
  (interactive)
  (cond
   (container-registry-browse--current-registry-id
    (container-registry-browse--run-search
     container-registry-browse--current-registry-id
     container-registry-browse--current-query))
   (container-registry-browse--current-query
    (container-registry-browse-search-all
     container-registry-browse--current-query))
   (t (user-error "Nothing to refresh"))))

(defun container-registry-browse--render-results (registry-id query rows)
  (let ((buf (get-buffer-create
              (format "*container-registry: %s %s*"
                      (if registry-id (symbol-name registry-id) "all")
                      query))))
    (with-current-buffer buf
      (unless (derived-mode-p 'container-registry-browse-results-mode)
        (container-registry-browse-results-mode))
      (setq container-registry-browse--current-query query
            container-registry-browse--current-registry-id registry-id
            container-registry-browse--current-rows rows
            tabulated-list-entries
            (cl-loop for r in rows for i from 0
                     collect (container-registry-browse--results-row i r)))
      (tabulated-list-print t))
    (pop-to-buffer buf)
    (message "container-registry-browse: %d result%s"
             (length rows) (if (= 1 (length rows)) "" "s"))))

(defun container-registry-browse--run-search (registry-id query)
  (let* ((reg (or (container-registry-browse--registry-by-id registry-id)
                  (user-error "Unknown registry: %s" registry-id)))
         (fn  (or (plist-get reg :search-fn)
                  (user-error "Registry %s does not support search"
                              (plist-get reg :name))))
         (raw (funcall fn query))
         (rows (mapcar
                (lambda (r) (plist-put r :registry-name (plist-get reg :name)))
                raw)))
    (container-registry-browse--render-results registry-id query rows)))

;;; ----------------------------------------------------------------- tags buffer

(defvar-local container-registry-browse--current-host nil)
(defvar-local container-registry-browse--current-repo nil)
(defvar-local container-registry-browse--current-tags nil)
(defvar-local container-registry-browse--tag-limit nil)

(defvar container-registry-browse-tags-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET")      #'container-registry-browse-tags-inspect)
    (define-key map (kbd "<return>") #'container-registry-browse-tags-inspect)
    (define-key map "p" #'container-registry-browse-tags-copy-podman)
    (define-key map "d" #'container-registry-browse-tags-copy-docker)
    (define-key map "c" #'container-registry-browse-tags-copy-ref)
    (define-key map "y" #'container-registry-browse-tags-copy-tag)
    (define-key map "g" #'container-registry-browse-tags-refresh)
    (define-key map "G" #'container-registry-browse-tags-load-all)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `container-registry-browse-tags-mode'.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal container-registry-browse-tags-mode-map
    (kbd "RET")      #'container-registry-browse-tags-inspect
    (kbd "<return>") #'container-registry-browse-tags-inspect
    "p"              #'container-registry-browse-tags-copy-podman
    "d"              #'container-registry-browse-tags-copy-docker
    "c"              #'container-registry-browse-tags-copy-ref
    "y"              #'container-registry-browse-tags-copy-tag
    "g"              #'container-registry-browse-tags-refresh
    "G"              #'container-registry-browse-tags-load-all
    "q"              #'quit-window))

(define-derived-mode container-registry-browse-tags-mode tabulated-list-mode
  "container-tags"
  "Major mode for browsing tags of a container repository."
  (setq tabulated-list-format
        [("Tag"    40 t)
         ("Pushed" 12 t)
         ("Size"    8 nil :right-align t)
         ("Digest"  0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun container-registry-browse--tag-row (idx tag)
  (list idx
        (vector
         (propertize (or (plist-get tag :tag) "")
                     'face 'container-registry-browse-tag-face)
         (propertize (container-registry-browse--format-pushed
                      (plist-get tag :pushed))
                     'face 'container-registry-browse-meta-face)
         (propertize (container-registry-browse--format-size
                      (plist-get tag :size))
                     'face 'container-registry-browse-meta-face)
         (propertize (let ((d (or (plist-get tag :digest) "")))
                       (if (> (length d) 19) (substring d 0 19) d))
                     'face 'container-registry-browse-meta-face))))

(defun container-registry-browse--tag-at-point ()
  (let ((idx (tabulated-list-get-id)))
    (or (and (numberp idx) (nth idx container-registry-browse--current-tags))
        (user-error "No tag at point"))))

(defun container-registry-browse--current-ref-string (&optional with-tag)
  (let ((tag (and with-tag (plist-get (container-registry-browse--tag-at-point) :tag))))
    (if tag
        (format "%s/%s:%s"
                container-registry-browse--current-host
                container-registry-browse--current-repo tag)
      (format "%s/%s"
              container-registry-browse--current-host
              container-registry-browse--current-repo))))

(defun container-registry-browse-tags-copy-podman ()
  "Copy `podman pull host/repo:tag' to the kill ring."
  (interactive)
  (let ((cmd (format "podman pull %s"
                     (container-registry-browse--current-ref-string t))))
    (kill-new cmd)
    (message "%s" cmd)))

(defun container-registry-browse-tags-copy-docker ()
  "Copy `docker pull host/repo:tag' to the kill ring."
  (interactive)
  (let ((cmd (format "docker pull %s"
                     (container-registry-browse--current-ref-string t))))
    (kill-new cmd)
    (message "%s" cmd)))

(defun container-registry-browse-tags-copy-ref ()
  "Copy `host/repo:tag' to the kill ring."
  (interactive)
  (let ((ref (container-registry-browse--current-ref-string t)))
    (kill-new ref)
    (message "%s" ref)))

(defun container-registry-browse-tags-copy-tag ()
  "Copy the tag name at point to the kill ring."
  (interactive)
  (let ((tag (plist-get (container-registry-browse--tag-at-point) :tag)))
    (kill-new tag)
    (message "%s" tag)))

(defun container-registry-browse-tags-inspect ()
  "Inspect the manifest for the tag at point via skopeo."
  (interactive)
  (container-registry-browse--inspect-buffer
   (container-registry-browse--current-ref-string t)))

(defun container-registry-browse-tags-refresh ()
  "Re-fetch tags using the current limit."
  (interactive)
  (container-registry-browse--show-tags
   container-registry-browse--current-host
   container-registry-browse--current-repo
   container-registry-browse--tag-limit))

(defun container-registry-browse-tags-load-all ()
  "Re-render the current tag list without the row cap."
  (interactive)
  (container-registry-browse--show-tags
   container-registry-browse--current-host
   container-registry-browse--current-repo
   most-positive-fixnum))

(defun container-registry-browse--show-tags (host repo &optional limit)
  (let* ((reg (or (container-registry-browse--registry-by-host host)
                  (list :host host :tags-fn 'skopeo :name host :id (intern host))))
         (raw (container-registry-browse--call-tags-fn reg repo))
         (sorted (container-registry-browse--sort-tags raw))
         (cap (or limit container-registry-browse-tag-limit))
         (truncated (if (and (numberp cap) (> (length sorted) cap))
                        (cl-subseq sorted 0 cap)
                      sorted))
         (buf (get-buffer-create
               (format "*container-tags: %s/%s*" host repo))))
    (with-current-buffer buf
      (unless (derived-mode-p 'container-registry-browse-tags-mode)
        (container-registry-browse-tags-mode))
      (setq container-registry-browse--current-host host
            container-registry-browse--current-repo repo
            container-registry-browse--current-tags truncated
            container-registry-browse--tag-limit cap
            tabulated-list-entries
            (cl-loop for tg in truncated for i from 0
                     collect (container-registry-browse--tag-row i tg)))
      (tabulated-list-print t))
    (pop-to-buffer buf)
    (message "container-registry-browse: %d tag%s%s"
             (length truncated)
             (if (= 1 (length truncated)) "" "s")
             (if (> (length sorted) (length truncated))
                 (format " (of %d — press G to load all)" (length sorted))
               ""))))

;;; ----------------------------------------------------------------- inspect buffer

(defun container-registry-browse--inspect-buffer (ref)
  (let* ((raw (container-registry-browse--skopeo-run
               "inspect" (format "docker://%s" ref)))
         (buf (get-buffer-create (format "*container-inspect: %s*" ref))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert raw)
        (goto-char (point-min))
        (when (fboundp 'json-pretty-print-buffer)
          (ignore-errors (json-pretty-print-buffer)))
        (goto-char (point-min))
        (cond
         ((fboundp 'json-ts-mode) (ignore-errors (json-ts-mode)))
         ((fboundp 'js-json-mode) (ignore-errors (js-json-mode)))
         ((fboundp 'json-mode)    (ignore-errors (json-mode)))
         (t (fundamental-mode))))
      (setq buffer-read-only t)
      (local-set-key "q" #'quit-window))
    (pop-to-buffer buf)))

;;; ----------------------------------------------------------------- interactive entry points

(defvar container-registry-browse--ref-history nil)
(defvar container-registry-browse--query-history nil)

(defun container-registry-browse--read-registry (only-searchable)
  (let* ((candidates (if only-searchable
                         (container-registry-browse--searchable-registries)
                       container-registry-browse--registries))
         (rows (mapcar
                (lambda (cell)
                  (cons (format "%-26s  %s"
                                (plist-get (cdr cell) :name)
                                (plist-get (cdr cell) :host))
                        (car cell)))
                candidates))
         (choice (completing-read "Registry: " (mapcar #'car rows) nil t)))
    (or (cdr (assoc choice rows))
        (user-error "No registry chosen"))))

;;;###autoload
(defun container-registry-browse-search (registry-id query)
  "Search REGISTRY-ID for QUERY, showing results in a buffer."
  (interactive
   (let* ((id (container-registry-browse--read-registry 'only-searchable))
          (reg (container-registry-browse--registry-by-id id))
          (q  (read-string (format "Search %s for: " (plist-get reg :name))
                           nil 'container-registry-browse--query-history)))
     (list id q)))
  (container-registry-browse--run-search registry-id query))

;;;###autoload
(defun container-registry-browse-search-all (query)
  "Search every searchable registry for QUERY into one results buffer."
  (interactive
   (list (read-string "Search all registries for: "
                      nil 'container-registry-browse--query-history)))
  (let ((rows
         (cl-loop for cell in (container-registry-browse--searchable-registries)
                  for reg = (cdr cell)
                  for batch = (condition-case err
                                  (funcall (plist-get reg :search-fn) query)
                                (error
                                 (message "container-registry-browse: %s search failed: %s"
                                          (plist-get reg :name)
                                          (error-message-string err))
                                 nil))
                  nconc (mapcar
                         (lambda (r)
                           (plist-put r :registry-name (plist-get reg :name)))
                         batch))))
    (container-registry-browse--render-results nil query rows)))

;;;###autoload
(defun container-registry-browse-tags (ref)
  "List tags for REF (a `host/repo' image reference)."
  (interactive
   (list (read-string "Image (host/repo): "
                      nil 'container-registry-browse--ref-history)))
  (let* ((parsed (container-registry-browse--parse-ref ref))
         (host (plist-get parsed :host))
         (repo (plist-get parsed :repo)))
    (when (or (null host) (string-empty-p host)
              (null repo) (string-empty-p repo))
      (user-error "Invalid image reference: %s (expected host/repo)" ref))
    (container-registry-browse--show-tags host repo)))

;;;###autoload
(defun container-registry-browse-inspect (ref)
  "Inspect the manifest for REF (`host/repo[:tag]')."
  (interactive
   (list (read-string "Image (host/repo[:tag]): "
                      nil 'container-registry-browse--ref-history)))
  (container-registry-browse--inspect-buffer ref))

;;; ----------------------------------------------------------------- keybindings

(map! :leader
      (:prefix ("s c" . "container registries")
       :desc "Search registry"   "s" #'container-registry-browse-search
       :desc "Search all"        "S" #'container-registry-browse-search-all
       :desc "List tags"         "t" #'container-registry-browse-tags
       :desc "Inspect manifest"  "i" #'container-registry-browse-inspect))


(provide 'container-registry-browse)
;;; container-registry-browse.el ends here
