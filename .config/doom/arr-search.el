;;; arr-search.el --- Search and add across *arr services -*- lexical-binding: t; -*-

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

;; Query any configured *arr service's metadata-provider via its
;; /<resource>/lookup endpoint, then add a picked result to the library.
;; Supports Sonarr (series), Radarr (movie), and Lidarr (artist) out of
;; the box; Readarr (book) is wired but has not been exercised by the
;; author — adjust `arr-search--resource-config' if Readarr's add body
;; needs tweaks.

;;; Code:

(require 'tabulated-list)
(require 'seq)
(require 'subr-x)
(require 'arr)

(defgroup arr-search nil
  "Search and add for *arr services."
  :group 'arr
  :prefix "arr-search-")

(defcustom arr-search-buffer-name "*arr-search*"
  "Buffer name used by `arr-search'."
  :type 'string
  :group 'arr-search)

(defvar-local arr-search--service nil
  "Service symbol for this search buffer.")
(defvar-local arr-search--query nil
  "Query string used for this search buffer.")
(defvar-local arr-search--results nil
  "Raw lookup results (list of alists) for the current buffer.")

;;; ---------------------------------------------------------------------
;;; Per-resource configuration

(defun arr-search--resource-config (resource)
  "Return a plist of search/add config for RESOURCE symbol.

Keys:
  :lookup-path    — lookup endpoint (below :api-path).
  :add-path       — library-add endpoint.
  :title-key      — alist key that holds the display title.
  :year-key       — optional key for the year column (may be nil).
  :extra-col      — (HEADER . ALIST-KEY) for the third column.
  :add-body-fn    — function (SVC RESULT QP-ID RF-PATH &optional META-ID)
                    returning the add-body alist."
  (pcase resource
    ('series
     `(:lookup-path "/series/lookup"
       :add-path    "/series"
       :title-key   title
       :year-key    year
       :extra-col   ("Network" . network)
       :add-body-fn arr-search--sonarr-add-body))
    ('movie
     `(:lookup-path "/movie/lookup"
       :add-path    "/movie"
       :title-key   title
       :year-key    year
       :extra-col   ("Studio" . studio)
       :add-body-fn arr-search--radarr-add-body))
    ('artist
     `(:lookup-path "/artist/lookup"
       :add-path    "/artist"
       :title-key   artistName
       :year-key    nil
       :extra-col   ("Type" . artistType)
       :add-body-fn arr-search--lidarr-add-body))
    ('book
     `(:lookup-path "/book/lookup"
       :add-path    "/book"
       :title-key   title
       :year-key    nil
       :extra-col   ("Author" . authorTitle)
       :add-body-fn arr-search--readarr-add-body))
    (_ (user-error "arr-search: unsupported resource `%s'" resource))))

;;; ---------------------------------------------------------------------
;;; Add-body builders

(defun arr-search--sonarr-add-body (_svc result qp-id rf-path &optional _meta-id)
  `((title            . ,(alist-get 'title result))
    (tvdbId           . ,(alist-get 'tvdbId result))
    (year             . ,(alist-get 'year result))
    (qualityProfileId . ,qp-id)
    (rootFolderPath   . ,rf-path)
    (monitored        . t)
    (seasonFolder     . t)
    (addOptions       . ((searchForMissingEpisodes   . t)
                         (ignoreEpisodesWithFiles    . :json-false)
                         (ignoreEpisodesWithoutFiles . :json-false)))))

(defun arr-search--radarr-add-body (_svc result qp-id rf-path &optional _meta-id)
  `((title               . ,(alist-get 'title result))
    (tmdbId              . ,(alist-get 'tmdbId result))
    (year                . ,(alist-get 'year result))
    (qualityProfileId    . ,qp-id)
    (rootFolderPath      . ,rf-path)
    (monitored           . t)
    (minimumAvailability . "announced")
    (addOptions          . ((searchForMovie . t)))))

(defun arr-search--lidarr-add-body (_svc result qp-id rf-path &optional meta-id)
  `((artistName        . ,(alist-get 'artistName result))
    (foreignArtistId   . ,(alist-get 'foreignArtistId result))
    (qualityProfileId  . ,qp-id)
    (metadataProfileId . ,meta-id)
    (rootFolderPath    . ,rf-path)
    (monitored         . t)
    (addOptions        . ((searchForMissingAlbums . t)
                          (monitor                . "all")))))

(defun arr-search--readarr-add-body (_svc result qp-id rf-path &optional meta-id)
  `((title             . ,(alist-get 'title result))
    (foreignBookId     . ,(alist-get 'foreignBookId result))
    (qualityProfileId  . ,qp-id)
    (metadataProfileId . ,meta-id)
    (rootFolderPath    . ,rf-path)
    (monitored         . t)
    (addOptions        . ((searchForNewBook . t)))))

;;; ---------------------------------------------------------------------
;;; Pickers (quality profile / root folder / metadata profile)

(defun arr-search--pick-from (prompt items name-key)
  "completing-read PROMPT over ITEMS keyed by NAME-KEY; return the picked item."
  (unless items (user-error "%s — no options returned by the service" prompt))
  (let* ((names (mapcar (lambda (it) (alist-get name-key it)) items))
         (choice (completing-read prompt names nil t)))
    (or (seq-find (lambda (it) (equal (alist-get name-key it) choice)) items)
        (user-error "arr-search: picked item not found: %s" choice))))

(defun arr-search--pick-quality-profile (svc)
  (arr-search--pick-from
   (format "[%s] Quality profile: " svc)
   (arr-get svc "/qualityprofile")
   'name))

(defun arr-search--pick-root-folder (svc)
  (arr-search--pick-from
   (format "[%s] Root folder: " svc)
   (arr-get svc "/rootfolder")
   'path))

(defun arr-search--pick-metadata-profile (svc)
  "Only relevant for Lidarr / Readarr."
  (arr-search--pick-from
   (format "[%s] Metadata profile: " svc)
   (arr-get svc "/metadataprofile")
   'name))

(defun arr-search--needs-metadata-profile-p (resource)
  (memq resource '(artist book)))

;;; ---------------------------------------------------------------------
;;; Rendering

(defun arr-search--row (result idx cfg)
  "Build a tabulated-list row for RESULT at IDX, using CFG."
  (let* ((title-key (plist-get cfg :title-key))
         (year-key  (plist-get cfg :year-key))
         (extra     (plist-get cfg :extra-col))
         (title     (or (alist-get title-key result) ""))
         (year      (if year-key
                        (let ((y (alist-get year-key result)))
                          (cond ((numberp y) (format "%d" y))
                                ((stringp y) (substring y 0 (min 4 (length y))))
                                (t "")))
                      ""))
         (extra-val (let ((v (alist-get (cdr extra) result)))
                      (cond ((stringp v) v)
                            ((numberp v) (format "%s" v))
                            (t "")))))
    (list idx (vector year extra-val title))))

(defun arr-search--render ()
  (let* ((svc      arr-search--service)
         (plist    (arr--service-get svc))
         (resource (plist-get plist :resource))
         (cfg      (arr-search--resource-config resource))
         (extra    (plist-get cfg :extra-col))
         (rows     (let ((i 0))
                     (mapcar (lambda (r)
                               (prog1 (arr-search--row r i cfg)
                                 (setq i (1+ i))))
                             arr-search--results))))
    (setq tabulated-list-format
          (vector `("Year" 5 t)
                  `(,(car extra) 14 t)
                  `("Title" 0 t)))
    (setq tabulated-list-entries rows)
    (tabulated-list-init-header)
    (tabulated-list-print)))

;;; ---------------------------------------------------------------------
;;; Mode

(defvar arr-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "a" #'arr-search-add-at-point)
    (define-key map "g" #'arr-search-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'arr-search-show-keys)
    map)
  "Keymap for `arr-search-mode'.")

(define-derived-mode arr-search-mode tabulated-list-mode "arr-search"
  "Major mode for *arr search results."
  (setq tabulated-list-padding 1))

;;; ---------------------------------------------------------------------
;;; Actions

(defun arr-search--result-at-point ()
  (let ((idx (tabulated-list-get-id)))
    (or (and (numberp idx) (nth idx arr-search--results))
        (user-error "No result at point"))))

(defun arr-search-add-at-point ()
  "Add the result at point to the current service's library."
  (interactive)
  (let* ((svc      arr-search--service)
         (plist    (arr--service-get svc))
         (resource (plist-get plist :resource))
         (cfg      (arr-search--resource-config resource))
         (result   (arr-search--result-at-point))
         (title    (alist-get (plist-get cfg :title-key) result))
         (qp       (arr-search--pick-quality-profile svc))
         (rf       (arr-search--pick-root-folder svc))
         (meta-id  (when (arr-search--needs-metadata-profile-p resource)
                     (alist-get 'id (arr-search--pick-metadata-profile svc))))
         (build    (plist-get cfg :add-body-fn))
         (body     (funcall build svc result
                            (alist-get 'id qp)
                            (alist-get 'path rf)
                            meta-id)))
    (when (yes-or-no-p (format "Add %s to %s? " title svc))
      (arr-post-json svc (plist-get cfg :add-path) body)
      (message "Added %s" title))))

(defun arr-search-refresh ()
  "Re-run the current search."
  (interactive)
  (when (and arr-search--service arr-search--query)
    (arr-search arr-search--service arr-search--query)))

(defun arr-search-show-keys ()
  "Display a help buffer listing `arr-search-mode' keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*arr-search-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "arr-search Keybindings\n"
              "======================\n\n"
              "  a    Add the result at point to the library\n"
              "  g    Re-run the search\n"
              "  S    Sort by column at point (tabulated-list)\n"
              "  q    Quit window\n"
              "  ?    This help\n"))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*arr-search-keys*"))

;;; ---------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun arr-search (&optional service query)
  "Search SERVICE for QUERY and display results.
Prompts for both when called interactively."
  (interactive)
  (let* ((svc (or service (arr-pick-service "Search which *arr: ")))
         (q   (or query (read-string (format "[%s] Search: " svc))))
         (plist    (arr--service-get svc))
         (resource (plist-get plist :resource))
         (cfg      (arr-search--resource-config resource))
         (path     (format "%s?term=%s"
                           (plist-get cfg :lookup-path)
                           (url-hexify-string q)))
         (results  (or (arr-get svc path) '()))
         (buf (get-buffer-create arr-search-buffer-name)))
    (with-current-buffer buf
      (arr-search-mode)
      (setq arr-search--service svc
            arr-search--query q
            arr-search--results results)
      (arr-search--render)
      (setq mode-name (format "arr-search[%s]" svc)))
    (pop-to-buffer buf)
    (message "arr-search[%s]: %d result%s for %S"
             svc (length results)
             (if (= 1 (length results)) "" "s")
             q)))

(provide 'arr-search)
;;; arr-search.el ends here
