;;; +rss.el -*- lexical-binding: t; -*-
;;
;; +rss.el - RSS feeds (elfeed) and Reddit (reddigg)
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
;; Two related news/discussion subsystems sharing a leader prefix:
;;
;;   `SPC o r'    -> elfeed (RSS reader)
;;   `SPC o R …'  -> reddigg (org-mode-native Reddit browser)
;;
;; reddigg also installs a `browse-url-handlers' entry so reddit.com
;; URLs encountered in eww / elfeed / org links open inside reddigg
;; rather than rendering as a generic web page — mirrors the
;; YouTube → mpv / PDF → pdf-tools pattern in eww.el.

;;; Code:

;;; RSS feeds (elfeed + elfeed-org, replaces feed.nvim)
;;;
;;; The feed list lives in feeds.org next to this file (elfeed-org).
;;; Edit it there; headings inherit tags and elfeed picks changes up on
;;; the next `elfeed' start or `M-x elfeed-org'.
;;;
;;; Reddit RSS is intentionally absent: Reddit ends feed support on
;;; 2026-11-13.  Topic coverage that used to come from subreddits is
;;; handled by hnrss.org keyword feeds below.
;;;
;;; Tags drive the saved filters in `zach/elfeed-filters' (press F in
;;; the search buffer).  Keep every feed tagged with at least one topic.
(defvar rmh-elfeed-org-files nil)
(setq rmh-elfeed-org-files (list (expand-file-name "feeds.org" doom-user-dir)))

(after! elfeed
  (setq elfeed-search-filter "@2-weeks-ago +unread"
        elfeed-search-title-max-width 100)

  ;; elfeed-goodies replaces the stock entry line and drops the date
  ;; column.  This one draws: date | feed | tags | title.
  (defun zach/elfeed-print-entry (entry)
    "Print ENTRY as date, feed, tags, then title."
    (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title (when feed
                         (or (elfeed-meta feed :title) (elfeed-feed-title feed) "")))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (concat "[" (mapconcat #'identity tags ",") "]")))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize (elfeed-format-column feed-title 22 :left)
                          'face 'elfeed-search-feed-face) " ")
      (insert (propertize (elfeed-format-column tags-str 24 :left)
                          'face 'elfeed-search-tag-face) " ")
      (insert (propertize title 'face title-faces 'kbd-help title))))

  (add-hook 'elfeed-search-mode-hook
            (defun zach/elfeed-use-date-column ()
              (setq-local elfeed-search-print-entry-function
                          #'zach/elfeed-print-entry)))

  (defvar zach/elfeed-filters
    '(("everything"  . "@2-weeks-ago +unread")
      ("emacs"       . "@4-weeks-ago +unread +emacs")
      ("opensource"  . "@2-weeks-ago +unread +opensource")
      ("decomp/ports" . "@4-weeks-ago +unread +decomp")
      ("gnu"         . "@4-weeks-ago +unread +gnu")
      ("ai"          . "@2-weeks-ago +unread +ai")
      ("github"      . "@1-week-ago +unread +github")
      ("steam"       . "@2-weeks-ago +unread +steam")
      ("ublue"       . "@4-weeks-ago +unread +ublue")
      ("blogs"       . "@4-weeks-ago +unread +blog")
      ("news"        . "@1-week-ago +unread +news")
      ("starred"     . "+star"))
    "Alist of (NAME . ELFEED-FILTER) for `zach/elfeed-filter'.")

  (defun zach/elfeed-filter ()
    "Pick a saved filter from `zach/elfeed-filters' and apply it."
    (interactive)
    (let* ((name (completing-read "Feed filter: " zach/elfeed-filters nil t))
           (filter (cdr (assoc name zach/elfeed-filters))))
      (elfeed-search-set-filter filter)))

  (map! :map elfeed-search-mode-map
        :n "F" #'zach/elfeed-filter))

(defvar zach/elfeed-stale-seconds 3600
  "Seconds after which opening `zach/elfeed' triggers an update.")

(defvar zach/elfeed--last-update nil
  "Float time of the last update started by `zach/elfeed', or nil.")

(defun zach/elfeed ()
  "Open elfeed, refreshing the feeds first if they are stale.
Updates run asynchronously so the search buffer appears immediately and
fills in as feeds come back.  Use `elfeed-update' (SPC o R u) to force."
  (interactive)
  (elfeed)
  (when (or (null zach/elfeed--last-update)
            (> (- (float-time) zach/elfeed--last-update)
               zach/elfeed-stale-seconds))
    (setq zach/elfeed--last-update (float-time))
    (elfeed-update)))

(defun zach/elfeed-edit-feeds ()
  "Visit the elfeed-org feed list (feeds.org) for editing."
  (interactive)
  (find-file (car rmh-elfeed-org-files)))

;; `o R' was previously bound to `elfeed-update' directly; clear it
;; with an explicit nil so Doom can rebuild it as a prefix.  Mirrors
;; the AI prefix pattern below (`SPC a' nil + flat "a c" sub-keys).
(map! :leader :desc "reddit/rss" "o R" nil)

(map! :leader
      :desc "Open RSS"         "o r"   #'zach/elfeed
      :desc "Edit RSS feeds"   "o F"   #'zach/elfeed-edit-feeds
      :desc "Reddit main"      "o R r" #'reddigg-view-main
      :desc "Reddit subreddit" "o R s" #'reddigg-view-sub
      :desc "Reddit comments"  "o R c" #'reddigg-view-comments
      :desc "Update RSS"       "o R u" #'elfeed-update)

;;; Reddit (reddigg) -- org-mode-native browser
(use-package! reddigg
  :defer t
  :config
  (setq reddigg-subs
        '(bash C_Programming Fedora Fire dividends emacs))

  ;; Route reddit.com URLs from eww / elfeed / org links into reddigg
  ;; instead of rendering as a generic web page. Matches the existing
  ;; YouTube->mpv / PDF->pdf-tools pattern in eww.el.
  (defun zach/browse-url-reddit (url &rest _args)
    "Open reddit URL in reddigg.
Comment threads -> `reddigg-view-comments'; subreddit pages ->
`reddigg-view-sub'; anything else -> eww."
    (cond
     ((string-match "/r/[^/]+/comments/" url)
      (reddigg-view-comments url))
     ((string-match "/r/\\([^/?#]+\\)" url)
      (reddigg-view-sub (match-string 1 url)))
     (t (eww-browse-url url))))
  (function-put 'zach/browse-url-reddit 'browse-url-browser-kind 'internal)

  (add-to-list 'browse-url-handlers
               '("\\(www\\.\\|old\\.\\)?reddit\\.com"
                 . zach/browse-url-reddit)))

(provide '+rss)
;;; +rss.el ends here
