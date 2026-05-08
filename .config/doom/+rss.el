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

;;; RSS feeds (elfeed, replaces feed.nvim)
;;; All 14 feeds from nvim config ported with matching tags
(after! elfeed
  (setq elfeed-feeds
        '(;; Reddit feeds
          ("https://www.reddit.com/r/bash/.rss?sort=new" reddit tech)
          ("https://www.reddit.com/r/C_Programming/.rss?sort=new" reddit tech)
          ("https://www.reddit.com/r/Fedora/.rss?sort=new" reddit tech)
          ("https://www.reddit.com/r/Fire/.rss?sort=new" reddit finance)
          ("https://www.reddit.com/r/dividends/.rss?sort=new" reddit finance investing)
          ;; NewsMax feeds
          ("https://www.newsmax.com/rss/Newsfront/16/" news headlines)
          ("https://www.newsmax.com/rss/US/18/" news)
          ("https://www.newsmax.com/rss/Health-News/177/" news health)
          ("https://www.newsmax.com/rss/Companies/6/" news finance)
          ("https://www.newsmax.com/rss/InvestingAnalysis/17/" news finance investing)
          ("https://www.newsmax.com/rss/Economy/2/" news finance)
          ("https://www.newsmax.com/rss/FinanceNews/4/" news finance)
          ("https://www.newsmax.com/rss/Headline/76/" news finance headlines)
          ;; DailyWire
          ("https://www.dailywire.com/feeds/rss.xml" news))))

;; `o R' was previously bound to `elfeed-update' directly; clear it
;; with an explicit nil so Doom can rebuild it as a prefix.  Mirrors
;; the AI prefix pattern below (`SPC a' nil + flat "a c" sub-keys).
(map! :leader :desc "reddit/rss" "o R" nil)

(map! :leader
      :desc "Open RSS"         "o r"   #'elfeed
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
