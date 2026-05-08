;;; +reading.el -*- lexical-binding: t; -*-
;;
;; +reading.el - Calendar, EPUB, debugger, markdown rendering, Calibre
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
;; Pile of medium-weight content readers / viewers / exporters.
;; Bundled together because each is small (handful of lines) but the
;; collection is too noisy for config.el:
;;
;;   - calfw         — `SPC o c' calendar
;;   - nov.el        — `.epub' reader (one combined `use-package!' block)
;;   - dape          — GDB UI for C debugging (`SPC d …')
;;   - grip-mode     — live markdown preview via `grip'
;;   - carbon-now-sh — code-screenshot uploader (`SPC c s')
;;   - valign        — pixel-perfect markdown table alignment
;;   - markdown-mode — header scaling, hide-markup, image inline,
;;                     anchor-following hook, RET on links
;;   - rich-text     — `zach/markdown-copy-as-rich-text' /
;;                     `zach/org-copy-as-rich-text' (HTML on clipboard)
;;   - calibredb     — `SPC o b' Calibre library browser

;;; Code:

;;; Calendar keybind (calfw, replaces calendar.vim)
(map! :leader
      :desc "Calendar" "o c" #'+calendar/open-calendar)

;;; EPUB reader (nov.el, replaces epub.nvim)
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file
        (expand-file-name "nov-places" doom-profile-data-dir)
        nov-text-width 80))

;;; Debugger: GDB via dape for C development
(after! dape
  (map! :leader
        :desc "Debug"         "d d" #'dape
        :desc "Breakpoint"    "d b" #'dape-breakpoint-toggle
        :desc "Continue"      "d c" #'dape-continue
        :desc "Step over"     "d n" #'dape-next
        :desc "Step into"     "d i" #'dape-step-in
        :desc "Step out"      "d o" #'dape-step-out
        :desc "Quit debug"    "d q" #'dape-quit))

;;; Live markdown browser preview (replaces markdown-preview.nvim)
(use-package! grip-mode
  :after markdown-mode
  :config
  (setq grip-preview-use-webkit nil)
  (map! :map markdown-mode-map
        :localleader
        :desc "Live preview" "p" #'grip-mode))

;;; Markdown / Org -> rich-text clipboard.  Renders source to HTML and
;;; puts it on the system clipboard tagged text/html, so paste targets
;;; that understand rich text (Slack, Gmail, GitHub web editor, browser
;;; composers) get formatted output without a preview round-trip.
(defun zach/--copy-html-to-clipboard (html)
  "Put HTML on the system clipboard with text/html MIME type."
  (let ((cmd (cond ((and (getenv "WAYLAND_DISPLAY")
                         (executable-find "wl-copy"))
                    '("wl-copy" "--type" "text/html"))
                   ((executable-find "xclip")
                    '("xclip" "-selection" "clipboard" "-t" "text/html"))
                   (t (user-error "Need wl-copy or xclip on PATH")))))
    (with-temp-buffer
      (insert html)
      (apply #'call-process-region (point-min) (point-max)
             (car cmd) nil nil nil (cdr cmd)))
    (message "Copied %d bytes as rich text" (length html))))

(defun zach/markdown-copy-as-rich-text (beg end)
  "Convert region (or whole buffer) markdown to HTML, copy as rich text."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (unless (executable-find "pandoc")
    (user-error "pandoc not on PATH"))
  (let* ((src  (buffer-substring-no-properties beg end))
         (html (with-temp-buffer
                 (insert src)
                 (unless (zerop (call-process-region
                                 (point-min) (point-max)
                                 "pandoc" t t nil
                                 "-f" "gfm" "-t" "html"))
                   (error "pandoc failed: %s" (buffer-string)))
                 (buffer-string))))
    (zach/--copy-html-to-clipboard html)))

(defun zach/org-copy-as-rich-text (beg end)
  "Convert region (or whole buffer) org to HTML, copy as rich text.
Uses org's built-in HTML exporter -- no pandoc dependency."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (require 'ox-html)
  (let ((html (org-export-string-as
               (buffer-substring-no-properties beg end)
               'html t)))
    (zach/--copy-html-to-clipboard html)))

(after! markdown-mode
  (map! :map markdown-mode-map
        :localleader
        :desc "Copy as rich text" "y" #'zach/markdown-copy-as-rich-text))

(after! org
  (map! :map org-mode-map
        :localleader
        :desc "Copy as rich text" "y" #'zach/org-copy-as-rich-text))

;;; Code screenshots (replaces carbon-now.nvim)
(use-package! carbon-now-sh
  :defer t
  :config
  (map! :leader
        :desc "Carbon screenshot" "c s" #'carbon-now-sh))

;;; Pixel-perfect table alignment (replaces render-markdown.nvim table rendering)
;;; Aligns table columns visually using display properties — works even with
;;; variable-width fonts or CJK characters.
(use-package! valign
  :hook (markdown-mode . valign-mode)
  :config
  (setq valign-fancy-bar t))  ;; render | as a continuous vertical bar

;;; Markdown rendering (replaces render-markdown.nvim + glow.nvim)
(after! markdown-mode
  ;; Display inline images on open (delayed to avoid breaking buffer display)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (run-with-idle-timer 0.5 nil #'markdown-display-inline-images)
              ;; Refresh images after save to pick up newly added image links
              (add-hook 'after-save-hook #'markdown-display-inline-images nil t)))
  (setq markdown-command "glow"
        ;; Scale headers like render-markdown.nvim
        markdown-header-scaling t
        markdown-header-scaling-values '(1.6 1.4 1.2 1.1 1.0 1.0)
        ;; Hide markup characters (* for bold, _ for italic, etc.)
        markdown-hide-markup t
        ;; Render bold/italic/code inline
        markdown-fontify-code-blocks-natively t
        ;; Enable wiki-style links
        markdown-enable-wiki-links t
        ;; Use checkboxes for lists
        markdown-list-indent-width 4
        ;; Display images inline
        markdown-display-remote-images t
        markdown-max-image-size '(800 . 600)
        ;; Render horizontal rules as a line across the buffer
        markdown-hr-display-char ?─)

  ;; Better header faces with catppuccin-friendly colors
  (custom-set-faces!
    '(markdown-header-face-1 :height 1.6 :weight bold)
    '(markdown-header-face-2 :height 1.4 :weight bold)
    '(markdown-header-face-3 :height 1.2 :weight bold)
    '(markdown-header-face-4 :height 1.1 :weight bold)
    ;; Style table faces to match catppuccin
    '(markdown-table-face :inherit fixed-pitch))

  ;; Handle #anchor fragment links (markdown-toc TOC entries)
  ;; markdown-mode silently drops fragment-only URLs, so intercept them
  ;; via the markdown-follow-link-functions hook before that happens.
  (defun zach-markdown-follow-anchor (url)
    "Jump to the heading matching a #fragment anchor URL.
Return t if handled, nil to fall through to default behaviour."
    (when (and url (string-prefix-p "#" url))
      (let* ((slug (substring url 1))
             (found nil))
        (save-excursion
          (goto-char (point-min))
          (while (and (not found)
                      (re-search-forward markdown-regex-header nil t))
            (let* ((heading (or (match-string-no-properties 1)
                                (match-string-no-properties 5)))
                   (heading-slug
                    (when heading
                      (thread-last heading
                        (downcase)
                        (replace-regexp-in-string "[^a-z0-9 -]" "")
                        (string-trim)
                        (replace-regexp-in-string " +" "-")))))
              (when (string= heading-slug slug)
                (setq found (point))))))
        (when found
          (goto-char found)
          (beginning-of-line)
          (recenter 4)
          t))))

  (add-hook 'markdown-follow-link-functions #'zach-markdown-follow-anchor)

  ;; RET follows links (TOC anchors, file links, URLs) in normal mode
  (map! :map markdown-mode-map
        :n "RET" #'markdown-follow-thing-at-point))

;;; Calibre library browser
(use-package! calibredb
  :commands (calibredb)
  :config
  (setq calibredb-program "flatpak run --command=calibredb com.calibre_ebook.calibre"
        calibredb-root-dir "~/Documents/E-Books"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Documents/E-Books")))
  (map! :leader
        :desc "Calibre library" "o b" #'calibredb)
  (map! :map calibredb-search-mode-map
        :n "RET" #'calibredb-find-file
        :n "V"   #'calibredb-open-file-with-default-tool
        :n "v"   #'calibredb-view
        :n "?"   #'calibredb-dispatch
        :n "/"   #'calibredb-search-live-filter
        :n "r"   #'calibredb-search-refresh-and-clear-filter
        :n "m"   #'calibredb-mark-and-forward
        :n "u"   #'calibredb-unmark-and-forward
        :n "q"   #'calibredb-search-quit)
  (map! :map calibredb-show-mode-map
        :n "?"   #'calibredb-entry-dispatch
        :n "q"   #'calibredb-search-quit))

(provide '+reading)
;;; +reading.el ends here
