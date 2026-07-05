;;; recipes-export.el --- Styled HTML/PDF export + email for recipes -*- lexical-binding: t; -*-

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

;; Export a recipe to a nicely styled, print-friendly document that looks like
;; Forgejo's light theme (white background, dark text — no ink-heavy dark
;; fills), then optionally attach it to a mu4e email.
;;
;;   recipes-export        (e)  — transient: HTML / PDF / Email
;;   recipes-export-html        — styled standalone HTML (built-in ox-html body
;;                                + our own header/CSS wrap; no pandoc needed)
;;   recipes-export-pdf         — HTML -> PDF via weasyprint (host binary)
;;   recipes-export-email       — export a PDF and start a mu4e compose with it
;;                                attached (compose-mail + mml-attach-file)
;;
;; Works from the browser (row at point) or while visiting a recipe file, via
;; `recipes--current-recipe'.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'xml)

;; Provided by recipes.el (loaded first via config.el).
(declare-function recipes--current-recipe "recipes")
(declare-function recipes--file-metadata "recipes")
(declare-function recipes--file-prop "recipes")
(declare-function recipes--body-lines "recipes")
(declare-function recipes--slugify "recipes")
(declare-function recipes--stars "recipes")
(defvar recipes-mode-map)

(defcustom recipes-export-dir "~/Downloads"
  "Directory where exported recipe HTML/PDF files are written.
Kept under $HOME so the (containerised) toolchain can read/write it."
  :type 'directory
  :group 'recipes)

(defconst recipes-export--css "
:root { color-scheme: light; }
* { box-sizing: border-box; }
html { -webkit-print-color-adjust: exact; }
body {
  margin: 0;
  background: #ffffff;
  color: #1f2328;
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Noto Sans', Helvetica, Arial, sans-serif;
  font-size: 15px;
  line-height: 1.55;
}
main.recipe { max-width: 720px; margin: 0 auto; padding: 2.4rem 2rem 3rem; }
h1 { font-size: 1.9rem; margin: 0 0 .4rem; padding-bottom: .3rem; border-bottom: 1px solid #d1d9e0; line-height: 1.25; }
h2 { font-size: 1.3rem; margin: 1.8rem 0 .6rem; padding-bottom: .25rem; border-bottom: 1px solid #d1d9e0; }
h3 { font-size: 1.08rem; margin: 1.2rem 0 .4rem; }
a { color: #216dca; text-decoration: none; }
a:hover { text-decoration: underline; }
ul, ol { padding-left: 1.5rem; }
li { margin: .2rem 0; }
p { margin: .6rem 0; }
code { background: #f6f8fa; border-radius: 5px; padding: .1rem .35rem; font-family: ui-monospace, 'SFMono-Regular', Menlo, Consolas, monospace; font-size: .9em; }
blockquote { margin: .6rem 0; padding: .2rem .9rem; color: #59636e; border-left: .25rem solid #d1d9e0; }
table { border-collapse: collapse; margin: .8rem 0; width: 100%; font-size: .95em; }
th, td { border: 1px solid #d1d9e0; padding: .4rem .6rem; text-align: left; }
thead th { background: #f6f8fa; }
tr:nth-child(2n) td { background: #f6f8fa; }
.recipe-meta { margin: .6rem 0 .3rem; color: #59636e; font-size: .95rem; }
.recipe-meta .meta { margin-right: 1.1rem; }
.recipe-meta .meta b { color: #1f2328; font-weight: 600; }
.recipe-meta .rating { color: #9a6700; letter-spacing: 1px; }
.recipe-tags { margin: .5rem 0 1rem; }
.recipe-tags .tag { display: inline-block; background: #f6f8fa; border: 1px solid #d1d9e0; border-radius: 999px; padding: .1rem .6rem; margin: 0 .3rem .3rem 0; font-size: .8rem; color: #59636e; }
.recipe-source { margin: .2rem 0 1rem; font-size: .85rem; color: #59636e; }
@page { size: Letter; margin: 1.6cm; }
@media print { main.recipe { max-width: none; padding: 0; } body { font-size: 12pt; } }
"
  "Forgejo/GitHub light-theme CSS embedded into exported recipe documents.")

(defun recipes-export--escape (s)
  "HTML-escape string S (nil becomes empty)."
  (xml-escape-string (or s "")))

(defun recipes-export--org-body (file)
  "Export FILE's recipe body (headings/content only) to HTML via ox-html."
  (require 'ox-html)
  (let ((content (string-join (recipes--body-lines file) "\n")))
    (org-export-string-as
     content 'html t
     ;; :with-broken-links so a cross-recipe id: link (e.g. cheesecake ->
     ;; whipped cream) renders its description instead of aborting the export.
     '(:with-toc nil :section-numbers nil :with-sub-superscript nil
       :with-broken-links t))))

(defun recipes-export--header-html (file m title)
  "Build the styled document header for recipe FILE (metadata M, TITLE)."
  (let* ((esc #'recipes-export--escape)
         (servings (plist-get m :servings))
         (prep (plist-get m :prep))
         (cook (plist-get m :cook))
         (rating (plist-get m :rating))
         (tags (plist-get m :tags))
         (source (recipes--file-prop file "SOURCE"))
         (meta-items (delq nil (list (and servings (cons "Serves" servings))
                                     (and prep (cons "Prep" prep))
                                     (and cook (cons "Cook" cook))))))
    (concat
     (format "<h1>%s</h1>\n" (funcall esc title))
     (when (or meta-items rating)
       (concat "<div class=\"recipe-meta\">"
               (mapconcat (lambda (kv)
                            (format "<span class=\"meta\"><b>%s</b> %s</span>"
                                    (car kv) (funcall esc (cdr kv))))
                          meta-items "")
               (when rating
                 (format "<span class=\"meta rating\">%s</span>" (recipes--stars rating)))
               "</div>\n"))
     (when tags
       (concat "<div class=\"recipe-tags\">"
               (mapconcat (lambda (tg) (format "<span class=\"tag\">%s</span>" (funcall esc tg)))
                          tags "")
               "</div>\n"))
     (when source
       (format "<div class=\"recipe-source\">Source: <a href=\"%s\">%s</a></div>\n"
               (funcall esc source) (funcall esc source))))))

(defun recipes-export--html-string (file)
  "Return a standalone, styled HTML document for recipe FILE."
  (let* ((m (recipes--file-metadata file))
         (title (or (plist-get m :title) (file-name-base file))))
    (concat
     "<!DOCTYPE html>\n<html lang=\"en\"><head>\n<meta charset=\"utf-8\">\n"
     (format "<title>%s</title>\n" (recipes-export--escape title))
     "<style>" recipes-export--css "</style>\n</head><body>\n<main class=\"recipe\">\n"
     (recipes-export--header-html file m title)
     (recipes-export--org-body file)
     "\n</main></body></html>\n")))

(defun recipes-export--out-path (file ext)
  "Absolute output path for FILE with extension EXT under `recipes-export-dir'."
  (let ((title (or (plist-get (recipes--file-metadata file) :title)
                   (file-name-base file))))
    (expand-file-name (concat (recipes--slugify title) "." ext)
                      (expand-file-name recipes-export-dir))))

;;;###autoload
(defun recipes-export-html (&optional file)
  "Write a styled HTML export of the current recipe (or FILE).  Return its path."
  (interactive)
  (let* ((file (or file (recipes--current-recipe)))
         (out (recipes-export--out-path file "html")))
    (make-directory (file-name-directory out) t)
    (with-temp-file out (insert (recipes-export--html-string file)))
    (when (called-interactively-p 'any) (message "Wrote %s" out))
    out))

(defun recipes-export-html-open ()
  "Export the current recipe to HTML and open it in a browser."
  (interactive)
  (browse-url-of-file (recipes-export-html)))

;;;###autoload
(defun recipes-export-pdf (&optional file)
  "Export the current recipe (or FILE) to a Forgejo-light PDF via weasyprint.
Return the PDF path."
  (interactive)
  (unless (executable-find "weasyprint")
    (user-error "weasyprint not found on PATH (pending image rebuild) — use HTML export for now"))
  (let* ((file (or file (recipes--current-recipe)))
         (html (recipes-export--out-path file "html"))
         (pdf (recipes-export--out-path file "pdf")))
    (make-directory (file-name-directory pdf) t)
    (with-temp-file html (insert (recipes-export--html-string file)))
    (let ((code (call-process "weasyprint" nil (get-buffer-create "*recipes-weasyprint*")
                              nil html pdf)))
      (unless (eq code 0)
        (user-error "weasyprint failed — see *recipes-weasyprint*"))
      (when (called-interactively-p 'any) (message "Wrote %s" pdf))
      pdf)))

;;;###autoload
(defun recipes-export-email (&optional file)
  "Export the current recipe (or FILE) to PDF and compose a mu4e mail with it attached."
  (interactive)
  (require 'mml)
  (let* ((file (or file (recipes--current-recipe)))
         (m (recipes--file-metadata file))
         (title (or (plist-get m :title) (file-name-base file)))
         (pdf (recipes-export-pdf file)))
    (compose-mail nil (format "Recipe: %s" title))
    (mml-attach-file pdf "application/pdf"
                     (concat (recipes--slugify title) ".pdf") "attachment")
    (message "Composed email with %s attached" (file-name-nondirectory pdf))))

;;;###autoload (autoload 'recipes-export "recipes-export" nil t)
(transient-define-prefix recipes-export ()
  "Export the current recipe."
  ["Export recipe (Forgejo light theme)"
   ("h" "HTML — styled, open in browser" recipes-export-html-open)
   ("p" "PDF — weasyprint"               recipes-export-pdf)
   ("m" "Email — attach PDF (mu4e)"      recipes-export-email)])

;;; ------------------------------------------------------ keybindings

(define-key recipes-mode-map "e" #'recipes-export)

(with-eval-after-load 'evil
  (evil-define-key* 'normal recipes-mode-map
    "e" #'recipes-export))

(provide 'recipes-export)
;;; recipes-export.el ends here
