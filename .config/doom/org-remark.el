;;; org-remark.el -*- lexical-binding: t; -*-
;;
;; org-remark.el - Universal annotation layer for all readable buffers
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
;; `org-remark' (<https://github.com/nobiot/org-remark>) highlights
;; arbitrary regions in any buffer and persists highlights + per-highlight
;; notes into aggregating .org files.  Wired here as a universal
;; annotation layer across everything we read:
;;
;;   Plain text / source code / org      → core org-remark via
;;                                         `org-remark-global-tracking-mode'
;;   Web (eww)                           → `org-remark-eww'
;;   EPUBs (nov)                         → `org-remark-nov'
;;   Info manuals                        → `org-remark-info'
;;
;; Notes live under the PARA tree at
;;   ~/Documents/notes/03_resources/marginalia/
;; with one .org per file-backed source and a single aggregate file
;; per backend (`web.org' / `epub.org' / `info.org' / `misc.org').
;; Because the tree sits inside `$ORG_DIRECTORY', org-ql and
;; org-transclusion index the marginalia automatically.
;;
;; Pens:
;;
;;   yellow    (default)  ad-hoc highlight           `SPC n r m'
;;   line      (default)  mark whole line            `SPC n r l'
;;   green     custom     confirmed / agreed         `SPC n r g'
;;   red       custom     refute / issue / wrong     `SPC n r R'
;;   blue      custom     question / follow-up       `SPC n r b'
;;   important custom     wavy-underline stress mark `SPC n r !'
;;
;; Full keymap under `SPC n r' — see `map!' block at bottom.

;;; Code:

;; Eager require of the main package: it defines `org-remark-mark' /
;; `org-remark-create' / all the other commands we bind below.  If we
;; only required `org-remark-global-tracking' the symbols in our
;; keymap would stay void (global-tracking doesn't `require' main),
;; and every keybinding would trip `commandp'.  Startup cost is
;; negligible (~50 ms) and org-remark is small.
(require 'org-remark)
(require 'org-remark-global-tracking)

;;; ---------------------------------------------------------------------
;;; Notes-file routing

(defun zach/org-remark--notes-file-name ()
  "Route org-remark notes under ~/Documents/notes/03_resources/marginalia/.
Per-source .org for file-backed buffers (so each source gets its own
marginalia file that can be cleanly transcluded or org-ql'd in
isolation).  Per-backend aggregate file for eww / nov / Info (no
meaningful `buffer-file-name', and small-note-per-URL sprawl is
undesirable).  Fallback: `misc.org'."
  (let* ((root (expand-file-name "~/Documents/notes/03_resources/marginalia/"))
         (files-dir (expand-file-name "files/" root)))
    (make-directory root t)
    (cond
     ((derived-mode-p 'eww-mode)  (expand-file-name "web.org"  root))
     ((derived-mode-p 'nov-mode)  (expand-file-name "epub.org" root))
     ((derived-mode-p 'Info-mode) (expand-file-name "info.org" root))
     ((buffer-file-name)
      (make-directory files-dir t)
      (expand-file-name
       ;; Sanitise absolute path -> filename so collisions are
       ;; impossible while the source remains identifiable by eye.
       (concat (replace-regexp-in-string "[/:]" "_" (buffer-file-name))
               ".org")
       files-dir))
     (t (expand-file-name "misc.org" root)))))

(setq org-remark-notes-file-name #'zach/org-remark--notes-file-name)

;;; ---------------------------------------------------------------------
;;; Backend enablement
;;;
;;; Each per-mode extension resolves its own source identifier (URL for
;;; eww, EPUB position for nov, Info node path for Info) so highlights
;;; rehydrate against the same document across sessions.  Must load
;;; after the backend package — `with-eval-after-load' gates that.

(with-eval-after-load 'eww  (require 'org-remark-eww))
(with-eval-after-load 'nov  (require 'org-remark-nov))
(with-eval-after-load 'info (require 'org-remark-info))

;; Enable the BACKEND minor mode in each host, not `org-remark-mode'.
;; The backend modes are what register the source-identifier resolver
;; on `org-remark-source-find-file-name-functions' (URL for eww, EPUB
;; position for nov, Info node path for Info).  Without that hook
;; populated, `org-remark-source-find-file-name' returns nil and
;; `org-remark-mark' bails with "this buffer is not supported".
;; `org-remark-mode' itself gets auto-enabled by `org-remark-mark' on
;; first use (see `org-remark-highlight-mark' in org-remark.el).
(add-hook 'eww-mode-hook  #'org-remark-eww-mode)
(add-hook 'nov-mode-hook  #'org-remark-nov-mode)
(add-hook 'Info-mode-hook #'org-remark-info-mode)

;; Global tracking: when a file-backed buffer is opened, automatically
;; load any saved highlights for it.  No manual `org-remark-mode'
;; toggle needed for ordinary files.
(add-hook 'doom-after-modules-config-hook #'org-remark-global-tracking-mode)

;;; ---------------------------------------------------------------------
;;; Pens (semantic palette matching catppuccin-mocha)
;;;
;;; The built-in yellow pen stays the default — `org-remark-mark' keeps
;;; its out-of-the-box behaviour for unsemantic highlights.  Each
;;; `org-remark-create' call defines both a face AND an interactive
;;; `org-remark-mark-NAME' command, which the keymap below binds.
;;; Foreground is forced to catppuccin base (#1e1e2e) on the solid-bg
;;; pens so text stays legible against the light highlight colour.

(after! org-remark
  (org-remark-create "green"
    '(:background "#a6e3a1" :foreground "#1e1e2e")
    '(CATEGORY "confirmed" help-echo "Confirmed / agree"))
  (org-remark-create "red"
    '(:background "#f38ba8" :foreground "#1e1e2e")
    '(CATEGORY "refute" help-echo "Refute / issue"))
  (org-remark-create "blue"
    '(:background "#89b4fa" :foreground "#1e1e2e")
    '(CATEGORY "question" help-echo "Question / follow up"))
  (org-remark-create "important"
    '(:underline (:color "#f38ba8" :style wave) :weight bold)
    '(CATEGORY "important" help-echo "Important")))

(provide 'org-remark-config)
;;; org-remark.el ends here
