;;; man-pager.el --- emacsclient man pager (eman) -*- lexical-binding: t; -*-
;;
;; man-pager.el - Open man pages from emacsclient_tty / eman
;; Copyright (C) 2026  Zach Podbielniak
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Counterpart to `bin/scripts/eman'.  emacsclient --eval's
;; `zach/man-pager' so a shell `man ls' lands in Man-mode through
;; emacsclient_tty (TTY / PGTK / in-Emacs), matching $EDITOR.
;;
;; Dedicated client frames bind q to delete the frame so emacsclient
;; returns to the shell (Doom's Man-mode `q' is `kill-current-buffer',
;; which would leave an empty client frame sitting on the tty).
;;
;; Coloring mirrors nvim `:Man' / `syntax/man.vim':
;;   manHeader         -> Title      (first line)
;;   manSectionHeading -> Statement  (NAME, SYNOPSIS, …)
;;   manSubHeading     -> Function   (indented bold subheads)
;;   manOptionDesc     -> Constant   (-a, --all)
;;   manReference      -> PreProc    (ls(1))
;; Faces inherit font-lock so they follow the Catppuccin flavor.

;;; Code:

(defgroup zach-man-pager nil
  "emacsclient man pager and nvim-like Man-mode coloring."
  :group 'man)

(defface zach-man-header-face
  '((t (:inherit font-lock-type-face :weight bold)))
  "Face for the man page header line (nvim manHeader / Title)."
  :group 'zach-man-pager)

(defface zach-man-footer-face
  '((t (:inherit shadow)))
  "Face for the man page footer line (nvim manFooter)."
  :group 'zach-man-pager)

(defface zach-man-section-face
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for NAME/SYNOPSIS/… headings (nvim manSectionHeading / Statement)."
  :group 'zach-man-pager)

(defface zach-man-subheading-face
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for indented subheadings (nvim manSubHeading / Function)."
  :group 'zach-man-pager)

(defface zach-man-option-face
  '((t (:inherit font-lock-constant-face)))
  "Face for option names (nvim manOptionDesc / Constant)."
  :group 'zach-man-pager)

(defface zach-man-reference-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for name(section) references (nvim manReference / PreProc)."
  :group 'zach-man-pager)

(defvar zach-man-pager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'zach/man-pager-quit)
    map)
  "Keymap for `zach-man-pager-mode'.")

(define-minor-mode zach-man-pager-mode
  "Dedicated emacsclient man pager: q deletes the client frame."
  :lighter " pager"
  :keymap zach-man-pager-mode-map)

(defun zach/man-pager-quit ()
  "Quit the dedicated man pager.
Delete this emacsclient frame so the waiting client returns to the
shell.  Fall back to killing the buffer when this is not a client
frame."
  (interactive)
  (if (frame-parameter nil 'client)
      (delete-frame)
    (kill-current-buffer)))

(with-eval-after-load 'evil
  ;; Immediate (non-deferred): Doom's Man-mode `q' is kill-current-buffer
  ;; on the major-mode map; this minor-mode map must win in the pager.
  (evil-define-key* 'normal zach-man-pager-mode-map
    "q"  #'zach/man-pager-quit
    "ZZ" #'zach/man-pager-quit
    "ZQ" #'zach/man-pager-quit))

(defun zach/man--put-face (beg end face)
  "Put FACE on BEG..END as a `font-lock-face' text property."
  (put-text-property beg end 'font-lock-face face))

(defun zach/man-colorize (&rest _)
  "Color the current Man-mode buffer the way nvim :Man does.

man.el paints headings with `Man-overstrike' (plain bold), so NAME /
SYNOPSIS / OPTIONS do not stand out.  This restyles them after
fontify/unindent, matching nvim runtime `syntax/man.vim'."
  (when (derived-mode-p 'Man-mode)
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t)
          (case-fold-search nil))
      (save-excursion
        (save-restriction
          (widen)
          ;; Header: first line (LS(1)  User Commands  LS(1)).
          (goto-char (point-min))
          (when (looking-at ".+$")
            (zach/man--put-face (match-beginning 0) (match-end 0)
                                'zach-man-header-face))
          ;; Footer: last non-empty line.
          (goto-char (point-max))
          (when (and (eolp) (not (bobp))) (forward-char -1))
          (beginning-of-line)
          (unless (bobp)
            (when (looking-at ".+$")
              (zach/man--put-face (match-beginning 0) (match-end 0)
                                  'zach-man-footer-face)))
          ;; Section headings: NAME, SYNOPSIS, DESCRIPTION, OPTIONS, …
          (goto-char (point-min))
          (while (re-search-forward Man-heading-regexp nil t)
            (zach/man--put-face (match-beginning 0) (match-end 0)
                                'zach-man-section-face))
          ;; Subheadings: indented bold lines that are not options.
          (goto-char (point-min))
          (while (not (eobp))
            (let ((face (get-text-property (point) 'font-lock-face))
                  (indent (current-indentation)))
              (when (and (eq face 'Man-overstrike)
                         (>= indent 3)
                         (not (looking-at "[ \t]*[-+]")))
                (zach/man--put-face (line-beginning-position)
                                    (line-end-position)
                                    'zach-man-subheading-face)))
            (forward-line 1))
          ;; Options: "-a, --all" / "--color[=WHEN]" at the start of a line.
          (goto-char (point-min))
          (while (re-search-forward
                  "^[ \t]+\\(?:[-+][-+[:alnum:]_.=\\[\\]]*\\(?:,[ \t]+\\)?\\)+"
                  nil t)
            (zach/man--put-face (match-beginning 0) (match-end 0)
                                'zach-man-option-face))
          ;; References: ls(1), printf(3), pty(7posix).  Skip header/footer
          ;; / section lines so LS(1) in the title stays Title-colored.
          (goto-char (point-min))
          (while (re-search-forward
                  "\\([A-Za-z0-9:_][-A-Za-z0-9._:]*\\)(\\([0-9][a-zA-Z0-9]*\\|[nlpox]\\))"
                  nil t)
            (unless (memq (get-text-property (match-beginning 0) 'font-lock-face)
                          '(zach-man-header-face
                            zach-man-footer-face
                            zach-man-section-face))
              (zach/man--put-face (match-beginning 0) (match-end 0)
                                  'zach-man-reference-face))))))))

(defun zach/man--apply-builtin-faces (&rest _)
  "Make groff italics render as italic (nvim manItalic), not underline."
  (when (facep 'Man-underline)
    (set-face-attribute 'Man-underline nil :inherit 'italic :underline nil)))

(with-eval-after-load 'man
  (zach/man--apply-builtin-faces)
  ;; After backspace-stripping (cooked) and again after unindent, once
  ;; the buffer is in its final shape.
  (add-hook 'Man-cooked-hook #'zach/man-colorize)
  (advice-add 'Man-unindent :after #'zach/man-colorize))

(when (boundp 'doom-load-theme-hook)
  (add-hook 'doom-load-theme-hook #'zach/man--apply-builtin-faces))

(defun zach/man-pager (args &optional dedicated)
  "Open man page ARGS via `man'.

ARGS is a string as `man' accepts it: \"ls\", \"3 printf\",
\"ls(1)\", \"-k foo\".  When DEDICATED is non-nil this is an
emacsclient pager frame: fill the frame and bind q to delete it."
  (require 'man)
  (let ((Man-notify-method (if dedicated 'bully 'aggressive))
        (Man-prefer-synchronous-call t)
        ;; man.el runs the real man(1) with stdout captured.  Unset
        ;; MANPAGER so a stray `eman --pager' in the daemon env cannot
        ;; recurse if stdout ever looks like a tty.
        (process-environment
         (cons "MANPAGER=cat"
               (cons "EMAN_REAL=1" process-environment))))
    (man args)
    (when dedicated
      (delete-other-windows)
      (zach-man-pager-mode 1))))

(provide 'man-pager)
;;; man-pager.el ends here
