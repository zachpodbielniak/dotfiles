;;; receipt-print.el --- Print buffers, regions and files to a thermal receipt printer -*- lexical-binding: t; -*-

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

;; A thin Emacs layer over the `receipt-print' shell script in
;; ~/.dotfiles/bin/scripts, which speaks ESC/POS to an Epson TM-T20IV-SP over
;; raw TCP.  Everything interesting -- code page transliteration, indent-aware
;; wrapping, ANSI stripping, framing, cutting -- lives in the script; this file
;; only decides *what* to send and hands it over on stdin.
;;
;; The two headline commands both operate on the region when one is active and
;; on the whole buffer otherwise:
;;
;;   receipt-print            print, leave the paper uncut
;;   receipt-print-and-cut    print and cut the paper off
;;
;; Leaving the paper attached by default means several small prints can share a
;; strip, which is usually what you want when dumping snippets; cut when you
;; actually want to tear one off.
;;
;; Entry points (under SPC C):
;;   receipt-print              p  region or buffer
;;   receipt-print-and-cut      P  region or buffer, then cut
;;   receipt-print-file         f  a file, or the marked files in dired
;;   receipt-print-org-subtree  s  the org subtree at point
;;   receipt-print-qr           q  a QR code
;;   receipt-print-barcode      b  a barcode
;;   receipt-print-cut          F  feed and cut, nothing else
;;   receipt-print-status       ?  online / paper state

;;; Code:

(require 'subr-x)

;; Forward declarations for functions owned by lazily loaded packages.
(declare-function dired-get-marked-files "dired")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-subtree "org")

(defgroup receipt-print nil
  "Print to an ESC/POS thermal receipt printer."
  :group 'applications
  :prefix "receipt-print-")

(defcustom receipt-print-program "receipt-print"
  "Name of the receipt-print executable, found on `exec-path'."
  :type 'string
  :group 'receipt-print)

(defcustom receipt-print-host nil
  "Printer hostname or IP, or nil to use the script's own default."
  :type '(choice (const :tag "Script default" nil) string)
  :group 'receipt-print)

(defcustom receipt-print-port nil
  "Raw ESC/POS port, or nil to use the script's own default (9100)."
  :type '(choice (const :tag "Script default" nil) integer)
  :group 'receipt-print)

(defcustom receipt-print-font 'b
  "Built-in printer font used for the body text.
Font A prints 42 columns and is easier to read at arm's length; font B
prints 56 and suits code and tables.  `auto' lets the script choose from
the longest line in the text."
  :type '(choice (const :tag "Font A (42 columns)" a)
                 (const :tag "Font B (56 columns)" b)
                 (const :tag "Pick automatically" auto))
  :group 'receipt-print)

(defcustom receipt-print-width nil
  "Column width override, or nil to take it from `receipt-print-font'."
  :type '(choice (const :tag "From font" nil) integer)
  :group 'receipt-print)

(defcustom receipt-print-codepage "cp1252"
  "Code page the script transliterates to before printing.
cp1252 keeps curly quotes, em dashes and accented Latin as native glyphs;
cp437 trades those for native box-drawing characters."
  :type '(choice (const "cp1252") (const "cp437") (const "cp858"))
  :group 'receipt-print)

(defcustom receipt-print-reflow 'auto
  "Whether to rejoin hard-wrapped source lines into paragraphs before wrapping.
Notes wrapped at 70 columns for a screen otherwise print as a ragged mix
of the file's breaks and the printer's.

`auto' passes --no-reflow for `prog-mode' buffers, where joining logical
lines would destroy the code, and otherwise lets the script decide from
the text itself.  `always' and `never' force the matter."
  :type '(choice (const :tag "Prose yes, code no" auto)
                 (const :tag "Always reflow" always)
                 (const :tag "Never reflow" never))
  :group 'receipt-print)

(defcustom receipt-print-include-title t
  "When non-nil, print the buffer or file name as a heading."
  :type 'boolean
  :group 'receipt-print)

(defcustom receipt-print-include-timestamp t
  "When non-nil, print a timestamp under the heading."
  :type 'boolean
  :group 'receipt-print)

(defcustom receipt-print-confirm-lines 100
  "Ask for confirmation before printing more than this many lines.
Thermal paper is finite and a stray \\[receipt-print] in a large buffer is
an expensive mistake.  Set to nil to never ask."
  :type '(choice (const :tag "Never ask" nil) integer)
  :group 'receipt-print)

;;; ------------------------------------------------------ process plumbing

(defun receipt-print--require-program ()
  "Signal a user-error unless `receipt-print-program' is on PATH."
  (unless (executable-find receipt-print-program)
    (user-error
     "receipt-print needs `%s' on PATH.  It lives in ~/.dotfiles/bin/scripts; run `just stow'"
     receipt-print-program)))

(defun receipt-print--reflow-args ()
  "Reflow flags for the current buffer, per `receipt-print-reflow'."
  (pcase receipt-print-reflow
    ('always (list "--reflow"))
    ('never (list "--no-reflow"))
    (_ (when (derived-mode-p 'prog-mode) (list "--no-reflow")))))

(defun receipt-print--common-args (&optional cut title)
  "Build the argument list shared by every command.
CUT non-nil cuts the paper.  TITLE, when non-nil and
`receipt-print-include-title' is set, is printed as a heading."
  (append
   (when receipt-print-host (list "--host" receipt-print-host))
   (when receipt-print-port (list "--port" (number-to-string receipt-print-port)))
   (list "--font" (symbol-name receipt-print-font)
         "--codepage" receipt-print-codepage)
   (when receipt-print-width
     (list "--width" (number-to-string receipt-print-width)))
   (receipt-print--reflow-args)
   (list (if cut "--cut" "--no-cut"))
   (when (and title receipt-print-include-title) (list "--title" title))
   (when (and title receipt-print-include-timestamp) (list "--timestamp"))))

(defun receipt-print--run (beg end args)
  "Send the text between BEG and END to the printer with ARGS.
BEG may be nil, in which case nothing is piped on stdin.  Returns non-nil
on success and signals a user-error carrying the script's stderr otherwise."
  (receipt-print--require-program)
  (let ((errfile (make-temp-file "receipt-print-err-")))
    (unwind-protect
        (let ((code (if beg
                        (apply #'call-process-region beg end
                               receipt-print-program nil (list nil errfile) nil args)
                      (apply #'call-process
                             receipt-print-program nil (list nil errfile) nil args))))
          (unless (eq code 0)
            (user-error "receipt-print failed: %s"
                        (with-temp-buffer
                          (insert-file-contents errfile)
                          (let ((msg (string-trim (buffer-string))))
                            (if (string-empty-p msg)
                                (format "exit status %s" code)
                              msg)))))
          t)
      (when (file-exists-p errfile)
        (delete-file errfile)))))

(defun receipt-print--confirm-size (beg end)
  "Ask before printing an unreasonable amount of paper between BEG and END.
Signals a quit if the answer is no."
  (when receipt-print-confirm-lines
    (let ((lines (count-lines beg end)))
      (when (and (> lines receipt-print-confirm-lines)
                 (not (y-or-n-p (format "Print %d lines to the receipt printer? " lines))))
        (user-error "Aborted")))))

(defun receipt-print--send-region (beg end cut title)
  "Print BEG..END, cutting when CUT, headed by TITLE."
  (receipt-print--confirm-size beg end)
  (receipt-print--run beg end (receipt-print--common-args cut title))
  (deactivate-mark)
  (message "receipt-print: sent %d lines (%d chars)%s"
           (count-lines beg end) (- end beg)
           (if cut ", cut" "")))

(defun receipt-print--title-for-buffer ()
  "Heading describing the current buffer, noting an active region."
  (concat (buffer-name) (if (use-region-p) " (region)" "")))

;;; ------------------------------------------------------ commands

;;;###autoload
(defun receipt-print (beg end)
  "Print the region if one is active, otherwise the whole buffer.
The paper is left uncut so several prints can share a strip; use
\\[receipt-print-and-cut] to cut it off."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (receipt-print--send-region beg end nil (receipt-print--title-for-buffer)))

;;;###autoload
(defun receipt-print-and-cut (beg end)
  "Print the region if one is active, otherwise the whole buffer, then cut."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (receipt-print--send-region beg end t (receipt-print--title-for-buffer)))

;;;###autoload
(defun receipt-print-file (files &optional no-cut)
  "Print FILES, cutting the paper afterwards.
Interactively, use the marked files in dired, or prompt for one file.
With a prefix argument NO-CUT, leave the paper uncut."
  (interactive
   (list (if (derived-mode-p 'dired-mode)
             (dired-get-marked-files)
           (list (read-file-name "Print file: " nil nil t)))
         current-prefix-arg))
  (unless files (user-error "No files to print"))
  (dolist (file files)
    (unless (file-readable-p file)
      (user-error "Cannot read %s" file))
    (receipt-print--run nil nil
                        (append (receipt-print--common-args
                                 (not no-cut) (file-name-nondirectory file))
                                (list (expand-file-name file)))))
  (message "receipt-print: sent %d file%s"
           (length files) (if (= (length files) 1) "" "s")))

;;;###autoload
(defun receipt-print-org-subtree (&optional cut)
  "Print the org subtree at point.  With a prefix argument CUT, cut the paper."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point))
          (end (save-excursion (org-end-of-subtree t t) (point))))
      (receipt-print--send-region
       beg end cut
       (buffer-substring-no-properties
        (line-beginning-position) (line-end-position))))))

;;;###autoload
(defun receipt-print-qr (text &optional no-cut)
  "Print TEXT as a QR code, cutting the paper afterwards.
Interactively, use the region when one is active, otherwise prompt with
any URL at point as the default.  With a prefix argument NO-CUT, leave
the paper uncut."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "QR contents: " (thing-at-point 'url t)))
         current-prefix-arg))
  (when (string-empty-p (string-trim text))
    (user-error "Nothing to encode"))
  (receipt-print--run nil nil
                      (append (receipt-print--common-args (not no-cut) nil)
                              (list "--qr" text)))
  (deactivate-mark)
  (message "receipt-print: sent QR code (%d chars)" (length text)))

;;;###autoload
(defun receipt-print-barcode (text &optional choose-type)
  "Print TEXT as a barcode and cut the paper.
With a prefix argument CHOOSE-TYPE, prompt for the barcode symbology
instead of using CODE128."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Barcode contents: "))
         current-prefix-arg))
  (when (string-empty-p (string-trim text))
    (user-error "Nothing to encode"))
  (let ((type (if choose-type
                  (completing-read "Barcode type: "
                                   '("code128" "code39" "code93" "ean13" "ean8"
                                     "upca" "upce" "itf" "codabar")
                                   nil t nil nil "code128")
                "code128")))
    (receipt-print--run nil nil
                        (append (receipt-print--common-args t nil)
                                (list "--barcode" text "--barcode-type" type)))
    (deactivate-mark)
    (message "receipt-print: sent %s barcode" type)))

;;;###autoload
(defun receipt-print-cut ()
  "Feed and cut the paper without printing anything."
  (interactive)
  (receipt-print--run nil nil
                      (append (receipt-print--common-args t nil) (list "--cut-only")))
  (message "receipt-print: fed and cut"))

;;;###autoload
(defun receipt-print-status ()
  "Report whether the printer is online and whether it has paper."
  (interactive)
  (receipt-print--require-program)
  (with-temp-buffer
    (let ((code (apply #'call-process receipt-print-program nil t nil
                       (append
                        (when receipt-print-host (list "--host" receipt-print-host))
                        (when receipt-print-port
                          (list "--port" (number-to-string receipt-print-port)))
                        (list "--status")))))
      (let ((out (string-trim (buffer-string))))
        (if (eq code 0)
            (message "%s" (string-join (split-string out "\n" t) "  |  "))
          (user-error "receipt-print: %s" out))))))

;;; ------------------------------------------------------ keybindings

(map! :leader :desc "receipt" "C" nil)

(map! :leader
      (:prefix ("C" . "receipt")
       :desc "Print region or buffer" "p" #'receipt-print
       :desc "Print and cut"          "P" #'receipt-print-and-cut
       :desc "Print file / marks"     "f" #'receipt-print-file
       :desc "Print org subtree"      "s" #'receipt-print-org-subtree
       :desc "Print QR code"          "q" #'receipt-print-qr
       :desc "Print barcode"          "b" #'receipt-print-barcode
       :desc "Feed and cut"           "F" #'receipt-print-cut
       :desc "Printer status"         "?" #'receipt-print-status))

(provide 'receipt-print)
;;; receipt-print.el ends here
