;;; +nano-mu4e.el --- NΛNO headers view for mu4e -*- lexical-binding: t; -*-
;;
;; +nano-mu4e.el - Doom/evil integration for rougier/nano-mu4e
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
;; `nano-mu4e' replaces the tabular mu4e headers view with a thread-centric
;; layout: one subject line per thread, one line per message, optional body
;; preview, and a left gutter carrying the flags and marks.  It works by
;; collapsing `mu4e-headers-fields' down to a single custom `:nano-mu4e'
;; field and advising four mu4e internals.  See `nano-mu4e-mode-on'.
;;
;; Four things need handling on top of a bare `(nano-mu4e-mode)':
;;
;;   1. Threads.  The whole layout is built around them, so
;;      `mu4e-search-threads' must stay on.
;;
;;   2. Navigation.  nano propertizes its lines with `msg' but never sets the
;;      `docid' text property that `mu4e~headers-docid-at-point' reads.  That
;;      breaks `mu4e~headers-move', and with it every command routed through
;;      it — `mu4e-headers-next'/`-prev' and, in the view buffer, C-j / C-k
;;      (`mu4e-view-headers-next'/`-prev').  Overridden below.
;;
;;   3. Evil.  `nano-mu4e-mode' ships a plain minor-mode keymap, which evil
;;      normal state shadows wholesale, so the bindings are re-established
;;      with `evil-define-minor-mode-key' — the plain `evil-define-key*'
;;      does not work for minor modes.
;;
;;   4. Teardown.  `nano-mu4e-mode-off' restores upstream mu4e's default
;;      `mu4e-headers-fields', not Doom's (which adds `:account-stripe' and
;;      swaps `:from' for `:from-or-to').  We snapshot the real pre-nano
;;      state on first activation and restore that instead, via
;;      `+nano-mu4e-toggle'.
;;
;; The fancy glyphs in `nano-mu4e-symbols' and the round tag style need a
;; NERD font v3.0 (oct collection) — Doom's `nerd-icons' fonts cover this.

;;; Code:

(defvar +nano-mu4e-auto-enable t
  "When non-nil, turn on `nano-mu4e-mode' in every mu4e headers buffer.")

(defvar +nano-mu4e--pristine-marks nil
  "Snapshot of `mu4e-marks' taken before nano-mu4e first activated.")

(defvar +nano-mu4e--pristine-headers-fields nil
  "Snapshot of `mu4e-headers-fields' taken before nano-mu4e first activated.")

(use-package! nano-mu4e
  :after mu4e
  :config
  ;; The layout is thread-based; without threading every message renders as
  ;; its own single-message thread and the view is worse than stock mu4e.
  (setq mu4e-search-threads t)

  (setq nano-mu4e-view-style 'regular   ;; simple | regular | compact | boxed
        nano-mu4e-tag-style  'round     ;; regular | square | round (NERD)
        nano-mu4e-msg-preview t         ;; show a body snippet for new mail
        nano-mu4e-msg-preview-func #'nano-mu4e-msg-preview-p)

  (defun +nano-mu4e-init-h ()
    "Snapshot pre-nano mu4e state, then enable `nano-mu4e-mode'.
Added to `mu4e-headers-mode-hook'.  The snapshot is taken once, on the
first headers buffer, so that `+nano-mu4e-toggle' can restore Doom's
configuration rather than upstream mu4e's defaults."
    (unless +nano-mu4e--pristine-marks
      (setq +nano-mu4e--pristine-marks (copy-tree mu4e-marks)
            +nano-mu4e--pristine-headers-fields (copy-tree mu4e-headers-fields)))
    (when (and +nano-mu4e-auto-enable
               (not (bound-and-true-p nano-mu4e-mode)))
      (nano-mu4e-mode 1)))

  (add-hook 'mu4e-headers-mode-hook #'+nano-mu4e-init-h)

  (defun +nano-mu4e-toggle ()
    "Toggle `nano-mu4e-mode', restoring Doom's headers config when off."
    (interactive)
    (if (bound-and-true-p nano-mu4e-mode)
        (progn
          (setq +nano-mu4e-auto-enable nil)
          (nano-mu4e-mode -1)
          ;; `nano-mu4e-mode-off' put upstream's defaults back; swap in the
          ;; values Doom and +mu4e.el actually established.
          (when +nano-mu4e--pristine-marks
            (setq mu4e-marks (copy-tree +nano-mu4e--pristine-marks)
                  mu4e-headers-fields (copy-tree +nano-mu4e--pristine-headers-fields))
            (mu4e-search-rerun)))
      (setq +nano-mu4e-auto-enable t)
      (nano-mu4e-mode 1)))

  (defun +nano-mu4e-toggle-preview ()
    "Toggle the in-headers message body preview."
    (interactive)
    (setq nano-mu4e-msg-preview (not nano-mu4e-msg-preview))
    (nano-mu4e-refresh)
    (message "nano-mu4e preview %s" (if nano-mu4e-msg-preview "on" "off")))

  ;; --- mu4e navigation internals ------------------------------------------
  ;; `mu4e~headers-move' is what `mu4e-headers-next/prev' AND the view
  ;; buffer's `mu4e-view-headers-next/prev' (C-j / C-k) all funnel through.
  ;; It is doubly incompatible with nano's layout:
  ;;
  ;;   1. It navigates with `line-move', but a nano message spans several
  ;;      lines (subject, sender/date, preview) with borders in between, so
  ;;      one line forward is usually not the next message.
  ;;   2. It then reads the docid via `mu4e~headers-docid-at-point', which
  ;;      wants a `docid' text property at `line-beginning-position'. nano
  ;;      propertizes its lines with `msg' only and never sets `docid', so
  ;;      the lookup returns nil even when point *is* on a message, and the
  ;;      entire body of the function is skipped -- nothing moves, nothing
  ;;      opens, and it silently returns nil.
  ;;
  ;; Overriding this one function fixes every caller at once instead of
  ;; rebinding each key. Highlighting goes through `hl-line' directly, since
  ;; `mu4e~headers-highlight' resolves docids through that same missing
  ;; property.
  (defadvice! +nano-mu4e-headers-move-a (fn lines)
    "Move by message rather than by line while `nano-mu4e-mode' is on."
    :around #'mu4e~headers-move
    (if (not (bound-and-true-p nano-mu4e-mode))
        (funcall fn lines)
      (cl-assert (eq major-mode 'mu4e-headers-mode))
      (let ((mover (if (< lines 0) #'nano-mu4e-prev-msg #'nano-mu4e-next-msg))
            (remaining (abs lines))
            (moved t)
            (any nil))
        ;; A partial move still counts: `3 C-j' near the end of the buffer
        ;; should land on, and open, the last message rather than nothing.
        ;; But a move that got nowhere returns nil, so we do not pointlessly
        ;; re-render the message already on screen.
        (while (and moved (> remaining 0))
          (setq moved (funcall mover)
                remaining (1- remaining))
          (when moved (setq any t)))
        (when-let* ((_ any)
                    (msg (mu4e-message-at-point t))
                    (docid (mu4e-message-field msg :docid)))
          ;; keep every window showing the headers buffer in sync
          (walk-windows
           (lambda (win)
             (when (eq (window-buffer win)
                       (mu4e-get-headers-buffer (buffer-name)))
               (set-window-point win (point))))
           nil t)
          (when (and mu4e-headers-open-after-move
                     (window-live-p mu4e~headers-view-win))
            (mu4e-headers-view-message))
          (when hl-line-mode
            (hl-line-highlight))
          docid))))

  ;; The unread- and thread-jumping commands are broken the same way, but via
  ;; a different route: `mu4e-headers-find-if' scans for the invisible
  ;; `mu4e~headers-docid-pre' marker ("\376") that mu4e wraps around each
  ;; docid, and nano never emits it. So the search finds nothing and every
  ;; caller gets nil. This covers `gj'/`gk'/`]]'/`[[' in the headers buffer
  ;; and their `mu4e-view-headers-*' counterparts in the view buffer. nano
  ;; already ships layout-aware equivalents; just dispatch to them. The view
  ;; side opens the message itself, in `mu4e--view-prev-or-next'.
  (defadvice! +nano-mu4e-prev-or-next-unread-a (fn backwards)
    "Use nano's unread navigation while `nano-mu4e-mode' is on."
    :around #'mu4e~headers-prev-or-next-unread
    (if (not (bound-and-true-p nano-mu4e-mode))
        (funcall fn backwards)
      (if backwards (nano-mu4e-prev-unread-msg) (nano-mu4e-next-unread-msg))))

  (defadvice! +nano-mu4e-prev-or-next-thread-a (fn backwards)
    "Use nano's thread navigation while `nano-mu4e-mode' is on."
    :around #'mu4e~headers-prev-or-next-thread
    (if (not (bound-and-true-p nano-mu4e-mode))
        (funcall fn backwards)
      (if backwards (nano-mu4e-prev-thread) (nano-mu4e-next-thread))))

  ;; --- evil ---------------------------------------------------------------
  ;; `nano-mu4e-mode' is a minor mode, so `evil-define-key*' silently does
  ;; nothing here — the minor-mode variant is required.  `j'/`k' are bound
  ;; because evil-collection's own `j'/`k' would otherwise win; they point at
  ;; mu4e's commands, which the advice above made layout-aware, so counts
  ;; (`3j') and open-after-move keep working exactly as in stock mu4e.
  (after! evil
    (evil-define-minor-mode-key 'normal 'nano-mu4e-mode
      "j"                 #'mu4e-headers-next
      "k"                 #'mu4e-headers-prev
      "n"                 #'nano-mu4e-next-unread-msg
      "p"                 #'nano-mu4e-prev-unread-msg
      "}"                 #'nano-mu4e-next-thread
      "{"                 #'nano-mu4e-prev-thread
      "x"                 #'nano-mu4e-mark-execute-all
      "gr"                #'nano-mu4e-rerun
      (kbd "C-l")         #'nano-mu4e-rerun
      (kbd "TAB")         #'nano-mu4e-fold-toggle
      (kbd "<tab>")       #'nano-mu4e-fold-toggle
      (kbd "S-TAB")       #'nano-mu4e-fold-toggle-all
      (kbd "<backtab>")   #'nano-mu4e-fold-toggle-all
      (kbd "<down>")      #'mu4e-headers-next
      (kbd "<up>")        #'mu4e-headers-prev
      (kbd "S-<down>")    #'nano-mu4e-next-thread
      (kbd "S-<up>")      #'nano-mu4e-prev-thread
      (kbd "<mouse-1>")   #'nano-mu4e-mouse-click)

    (evil-define-minor-mode-key 'visual 'nano-mu4e-mode
      "j" #'mu4e-headers-next
      "k" #'mu4e-headers-prev))

  ;; --- localleader (SPC m) ------------------------------------------------
  ;; nano binds `:', `g', `G', `t', `T' and `@' directly; all of those are
  ;; load-bearing in evil, so the non-navigation commands live here instead.
  (map! :map mu4e-headers-mode-map
        :localleader
        (:prefix ("N" . "nano-mu4e")
         "N" #'+nano-mu4e-toggle
         "p" #'+nano-mu4e-toggle-preview
         "v" #'nano-mu4e-view-style-cycle
         "@" #'nano-mu4e-tag-style-cycle
         "r" #'nano-mu4e-refresh
         "R" #'nano-mu4e-rerun
         "u" #'nano-mu4e-mark-as-new
         "t" #'nano-mu4e-toggle-todo-root
         "T" #'nano-mu4e-toggle-todo
         "g" #'nano-mu4e-edit-tags-root
         "G" #'nano-mu4e-edit-tags)))

;;; ------------------------------------------------------ faces (catppuccin-mocha)
;; The upstream defaults inherit from `link'/`shadow'/`error' and lean on
;; `:inverse-video' for the gutter, which reads as harsh blocks against
;; catppuccin's low-contrast base.  Explicit fg/bg pairs instead.

(custom-set-faces!
  ;; structure
  '(nano-mu4e-border            :foreground "#45475a")
  '(nano-mu4e-preview           :foreground "#7f849c" :slant italic)
  ;; thread subject lines
  '(nano-mu4e-title-active      :foreground "#cdd6f4" :weight bold)
  '(nano-mu4e-title-inactive    :foreground "#6c7086" :weight bold)
  ;; message state
  '(nano-mu4e-new               :foreground "#a6e3a1" :weight bold)
  '(nano-mu4e-unread            :foreground "#89b4fa")
  '(nano-mu4e-match             :foreground "#cdd6f4" :weight bold)
  '(nano-mu4e-related           :foreground "#6c7086")
  '(nano-mu4e-draft             :foreground "#f9e2af")
  '(nano-mu4e-flagged           :foreground "#fab387")
  '(nano-mu4e-archived          :foreground "#6c7086")
  '(nano-mu4e-sent              :foreground "#7f849c" :slant italic)
  '(nano-mu4e-system            :foreground "#f38ba8" :weight bold)
  ;; tags
  '(nano-mu4e-tag-active        :foreground "#89dceb" :weight bold)
  '(nano-mu4e-tag-inactive      :foreground "#585b70" :weight bold)
  '(nano-mu4e-todo              :foreground "#f38ba8" :weight bold)
  ;; left gutter
  '(nano-mu4e-gutter-head-active   :foreground "#1e1e2e" :background "#89b4fa" :weight bold)
  '(nano-mu4e-gutter-head-inactive :foreground "#1e1e2e" :background "#585b70")
  '(nano-mu4e-gutter-body          :foreground "#6c7086" :background "#313244")
  '(nano-mu4e-gutter-match         :foreground "#cdd6f4" :background "#313244")
  '(nano-mu4e-gutter-preview       :foreground "#6c7086" :background "#313244")
  '(nano-mu4e-gutter-mark          :foreground "#1e1e2e" :background "#f38ba8" :weight bold))

(provide '+nano-mu4e)
;;; +nano-mu4e.el ends here
