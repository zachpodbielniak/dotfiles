;;; +org-timeblock.el -*- lexical-binding: t; -*-
;;
;; +org-timeblock.el - Org-timeblock subsystem (transparent SVG, evil bindings)
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
;; Interactive multi-day timeblock view configuration.  Pulled out of
;; the larger org-mode block (`+org.el') because it's substantial
;; (~300 lines) and largely independent — it advises `svg-create' /
;; `svg-image', injects a custom faces/keymaps, and ships its own
;; help command.
;;
;; Two non-trivial subsystems live here:
;;
;;   1. SVG transparency masking — make the org-timeblock SVG respect
;;      the frame's `alpha-background' so the wallpaper shows through
;;      empty grid cells.  Implemented as three coordinated advice
;;      hooks plus a dynamic flag (`zach/org-timeblock--injecting').
;;      See the inline commentary for the gory details.
;;
;;   2. Cursor suppression — Evil's `evil-refresh-cursor' clobbers
;;      `org-timeblock-mode's `(setq cursor-type nil)', re-painting
;;      the masked SVG with a bright accent color.  Fixed by zeroing
;;      every per-state cursor variable buffer-locally.
;;
;; Must load AFTER `+org.el' — references org-timeblock-mode-map
;; and depends on `:after org' from `(use-package! org-timeblock …)'.

;;; Code:

;;; org-timeblock: interactive multi-day timeblock view
(use-package! org-timeblock
  :after org
  :config
  (setq org-timeblock-scale-options nil)
  ;; Show DONE/KILL items so timeblock works like a full calendar.
  ;; The package hardcodes `org-entry-is-done-p' to skip done entries,
  ;; so we advise it to always return nil while gathering entries.
  (defadvice! +org-timeblock--show-done-a (orig-fn &rest args)
    :around #'org-timeblock-get-buffer-entries-all
    (cl-letf (((symbol-function 'org-entry-is-done-p) #'ignore))
      (apply orig-fn args)))
  ;; ─────────────────────────────────────────────────────────────────
  ;; Make the timeblock SVG respect the frame's `alpha-background'
  ;; ─────────────────────────────────────────────────────────────────
  ;;
  ;; Problem.  `org-timeblock-mode' renders its multi-day view as a
  ;; single inline SVG.  The rest of the frame is alpha-transparent
  ;; (see `alpha-background' setup earlier in this file), but the SVG
  ;; area shows up as an opaque matte rectangle — the image's
  ;; transparent pixels are being flattened against a solid colour
  ;; before display, so the compositor never gets per-pixel alpha to
  ;; show the desktop through.
  ;;
  ;; Why.  Emacs's C-level SVG loader (`svg_load_image' in image.c)
  ;; rasterises to a Cairo ARGB32 surface that is pre-filled with a
  ;; solid bg colour (from `:background' if supplied, else
  ;; `face-background', else white) before librsvg paints onto it.
  ;; There is no Lisp-reachable way to skip that pre-fill, so by the
  ;; time the pixmap reaches the compositor the alpha channel is
  ;; already baked opaque.  `alpha-background' only makes Emacs's own
  ;; bg draws transparent — it never touches image pixmaps.
  ;;
  ;; Workaround.  Use the display-time `:mask' property, which builds
  ;; a 1-bit clipping pixmap applied when the image is blit.  Where
  ;; mask = 0, Emacs doesn't write the image pixel — instead it leaves
  ;; whatever was drawn there (the line's face bg), which *is* subject
  ;; to `alpha-background' and therefore does composite through to the
  ;; desktop.  Three moving parts make this stable:
  ;;
  ;;   1. `svg-create' :filter-return — inject a theme-bg `<rect>' as
  ;;      the first child of every SVG built inside
  ;;      `org-timeblock-redraw-timeblocks', giving the rasterised
  ;;      pixmap a known uniform colour at every empty pixel.  Gated
  ;;      by a dynamic flag (see scope-injection below) so this only
  ;;      affects our SVG, not other callers of `svg-create'.
  ;;
  ;;   2. `org-timeblock-redraw-timeblocks' :around — binds the
  ;;      `zach/org-timeblock--injecting' dynamic flag while the
  ;;      redraw runs, so the svg-create filter knows to inject.
  ;;
  ;;   3. `svg-image' :around — adds `:mask (heuristic (R G B))' with
  ;;      the exact theme-bg colour to the image spec.  This is the
  ;;      single choke-point hit by BOTH the initial render (through
  ;;      `svg-insert-image') AND the cursor-move refresh (through
  ;;      `svg-possibly-update-image').  Advising here rather than at
  ;;      the redraw level is what makes the masking stable across
  ;;      cursor movement — a subtle trap discovered the hard way.
  ;;      Gated by identity check `(eq svg org-timeblock-svg)' so
  ;;      other SVGs in the frame are untouched.
  ;;
  ;; Caveat.  `:mask' is 1-bit — anti-aliased edges of geometry don't
  ;; exactly match theme-bg and stay opaque, giving a very faint halo
  ;; against dark themes.  Acceptable trade-off for the transparency.

  (defcustom zach/org-timeblock-theme-bg t
    "When non-nil, make `org-timeblock' SVG respect `alpha-background'.
Injects a theme-colored bg `<rect>' into the SVG and adds a
heuristic clipping mask at display time so transparent regions of
the grid pass the frame's background alpha through to the
compositor.  Toggle to nil to restore stock (opaque matte) rendering."
    :type 'boolean
    :group 'org-timeblock)

  (defvar zach/org-timeblock--injecting nil
    "Dynamic flag — non-nil only while `org-timeblock-redraw-timeblocks'
is running.  The `svg-create' :filter-return advice keys off this to
scope bg-rect injection to our SVG instead of every SVG Emacs ever
creates.")

  (defun zach/org-timeblock--theme-bg-color ()
    "Return the current theme background color string, e.g. \"#1e1e2e\".
Falls back to catppuccin-mocha base if the default face has no
explicit :background set."
    (or (face-attribute 'default :background nil t)
        "#1e1e2e"))

  (defun zach/org-timeblock--scope-injection (orig-fn &rest args)
    "Around-advice for `org-timeblock-redraw-timeblocks'.
Binds `zach/org-timeblock--injecting' while ORIG-FN runs so the
`svg-create' filter fires only for our SVG."
    (let ((zach/org-timeblock--injecting t))
      (apply orig-fn args)))

  (defun zach/org-timeblock--inject-bg-rect (svg)
    "Filter-return advice for `svg-create'.
When invoked inside `org-timeblock-redraw-timeblocks' (flag:
`zach/org-timeblock--injecting'), append a full-coverage theme-bg
`<rect>' as the first child of SVG so every pixel of the rasterized
image has a known uniform colour — the target colour for the
heuristic mask in step 3.  Returns SVG unchanged in every other
context."
    (when (and zach/org-timeblock--injecting
               zach/org-timeblock-theme-bg
               (consp svg))
      (dom-append-child
       svg
       (dom-node 'rect
                 `((x . "0") (y . "0")
                   (width  . "100%")
                   (height . "100%")
                   (fill   . ,(zach/org-timeblock--theme-bg-color))))))
    svg)

  (defun zach/org-timeblock--svg-image-with-mask (orig svg &rest props)
    "Around-advice for `svg-image'.
When SVG is our `org-timeblock-svg', inject
`:mask (heuristic (R G B))' with the theme-bg color into PROPS
before calling ORIG, so Emacs clips theme-bg pixels to transparent
at display time.  Gated by identity check on SVG so every other
caller of `svg-image' in the frame is untouched.  Fires on both
`svg-insert-image' (initial redraw) and `svg-possibly-update-image'
(cursor-move refresh) — advising at the redraw level alone would
leave cursor-move renders unmasked and cause the image to flash
back to matte on every movement."
    (if (and zach/org-timeblock-theme-bg
             (boundp 'org-timeblock-svg)
             org-timeblock-svg
             (eq svg org-timeblock-svg))
        (apply orig svg
               :mask (list 'heuristic
                           (color-values
                            (zach/org-timeblock--theme-bg-color)))
               props)
      (apply orig svg props)))

  (advice-add 'org-timeblock-redraw-timeblocks
              :around #'zach/org-timeblock--scope-injection)
  (advice-add 'svg-create
              :filter-return #'zach/org-timeblock--inject-bg-rect)
  (advice-add 'svg-image
              :around #'zach/org-timeblock--svg-image-with-mask)

  ;; ─────────────────────────────────────────────────────────────────
  ;; Kill the cursor in *org-timeblock*
  ;; ─────────────────────────────────────────────────────────────────
  ;;
  ;; Partner to the masking above.  Without this, the masked SVG
  ;; showed a bright-white accent when focused that vanished on blur.
  ;;
  ;; Why it happened.  `:mask' reveals whatever Emacs drew at that
  ;; pixel position *before* the image draw.  Normally that's the
  ;; line's face bg (alpha-transparent → desktop tint).  But when
  ;; point sits on the character carrying the SVG's `display'
  ;; property, Emacs additionally draws the text cursor AT FULL IMAGE
  ;; SIZE on top of the image, in the cursor face's bg color — a
  ;; rosewater `#f5e0dc' under catppuccin-mocha.  Via
  ;; `alpha-background' that reads as a bright-white accent.  Blur
  ;; stops the selected-window cursor draw, so the accent vanishes
  ;; and only the masked line bg is visible.
  ;;
  ;; Why the mode's own `(setq cursor-type nil)' wasn't enough.
  ;; `org-timeblock-mode' sets `cursor-type nil' in its body (line
  ;; 354 of the package).  But Evil's `evil-refresh-cursor' runs
  ;; right after the mode hook and unconditionally re-assigns
  ;; `cursor-type' from whichever `evil-<STATE>-state-cursor' matches
  ;; the buffer's current state.  For `special-mode'-derived buffers
  ;; Doom lands in `motion' state, whose default cursor is `hollow'.
  ;; So the mode's nil gets silently clobbered.
  ;;
  ;; Fix.  Zero every per-state cursor var buffer-locally and call
  ;; `evil-refresh-cursor' — that removes the rug Evil stands on,
  ;; not just the cursor that's currently on it.  Also defensively
  ;; turn off `hl-line-mode', which would leak its bg the same way
  ;; if some other config ever re-enables it in this buffer.

  (defun zach/org-timeblock--hide-cursor ()
    "Make no cursor of any kind draw in the current buffer.
Buffer-local: zeros `cursor-type' AND every `evil-<STATE>-state-cursor'
variable, then calls `evil-refresh-cursor' to apply immediately.
Also turns off `hl-line-mode' if active.  Required in *org-timeblock*
so Evil's cursor draw doesn't paint over the masked SVG with cursor
face bg; see the commentary above for the full rationale."
    (setq-local cursor-type nil)
    (dolist (var '(evil-normal-state-cursor
                   evil-motion-state-cursor
                   evil-insert-state-cursor
                   evil-visual-state-cursor
                   evil-replace-state-cursor
                   evil-operator-state-cursor
                   evil-emacs-state-cursor))
      (when (boundp var)
        (set (make-local-variable var) nil)))
    (when (fboundp 'evil-refresh-cursor)
      (evil-refresh-cursor))
    (when (bound-and-true-p hl-line-mode)
      (hl-line-mode -1)))

  (add-hook 'org-timeblock-mode-hook #'zach/org-timeblock--hide-cursor)

  ;; Mode hooks don't re-run on `SPC h r r' for already-live buffers,
  ;; so retroactively apply the fix to any *org-timeblock* that is
  ;; already open at config-load time.
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (with-current-buffer buf
                 (derived-mode-p 'org-timeblock-mode)))
      (with-current-buffer buf
        (zach/org-timeblock--hide-cursor))))
  ;; Help command: show keybinding cheatsheet in minibuffer
  (defun org-timeblock-help ()
    "Display org-timeblock Evil keybindings."
    (interactive)
    (let ((help-text
           (concat
            "org-timeblock keybindings:\n"
            "─── Navigation ───────────────────────────\n"
            "  j/k      block down/up    h/l    column left/right\n"
            "  H/L      prev/next day    J      jump to day\n"
            "─── Actions ──────────────────────────────\n"
            "  RET      go to task       go     go to (other window)\n"
            "  s        schedule         d      set duration\n"
            "  t        toggle TODO      a      new task\n"
            "  ci/co    clock in/out\n"
            "─── Marks ────────────────────────────────\n"
            "  m        mark block       u      unmark block\n"
            "  %        mark by regexp   U      unmark all\n"
            "─── View ─────────────────────────────────\n"
            "  v        switch scaling   V      change span\n"
            "  T        toggle list      gr     refresh\n"
            "  W        write/export     q      quit\n"
            "  C-s      save org files   ?      this help")))
      (with-current-buffer (get-buffer-create "*org-timeblock-help*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert help-text)
          (goto-char (point-min))
          (special-mode))
        (display-buffer (current-buffer)
                        '(display-buffer-at-bottom . ((window-height . fit-window-to-buffer)))))))
  ;; Evil-friendly keybindings for the timeblock SVG view
  (evil-define-key* 'normal org-timeblock-mode-map
    ;; navigation: vim-style hjkl
    "j" #'org-timeblock-forward-block
    "k" #'org-timeblock-backward-block
    "h" #'org-timeblock-backward-column
    "l" #'org-timeblock-forward-column
    ;; day navigation
    "H" #'org-timeblock-day-earlier
    "L" #'org-timeblock-day-later
    ;; jump / goto
    "J" #'org-timeblock-jump-to-day
    (kbd "RET") #'org-timeblock-goto
    (kbd "TAB") #'org-timeblock-goto-other-window
    "go" #'org-timeblock-goto-other-window
    ;; actions
    "s" #'org-timeblock-schedule
    "d" #'org-timeblock-set-duration
    "t" #'org-timeblock-todo
    "a" #'org-timeblock-new-task
    ;; clock
    "ci" #'org-timeblock-clock-in
    "co" #'org-clock-out
    ;; marks
    "m" #'org-timeblock-mark-block
    "%" #'org-timeblock-mark-by-regexp
    "u" #'org-timeblock-unmark-block
    "U" #'org-timeblock-unmark-all-blocks
    ;; view
    "v" #'org-timeblock-switch-scaling
    "V" #'org-timeblock-change-span
    "T" #'org-timeblock-toggle-timeblock-list
    "gr" #'org-timeblock-redraw-buffers
    "W" #'org-timeblock-write
    "q" #'org-timeblock-quit
    "?" #'org-timeblock-help
    (kbd "C-s") #'org-save-all-org-buffers)
  ;; Evil-friendly keybindings for the timeblock list sidebar
  (evil-define-key* 'normal org-timeblock-list-mode-map
    "j" #'org-timeblock-list-next-line
    "k" #'org-timeblock-list-previous-line
    "H" #'org-timeblock-day-earlier
    "L" #'org-timeblock-day-later
    "J" #'org-timeblock-jump-to-day
    (kbd "RET") #'org-timeblock-list-goto
    (kbd "TAB") #'org-timeblock-list-goto-other-window
    "go" #'org-timeblock-list-goto-other-window
    "s" #'org-timeblock-list-schedule
    "d" #'org-timeblock-list-set-duration
    "t" #'org-timeblock-todo
    "a" #'org-timeblock-new-task
    "ci" #'org-timeblock-list-clock-in
    "co" #'org-clock-out
    "v" #'org-timeblock-switch-scaling
    "V" #'org-timeblock-change-span
    "T" #'org-timeblock-list-toggle-timeblock
    "gr" #'org-timeblock-redraw-buffers
    "q" #'org-timeblock-quit
    "?" #'org-timeblock-help
    (kbd "C-s") #'org-save-all-org-buffers))

(provide '+org-timeblock)
;;; +org-timeblock.el ends here
