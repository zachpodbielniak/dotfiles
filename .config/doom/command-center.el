;;; command-center.el --- Bespoke two-column startup command-center dashboard -*- lexical-binding: t; -*-

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

;; A bespoke "COMMAND CENTER" startup buffer, rendered as a two-column TUI
;; dashboard that replaces the stock Doom splash.
;;
;;   LEFT  : a time-aware greeting, a navigable LAUNCH menu (jump-off actions
;;           with their leader keys), and an AI / CLAUDE block.
;;   RIGHT : a live clock, three progress rings (TASKS done/total · BLOCK
;;           timeblocked hours · NEXT countdown to the next event), then
;;           TODAY (weather + next event), PROJECT (name · git branch),
;;           WORKSPACES, NOW PLAYING (EMMS, only when active), INBOX
;;           (second-brain 00_inbox count), and RECENT files.
;;
;; The rings are drawn as SVG donuts in a graphical frame and degrade to
;; Unicode block bars under `emacs -nw'.  Dispatch is `cc--graphic-p'.
;;
;; Data loads asynchronously: the buffer paints instantly with placeholders,
;; then an idle timer populates the local panels and an async `url-retrieve'
;; fills in the weather.  A 60s timer refreshes the clock and NEXT ring.
;;
;; Entry points:
;;   `command-center'          — open / reopen the dashboard (SPC o D)
;;   `command-center-refresh'  — repaint + re-fetch (g in the buffer)
;;   `cc-initial-buffer'       — returns the buffer for `initial-buffer-choice'
;;
;; In-buffer keys: j/k move the launch highlight, RET fires it, each row's
;; letter is a direct hotkey, g refreshes, q quits.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'svg nil t)
(require 'dom nil t)

;;; ------------------------------------------------------ customization

(defgroup command-center nil
  "Bespoke two-column startup dashboard."
  :group 'applications
  :prefix "command-center-")

(defcustom command-center-buffer-name "*command-center*"
  "Name of the command-center buffer."
  :type 'string
  :group 'command-center)

(defcustom command-center-user-name
  (or (and (stringp user-full-name)
           (not (string-empty-p user-full-name))
           (car (split-string user-full-name)))
      "there")
  "Name used in the greeting line."
  :type 'string
  :group 'command-center)

(defcustom command-center-left-width 52
  "Width, in columns, of the left (launch) column."
  :type 'integer
  :group 'command-center)

(defcustom command-center-ring-size 80
  "Height, in pixels, of the SVG ring band (GUI only)."
  :type 'integer
  :group 'command-center)

(defcustom command-center-clock-interval 60
  "Seconds between clock / NEXT-ring refreshes."
  :type 'integer
  :group 'command-center)

(defcustom command-center-inbox-dir
  (expand-file-name "00_inbox"
                    (or (and (boundp 'org-directory) org-directory)
                        "~/Documents/notes"))
  "Second-brain inbox directory whose note count is shown."
  :type 'directory
  :group 'command-center)

(defcustom command-center-enable-weather t
  "When non-nil, fetch live weather from wttr.in for the TODAY panel."
  :type 'boolean
  :group 'command-center)

(defcustom command-center-weather-location ""
  "wttr.in location segment, e.g. \"Detroit\".  Empty = auto by IP."
  :type 'string
  :group 'command-center)

(defcustom command-center-weather-ttl 1800
  "Seconds to cache a weather result before re-fetching."
  :type 'integer
  :group 'command-center)

(defcustom command-center-workday-hours 8.0
  "Target hours used to scale the BLOCK ring and NEXT horizon."
  :type 'number
  :group 'command-center)

(defcustom command-center-agenda-files
  (let ((dir (expand-file-name "02_areas/org"
                               (or (and (boundp 'org-directory) org-directory)
                                   "~/Documents/notes"))))
    (when (file-directory-p dir)
      (directory-files dir t "\\.org\\'")))
  "Org files scanned for the TASKS / BLOCK / NEXT rings.
Defaults to the small set of dated-task files under 02_areas/org.
Scanning the full `org-agenda-files' set (which is built recursively from
the PARA tree and can hold hundreds of note files) freezes startup, so
keep this list small.  Use the symbol `agenda' to scan all
`org-agenda-files' instead (slow, not recommended with a large tree)."
  :type '(choice (const :tag "All agenda files (slow)" agenda)
                 (repeat file))
  :group 'command-center)

;;; ------------------------------------------------------ faces (catppuccin-mocha)

(defface command-center-title-face
  '((t (:foreground "#cba6f7" :weight bold)))
  "Face for the title bar and clock time." :group 'command-center)

(defface command-center-label-face
  '((t (:foreground "#f9e2af" :weight bold)))
  "Face for section labels (LAUNCH, TODAY, …)." :group 'command-center)

(defface command-center-text-face
  '((t (:foreground "#cdd6f4")))
  "Face for primary body text." :group 'command-center)

(defface command-center-dim-face
  '((t (:foreground "#6c7086" :slant italic)))
  "Face for dim / secondary text." :group 'command-center)

(defface command-center-key-face
  '((t (:foreground "#cba6f7")))
  "Face for right-aligned keybinding hints." :group 'command-center)

(defface command-center-greeting-face
  '((t (:foreground "#cba6f7" :weight bold :height 1.1)))
  "Face for the greeting line." :group 'command-center)

(defface command-center-selected-face
  '((t (:background "#313244" :foreground "#cba6f7" :weight bold :extend t)))
  "Face for the highlighted launch row." :group 'command-center)

;;; ------------------------------------------------------ palette

(defconst cc--colors
  '(:base     "#1e1e2e"
    :surface0 "#313244"
    :surface1 "#45475a"
    :text     "#cdd6f4"
    :subtext  "#a6adc8"
    :overlay  "#6c7086"
    :mauve    "#cba6f7"
    :yellow   "#f9e2af"
    :green    "#a6e3a1"
    :blue     "#89b4fa"
    :peach    "#fab387"
    :red      "#f38ba8")
  "Raw hex palette (catppuccin-mocha) for the SVG builder.")

;;; ------------------------------------------------------ launch actions

(defvar cc--launch-actions
  '((:section launch :key "s" :label "resume session" :key-display "SPC q l" :command doom/quickload-session)
    (:section launch :key "f" :label "find file"      :key-display "SPC ."   :command find-file)
    (:section launch :key "r" :label "recent files"   :key-display "SPC f o" :command recentf-open-files)
    (:section launch :key "p" :label "switch project" :key-display "SPC p p" :command projectile-switch-project)
    (:section launch :key "a" :label "agenda"         :key-display "SPC n a" :command org-agenda)
    (:section launch :key "c" :label "capture"        :key-display "SPC X"   :command org-capture)
    (:section launch :key "t" :label "timeblock"      :key-display "SPC n T" :command org-timeblock)
    (:section launch :key "b" :label "recipes"        :key-display "SPC o k" :command recipes)
    (:section launch :key "m" :label "mail"           :key-display "SPC o m" :command mu4e)
    (:section launch :key "e" :label "rss"            :key-display "SPC o r" :command elfeed)
    (:section launch :key "x" :label "matrix"         :key-display "SPC M M" :command ement-connect)
    (:section ai :key "i" :label "▶ ask claude"      :key-display "SPC a c" :command claude-code-start)
    (:section ai :key "v" :label "send region"        :key-display "SPC a s" :command claude-code-send-region)
    (:section ai :key "o" :label "gptel chat"         :key-display "SPC a g" :command gptel)
    (:section ai :key "u" :label "ai menu"            :key-display "SPC a m" :command gptel-menu))
  "Flat list of selectable dashboard actions.  Position = highlight index.")

;;; ------------------------------------------------------ buffer-local state

(defvar-local cc--state nil
  "Plist of panel data for this dashboard buffer.")
(defvar-local cc--launch-index 0
  "Index into `cc--launch-actions' of the highlighted row.")
(defvar-local cc--launch-overlay nil
  "Overlay drawing the launch-row highlight.")
(defvar-local cc--right-width nil
  "Cached right-column width for the current render.")

(defvar cc--clock-timer nil "Repeating clock/NEXT refresh timer.")
(defvar cc--populate-timer nil "One-shot local-data population timer.")
(defvar cc--weather-cache nil "Cons (EPOCH . SUMMARY) of the last weather fetch.")

;;; ------------------------------------------------------ environment helpers

(defun cc--graphic-p ()
  "Non-nil when SVG rings can be drawn (graphical frame + svg support)."
  (and (display-graphic-p)
       (featurep 'svg)
       (image-type-available-p 'svg)))

(defun cc--theme-bg ()
  "Return the theme background hex, falling back to catppuccin base."
  (let ((bg (face-attribute 'default :background nil t)))
    (if (and (stringp bg) (string-prefix-p "#" bg))
        bg
      (plist-get cc--colors :base))))

(defun cc--window-width ()
  "Body width of the window showing this buffer, or a default."
  (let ((win (get-buffer-window (current-buffer) t)))
    (if win (window-body-width win) 120)))

(defun cc--narrow-p (width)
  "Non-nil when WIDTH is too small for two columns."
  (< width (+ command-center-left-width 28)))

(defun cc--set (key val)
  "Set KEY to VAL in the buffer-local `cc--state'."
  (setq cc--state (plist-put cc--state key val)))

;;; ------------------------------------------------------ data fetch helpers

(defun cc--greeting ()
  "Return a time-of-day greeting word."
  (let ((h (string-to-number (format-time-string "%H"))))
    (cond ((< h 12) "Good morning")
          ((< h 18) "Good afternoon")
          (t        "Good evening"))))

(defun cc--agenda-file-list ()
  "Resolve `command-center-agenda-files' to a concrete list of files."
  (if (eq command-center-agenda-files 'agenda)
      (and (boundp 'org-agenda-files) (listp org-agenda-files) org-agenda-files)
    command-center-agenda-files))

(defun cc--org-file-buffer (file)
  "Return a buffer visiting FILE, opening it with all hooks suppressed.
Reuses an existing buffer if one already visits FILE; otherwise opens a
fresh one without firing `find-file-hook' / `org-mode-hook' so a scan
never drags in org-roam, lsp, or other per-file machinery."
  (or (find-buffer-visiting file)
      (let ((find-file-hook nil)
            (org-mode-hook nil)
            (org-inhibit-startup t))
        (find-file-noselect file t))))

(defun cc--scan-agenda ()
  "Scan `command-center-agenda-files'; return a plist of today's stats.
Keys: :done :total :block-hours :next-secs :next-label."
  (let ((done 0) (total 0) (block-mins 0) next-secs next-label
        (now (float-time))
        (files (cc--agenda-file-list)))
    (when (and files (require 'org nil t))
      (let ((today (org-today)))
        (dolist (file files)
          (when (and (stringp file) (file-exists-p file))
            (ignore-errors
              (with-current-buffer (cc--org-file-buffer file)
                (save-excursion
                 (save-restriction
                  (widen)
                  (goto-char (point-min))
                  (org-map-entries
                  (lambda ()
                    (let ((ts (or (org-entry-get (point) "SCHEDULED")
                                  (org-entry-get (point) "DEADLINE"))))
                      (when ts
                        (let ((tm (org-time-string-to-time ts)))
                          (when (= (time-to-days tm) today)
                            (setq total (1+ total))
                            (when (org-entry-is-done-p) (setq done (1+ done)))
                            (when (string-match
                                   "\\([0-9][0-9]\\):\\([0-9][0-9]\\)-\\([0-9][0-9]\\):\\([0-9][0-9]\\)"
                                   ts)
                              (let ((dur (- (+ (* 60 (string-to-number (match-string 3 ts)))
                                               (string-to-number (match-string 4 ts)))
                                            (+ (* 60 (string-to-number (match-string 1 ts)))
                                               (string-to-number (match-string 2 ts))))))
                                (when (> dur 0) (setq block-mins (+ block-mins dur)))))
                            (when (string-match "[0-9][0-9]:[0-9][0-9]" ts)
                              (let ((start (float-time tm)))
                                (when (and (> start now)
                                           (or (null next-secs) (< (- start now) next-secs)))
                                  (setq next-secs (- start now)
                                        next-label (org-get-heading t t t t))))))))))
                  nil 'file)))))))))
    (list :done done :total total
          :block-hours (/ block-mins 60.0)
          :next-secs next-secs :next-label next-label)))

(defun cc--inbox-count ()
  "Count note files in `command-center-inbox-dir'."
  (ignore-errors
    (when (file-directory-p command-center-inbox-dir)
      (length (directory-files command-center-inbox-dir nil
                               "\\.\\(org\\|md\\|txt\\)\\'" t)))))

(defun cc--recent-files ()
  "Return up to six recent file paths."
  (when (bound-and-true-p recentf-list)
    (seq-take recentf-list 6)))

(defun cc--git-branch ()
  "Return the current git branch of `default-directory' without loading magit."
  (ignore-errors
    (let ((root (locate-dominating-file default-directory ".git")))
      (when root
        (let ((head (expand-file-name ".git/HEAD" root)))
          (when (file-readable-p head)
            (with-temp-buffer
              (insert-file-contents head)
              (when (re-search-forward "ref: refs/heads/\\(.*\\)" nil t)
                (string-trim (match-string 1))))))))))

(defun cc--project-info ()
  "Return (NAME . BRANCH) for the current project, or nil."
  (ignore-errors
    (let* ((branch (cc--git-branch))
           (root (locate-dominating-file default-directory ".git"))
           (name (cond
                  ((and (fboundp 'projectile-project-p)
                        (fboundp 'projectile-project-name)
                        (projectile-project-p))
                   (projectile-project-name))
                  (root (file-name-nondirectory (directory-file-name root)))
                  (t nil))))
      (when (or name branch) (cons (or name "—") branch)))))

(defun cc--workspaces ()
  "Return a one-line summary of persp workspaces, or nil."
  (ignore-errors
    (when (and (fboundp '+workspace-list-names) (bound-and-true-p persp-mode))
      (let ((cur (and (fboundp '+workspace-current-name) (+workspace-current-name)))
            (names (+workspace-list-names)))
        (when (and (listp names) names)
          (mapconcat
           (lambda (n)
             (if (equal n cur)
                 (propertize (concat "● " n) 'face 'command-center-text-face)
               (propertize (concat "○ " n) 'face 'command-center-dim-face)))
           names "  "))))))

(defun cc--now-playing ()
  "Return the currently-playing EMMS track description, or nil."
  (ignore-errors
    (when (and (fboundp 'emms-player-playing-p) (emms-player-playing-p))
      (let ((track (and (fboundp 'emms-playlist-current-selected-track)
                        (emms-playlist-current-selected-track))))
        (when track (emms-track-description track))))))

(defun cc--next-short (secs)
  "Format SECS until the next event compactly."
  (cond ((null secs) "clear")
        ((< secs 3600) (format "%dm" (round (/ secs 60.0))))
        (t (format "%.1fh" (/ secs 3600.0)))))

;;; ------------------------------------------------------ ring rendering

(defun cc--rings ()
  "Return the three ring tuples (FRAC COLOR LABEL VALUE) from state."
  (let* ((ag (plist-get cc--state :agenda))
         (pending (or (null ag) (eq ag :pending)))
         (done (if pending 0 (or (plist-get ag :done) 0)))
         (total (if pending 0 (or (plist-get ag :total) 0)))
         (block (if pending 0.0 (or (plist-get ag :block-hours) 0.0)))
         (next-secs (unless pending (plist-get ag :next-secs)))
         (horizon (* command-center-workday-hours 3600.0)))
    (list
     (list (if (> total 0) (/ (float done) total) 0.0)
           (plist-get cc--colors :green) "TASKS"
           (if pending "…" (format "%d/%d" done total)))
     (list (min 1.0 (/ block (max 1.0 command-center-workday-hours)))
           (plist-get cc--colors :blue) "BLOCK"
           (if pending "…" (format "%.0fh" block)))
     (list (if next-secs (max 0.0 (min 1.0 (- 1.0 (/ next-secs horizon)))) 0.0)
           (plist-get cc--colors :peach) "NEXT"
           (if pending "…" (cc--next-short next-secs))))))

(defun cc--svg-donut (svg cx cy r sw frac color value label)
  "Append one donut (track + arc + VALUE + LABEL) at CX,CY to SVG."
  (let* ((circ (* 2 float-pi r))
         (filled (* (max 0.0 (min 1.0 frac)) circ)))
    (dom-append-child svg
      (dom-node 'circle `((cx . ,(number-to-string cx)) (cy . ,(number-to-string cy))
                          (r . ,(number-to-string r)) (fill . "none")
                          (stroke . ,(plist-get cc--colors :surface1))
                          (stroke-width . ,(number-to-string sw)))))
    (dom-append-child svg
      (dom-node 'circle `((cx . ,(number-to-string cx)) (cy . ,(number-to-string cy))
                          (r . ,(number-to-string r)) (fill . "none")
                          (stroke . ,color)
                          (stroke-width . ,(number-to-string sw))
                          (stroke-linecap . "round")
                          (stroke-dasharray . ,(format "%.3f %.3f" filled (- circ filled)))
                          (transform . ,(format "rotate(-90 %d %d)" cx cy)))))
    (dom-append-child svg
      (dom-node 'text `((x . ,(number-to-string cx)) (y . ,(number-to-string (+ cy 5)))
                        (fill . ,(plist-get cc--colors :text))
                        (text-anchor . "middle") (font-size . "14")
                        (font-family . "monospace") (font-weight . "bold"))
                value))
    (dom-append-child svg
      (dom-node 'text `((x . ,(number-to-string cx))
                        (y . ,(number-to-string (+ cy r sw 8)))
                        (fill . ,(plist-get cc--colors :subtext))
                        (text-anchor . "middle") (font-size . "10")
                        (font-family . "monospace"))
                label))))

(defun cc--svg-ring-band (rings)
  "Return a single propertized string displaying RINGS as one SVG image."
  (let* ((n (length rings))
         (cell 84) (r 26) (sw 7) (cy 30)
         (w (* n cell))
         (h command-center-ring-size)
         (svg (svg-create w h))
         (i 0))
    ;; A known-colour backing rect makes the heuristic mask below clip the
    ;; theme background to transparent, so the image composites over the
    ;; frame's `alpha-background' instead of showing an opaque matte.
    ;; (Same technique as the org-timeblock SVG; see +org-timeblock.el.)
    (dom-append-child svg
      (dom-node 'rect `((x . "0") (y . "0") (width . "100%") (height . "100%")
                        (fill . ,(cc--theme-bg)))))
    (dolist (ring rings)
      (cc--svg-donut svg (+ (/ cell 2) (* i cell)) cy r sw
                     (nth 0 ring) (nth 1 ring) (nth 3 ring) (nth 2 ring))
      (setq i (1+ i)))
    (propertize " " 'display
                (svg-image svg
                           :mask (list 'heuristic (color-values (cc--theme-bg)))
                           :ascent 'center))))

(defun cc--text-bar (frac width)
  "Return a WIDTH-char Unicode bar filled to FRAC (0..1)."
  (let* ((w (or width 10))
         (filled (round (* w (max 0.0 (min 1.0 frac)))))
         (empty (- w filled)))
    (concat (make-string filled ?█) (make-string empty ?░))))

(defun cc--text-ring-line (ring)
  "Render RING as a single `LABEL  bar  VALUE' text line."
  (concat "  "
          (propertize (format "%-6s" (nth 2 ring)) 'face 'command-center-label-face)
          " "
          (propertize (cc--text-bar (nth 0 ring) 10) 'face `(:foreground ,(nth 1 ring)))
          (propertize (format "  %s" (nth 3 ring)) 'face 'command-center-text-face)))

(defun cc--ring-band-lines (rings)
  "Return a list of right-column lines for RINGS (SVG band or text bars)."
  (if (cc--graphic-p)
      (let ((rows (max 1 (ceiling command-center-ring-size
                                  (float (default-line-height))))))
        (cons (concat "  " (cc--svg-ring-band rings))
              (make-list (1- rows) "")))
    (mapcar #'cc--text-ring-line rings)))

;;; ------------------------------------------------------ layout helpers

(defun cc--fit (s width)
  "Pad or truncate display-string S to exactly WIDTH columns."
  (let ((w (string-width s)))
    (cond ((= w width) s)
          ((< w width) (concat s (make-string (- width w) ?\s)))
          (t (truncate-string-to-width s width nil nil "…")))))

(defun cc--row (label key)
  "Render a launch row: LABEL on the left, KEY right-aligned within the column."
  (let* ((label-s (concat "  " label))
         (lw command-center-left-width)
         (pad (max 1 (- lw (string-width label-s) (string-width key) 1))))
    (concat (propertize label-s 'face 'command-center-text-face)
            (make-string pad ?\s)
            (propertize key 'face 'command-center-key-face)
            " ")))

(defun cc--section (title)
  "Render a section header line for TITLE."
  (propertize (concat "  " title) 'face 'command-center-label-face))

(defun cc--action-row (action idx)
  "Render ACTION as a row tagged with highlight index IDX."
  (propertize (cc--row (plist-get action :label) (plist-get action :key-display))
              'cc-index idx))

(defun cc--launch-block ()
  "Return the LAUNCH section as a list of lines."
  (let ((lines (list (cc--section "LAUNCH"))) (i 0))
    (dolist (a cc--launch-actions)
      (when (eq (plist-get a :section) 'launch)
        (push (cc--action-row a i) lines))
      (setq i (1+ i)))
    (nreverse lines)))

(defun cc--ai-block ()
  "Return the AI / CLAUDE section as a list of lines."
  (let ((lines (list (cc--section "AI · CLAUDE"))) (i 0))
    (dolist (a cc--launch-actions)
      (when (eq (plist-get a :section) 'ai)
        (push (cc--action-row a i) lines))
      (setq i (1+ i)))
    (nreverse lines)))

;;; ------------------------------------------------------ right-column panels

(defun cc--format-weather ()
  "Return the weather string for the TODAY panel."
  (let ((w (plist-get cc--state :weather)))
    (cond ((or (null w) (eq w :pending)) "…")
          (t w))))

(defun cc--panel-today ()
  "Return the TODAY panel lines."
  (let* ((ag (plist-get cc--state :agenda))
         (next (when (and (listp ag) (plist-get ag :next-secs))
                 (format "  next: %s — %s"
                         (cc--next-short (plist-get ag :next-secs))
                         (or (plist-get ag :next-label) "")))))
    (delq nil
          (list (cc--section "TODAY")
                (concat "  " (propertize (concat "☀ " (cc--format-weather))
                                         'face 'command-center-text-face))
                (when next (propertize next 'face 'command-center-dim-face))))))

(defun cc--panel-project ()
  "Return the PROJECT panel lines."
  (let ((p (plist-get cc--state :project)))
    (list (cc--section "PROJECT")
          (concat "  " (propertize
                        (if p (concat (car p)
                                      (if (cdr p) (concat " · " (cdr p)) ""))
                          "—")
                        'face 'command-center-text-face)))))

(defun cc--panel-workspaces ()
  "Return the WORKSPACES panel lines."
  (list (cc--section "WORKSPACES")
        (concat "  " (or (plist-get cc--state :workspaces)
                         (propertize "—" 'face 'command-center-dim-face)))))

(defun cc--panel-now-playing ()
  "Return the NOW PLAYING panel lines, or nil when nothing plays."
  (let ((np (plist-get cc--state :now-playing)))
    (when np
      (list (cc--section "NOW PLAYING")
            (concat "  " (propertize (concat "♪ " np) 'face 'command-center-text-face))
            ""))))

(defun cc--panel-inbox ()
  "Return the INBOX panel lines."
  (let ((n (plist-get cc--state :inbox)))
    (list (cc--section "INBOX")
          (concat "  " (propertize
                        (if (numberp n) (format "%d notes to triage" n) "…")
                        'face 'command-center-text-face)))))

(defun cc--panel-recent ()
  "Return the RECENT panel lines."
  (cons (cc--section "RECENT")
        (or (mapcar (lambda (f)
                      (concat "  " (propertize (file-name-nondirectory f)
                                               'face 'command-center-text-face)))
                    (plist-get cc--state :recent))
            (list (concat "  " (propertize "…" 'face 'command-center-dim-face))))))

;;; ------------------------------------------------------ composite render

(defun cc--greeting-line ()
  "Left greeting line."
  (concat "  " (propertize (format "%s, %s" (cc--greeting) command-center-user-name)
                           'face 'command-center-greeting-face)))

(defun cc--greeting-date-line ()
  "Left date line."
  (concat "  " (propertize (format-time-string "%A, %B %-d %Y")
                           'face 'command-center-dim-face)))

(defun cc--clock-time-line ()
  "Right clock time line."
  (concat "  " (propertize (format-time-string "%-I:%M %p")
                           'face 'command-center-title-face)))

(defun cc--clock-date-line ()
  "Right clock date line."
  (concat "  " (propertize (format-time-string "%A, %B %-d")
                           'face 'command-center-dim-face)))

(defun cc--render-left (ring-rows)
  "Return the left column as a list of lines, aligned to RING-ROWS band height."
  (append
   (list (cc--greeting-line) (cc--greeting-date-line) "")
   (make-list ring-rows "")
   (list "")
   (cc--launch-block)
   (list "")
   (cc--ai-block)))

(defun cc--render-right (ring-lines)
  "Return the right column as a list of lines, embedding RING-LINES."
  (append
   (list (cc--clock-time-line) (cc--clock-date-line) "")
   ring-lines
   (list "")
   (cc--panel-today) (list "")
   (cc--panel-project) (list "")
   (cc--panel-workspaces) (list "")
   (cc--panel-now-playing)
   (cc--panel-inbox) (list "")
   (cc--panel-recent)))

(defun cc--compose (left right lw total)
  "Zip LEFT and RIGHT line-lists into a string; LW = left width, TOTAL = window."
  (let* ((rw (max 16 (- total lw 2)))
         (n (max (length left) (length right)))
         (out '()))
    (dotimes (i n)
      (let ((l (cc--fit (or (nth i left) "") lw))
            (r (cc--fit (or (nth i right) "") rw)))
        (push (concat l "  " r) out)))
    (mapconcat #'identity (nreverse out) "\n")))

(defun cc--render-header (width)
  "Render the top title bar to WIDTH columns."
  (let* ((title (concat "  " (propertize "⊙ COMMAND CENTER"
                                         'face 'command-center-title-face)))
         (clock (upcase (format-time-string "%a %b %-d · %-I:%M %p")))
         (pad (max 1 (- width (string-width title) (string-width clock) 2))))
    (concat title (make-string pad ?\s)
            (propertize clock 'face 'command-center-dim-face) "\n"
            (propertize (make-string (max 1 width) ?─) 'face 'command-center-dim-face))))

(defun cc--render-footer (width)
  "Render the footer to WIDTH columns."
  (let* ((path (concat "  " (abbreviate-file-name default-directory)))
         (ver "COMMAND CENTER · v1  ")
         (pad (max 1 (- width (string-width path) (string-width ver)))))
    (concat (propertize (make-string (max 1 width) ?─) 'face 'command-center-dim-face) "\n"
            (propertize path 'face 'command-center-dim-face)
            (make-string pad ?\s)
            (propertize ver 'face 'command-center-dim-face))))

(defun cc--render-stacked ()
  "Single-column fallback for narrow / terminal frames."
  (mapconcat
   #'identity
   (append (list (cc--greeting-line) (cc--greeting-date-line) "")
           (mapcar #'cc--text-ring-line (cc--rings)) (list "")
           (cc--launch-block) (list "")
           (cc--ai-block) (list "")
           (cc--panel-today) (list "")
           (cc--panel-project) (list "")
           (cc--panel-workspaces) (list "")
           (or (cc--panel-now-playing) nil)
           (cc--panel-inbox) (list "")
           (cc--panel-recent))
   "\n"))

(defun cc--render ()
  "Return the full dashboard as a string."
  (let ((width (cc--window-width)))
    (if (cc--narrow-p width)
        (concat (cc--render-header width) "\n"
                (cc--render-stacked) "\n"
                (cc--render-footer width))
      (let* ((ring-lines (cc--ring-band-lines (cc--rings)))
             (rows (length ring-lines))
             (left (cc--render-left rows))
             (right (cc--render-right ring-lines)))
        (concat (cc--render-header width) "\n"
                (cc--compose left right command-center-left-width width) "\n"
                (cc--render-footer width))))))

;;; ------------------------------------------------------ highlight + repaint

(defun cc--apply-highlight ()
  "Move the launch highlight overlay onto the current index's row."
  (let ((pos (text-property-any (point-min) (point-max) 'cc-index cc--launch-index)))
    (when pos
      (save-excursion
        (goto-char pos)
        (let* ((bol (line-beginning-position))
               (end (min (line-end-position) (+ bol command-center-left-width))))
          (if (overlayp cc--launch-overlay)
              (move-overlay cc--launch-overlay bol end)
            (setq cc--launch-overlay (make-overlay bol end)))
          (overlay-put cc--launch-overlay 'face 'command-center-selected-face))))))

(defun cc--repaint ()
  "Erase and re-render the dashboard in place, preserving point + highlight."
  (when (eq major-mode 'command-center-mode)
    (let ((inhibit-read-only t) (pt (point)))
      (when (overlayp cc--launch-overlay) (delete-overlay cc--launch-overlay))
      (setq cc--launch-overlay nil)
      (erase-buffer)
      (setq cc--right-width (cc--window-width))
      (insert (cc--render))
      (goto-char (min pt (point-max)))
      (cc--apply-highlight))))

;;; ------------------------------------------------------ navigation

(defun cc--goto-selected ()
  "Move point to the currently-selected launch row."
  (let ((pos (text-property-any (point-min) (point-max) 'cc-index cc--launch-index)))
    (when pos (goto-char pos))))

(defun cc-launch-next ()
  "Move the launch highlight down."
  (interactive)
  (setq cc--launch-index (mod (1+ cc--launch-index) (max 1 (length cc--launch-actions))))
  (cc--apply-highlight)
  (cc--goto-selected))

(defun cc-launch-prev ()
  "Move the launch highlight up."
  (interactive)
  (setq cc--launch-index (mod (1- cc--launch-index) (max 1 (length cc--launch-actions))))
  (cc--apply-highlight)
  (cc--goto-selected))

(defun cc--run-action (action)
  "Invoke ACTION's command, or report if it is unavailable."
  (let ((cmd (plist-get action :command)))
    (cond ((commandp cmd) (call-interactively cmd))
          ((fboundp cmd) (funcall cmd))
          (t (message "command-center: %s is unavailable" cmd)))))

(defun cc-launch-activate ()
  "Fire the currently-highlighted launch action."
  (interactive)
  (let ((a (nth cc--launch-index cc--launch-actions)))
    (when a (cc--run-action a))))

(defun cc-launch-dispatch ()
  "Fire the launch action whose hotkey matches the pressed key."
  (interactive)
  (let ((ch (char-to-string last-command-event)) (found nil))
    (dolist (a cc--launch-actions)
      (when (and (not found) (equal (plist-get a :key) ch))
        (setq found a)))
    (if found (cc--run-action found)
      (message "command-center: no action for %s" ch))))

;;; ------------------------------------------------------ async population

(defun cc--populate-local (buf)
  "Gather all local panel data for BUF, then repaint."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (cc--set :recent (cc--recent-files))
      (cc--set :project (cc--project-info))
      (cc--set :workspaces (cc--workspaces))
      (cc--set :now-playing (cc--now-playing))
      (cc--set :inbox (cc--inbox-count))
      (cc--set :agenda (cc--scan-agenda))
      (cc--repaint))))

(defun cc--apply-weather (buf summary)
  "Store SUMMARY into BUF's state and repaint."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (cc--set :weather summary)
      (cc--repaint))))

(defun cc--fetch-weather (buf)
  "Fetch weather for BUF asynchronously (honouring the TTL cache)."
  (when command-center-enable-weather
    (if (and cc--weather-cache
             (< (- (float-time) (car cc--weather-cache)) command-center-weather-ttl))
        (cc--apply-weather buf (cdr cc--weather-cache))
      (ignore-errors
        (let ((url-request-extra-headers '(("Accept" . "application/json"))))
          (url-retrieve
           (format "https://wttr.in/%s?format=j1"
                   (or command-center-weather-location ""))
           (lambda (status &rest _)
             (let ((err (plist-get status :error)) (summary nil))
               (unless err
                 (ignore-errors
                   (goto-char (point-min))
                   (when (re-search-forward "\n\n" nil t)
                     (let* ((json (json-parse-buffer :object-type 'alist
                                                     :array-type 'list))
                            (cur (car (alist-get 'current_condition json)))
                            (tempf (alist-get 'temp_F cur))
                            (desc (alist-get 'value
                                             (car (alist-get 'weatherDesc cur)))))
                       (setq summary (string-trim
                                      (format "%s %s°F" (or desc "") (or tempf ""))))))))
               (kill-buffer (current-buffer))
               (setq summary (if (and summary (not (string-empty-p summary)))
                                 summary "weather offline"))
               (setq cc--weather-cache (cons (float-time) summary))
               (cc--apply-weather buf summary)))
           nil t t))))))

(defun cc--tick (buf)
  "Repaint BUF on the clock timer; self-cancel when BUF dies."
  (if (buffer-live-p buf)
      (with-current-buffer buf (cc--repaint))
    (cc--cancel-timers)))

(defun cc--cancel-timers ()
  "Cancel all dashboard timers."
  (when (timerp cc--clock-timer) (cancel-timer cc--clock-timer))
  (when (timerp cc--populate-timer) (cancel-timer cc--populate-timer))
  (setq cc--clock-timer nil cc--populate-timer nil))

(defun cc--schedule (buf)
  "(Re)schedule population + clock timers for BUF."
  (cc--cancel-timers)
  (setq cc--populate-timer (run-with-idle-timer 0.1 nil #'cc--populate-local buf))
  (when command-center-enable-weather
    (run-with-idle-timer 0.3 nil #'cc--fetch-weather buf))
  (setq cc--clock-timer
        (run-with-timer command-center-clock-interval
                        command-center-clock-interval #'cc--tick buf)))

;;; ------------------------------------------------------ cursor hiding

(defun cc--hide-cursor ()
  "Stop any cursor drawing in this buffer so it never paints over the SVG band.
Mirrors the org-timeblock approach: zero `cursor-type' and every Evil
per-state cursor variable buffer-locally, then refresh."
  (setq-local cursor-type nil)
  (dolist (var '(evil-normal-state-cursor evil-motion-state-cursor
                 evil-insert-state-cursor evil-visual-state-cursor
                 evil-replace-state-cursor evil-operator-state-cursor
                 evil-emacs-state-cursor))
    (when (boundp var) (set (make-local-variable var) nil)))
  (when (fboundp 'evil-refresh-cursor) (evil-refresh-cursor))
  (when (bound-and-true-p hl-line-mode) (hl-line-mode -1)))

;;; ------------------------------------------------------ mode

(defvar command-center-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'command-center-refresh)
    (define-key map "q" #'quit-window)
    (define-key map "j" #'cc-launch-next)
    (define-key map "k" #'cc-launch-prev)
    (define-key map (kbd "RET") #'cc-launch-activate)
    (define-key map (kbd "<return>") #'cc-launch-activate)
    (define-key map (kbd "<down>") #'cc-launch-next)
    (define-key map (kbd "<up>") #'cc-launch-prev)
    (dolist (a cc--launch-actions)
      (let ((k (plist-get a :key)))
        (when k (define-key map k #'cc-launch-dispatch))))
    map)
  "Keymap for `command-center-mode'.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal command-center-mode-map
    "g" #'command-center-refresh
    "q" #'quit-window
    "j" #'cc-launch-next
    "k" #'cc-launch-prev
    (kbd "RET") #'cc-launch-activate
    (kbd "<return>") #'cc-launch-activate)
  (dolist (a cc--launch-actions)
    (let ((k (plist-get a :key)))
      (when k (evil-define-key* 'normal command-center-mode-map
                k #'cc-launch-dispatch)))))

(define-derived-mode command-center-mode special-mode "Command-Center"
  "Major mode for the command-center dashboard."
  (setq-local truncate-lines t)
  (setq-local cc--state (list :recent nil :project nil :workspaces nil
                              :now-playing nil :inbox nil
                              :agenda :pending :weather :pending))
  (setq-local cc--launch-index 0)
  (buffer-disable-undo)
  (cc--hide-cursor)
  (add-hook 'kill-buffer-hook #'cc--cancel-timers nil t))

;;; ------------------------------------------------------ entry points

(defun cc--get-buffer ()
  "Create / refresh the dashboard buffer and return it."
  (let ((buf (get-buffer-create command-center-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'command-center-mode) (command-center-mode))
      (cc--repaint)
      (cc--schedule buf))
    buf))

;;;###autoload
(defun command-center ()
  "Open (or reopen) the command-center dashboard."
  (interactive)
  (pop-to-buffer (cc--get-buffer)))

(defun command-center-refresh ()
  "Repaint the dashboard and re-fetch its data."
  (interactive)
  (cc--repaint)
  (let ((buf (current-buffer)))
    (run-with-idle-timer 0.05 nil #'cc--populate-local buf)
    (when command-center-enable-weather
      (run-with-idle-timer 0.1 nil #'cc--fetch-weather buf))))

;;;###autoload
(defun cc-initial-buffer ()
  "Return the command-center buffer, for `initial-buffer-choice'."
  (cc--get-buffer))

(provide 'command-center)
;;; command-center.el ends here
