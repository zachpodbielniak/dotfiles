;;; org-roam-dired.el --- Browse the org-roam link graph as dired -*- lexical-binding: t; -*-

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

;; org-roam nodes link to other nodes, which forms a pseudo-filesystem.
;; This module lets you walk that graph the way you walk directories: a
;; real `dired-mode' buffer listing the files the current node links to,
;; annotated with node titles.
;;
;; Because the buffer really is dired, marks, `!', `A', `%', wdired and
;; `dired-get-marked-files' all keep working across a graph neighbourhood.
;;
;;   SPC n g   browse from a node picked by completion
;;   SPC n G   browse from the node/file at point
;;
;; Inside the buffer:  RET / l descend, h / - / ^ back, TAB flips between
;; outbound links and backlinks, o / O open, ? for the full dispatcher.
;;
;; Four things about the surrounding config shape this implementation.
;; They were each verified against the live Emacs, and each one is a bug
;; if you get it wrong:
;;
;;   1. `dirvish-override-dired-mode' is on, and `dirvish-dired-noselect-a'
;;      keys its session buffers by DIRECTORY alone.  Every view here roots
;;      at `org-roam-directory', so going through `dired'/`dired-noselect'
;;      would make the second view silently return the first buffer.  We
;;      hand-build the buffer instead and never call either.
;;
;;   2. `(cons DIR nil)' does not mean "empty listing" to dired -- it means
;;      "list DIR itself", i.e. all ~2250 files in the notes tree.  Empty
;;      results are the common case here (most nodes are outbound leaves),
;;      so `org-roam-dired--visit' refuses before touching the buffer.
;;
;;   3. `evil-define-key*' -- the pattern documented in CLAUDE.md -- does
;;      NOT work for a minor mode.  See the keymap section below.
;;
;;   4. `dired-omit-expunge' runs on the *global* `dired-after-readin-hook',
;;      after buffer-local entries, and would delete the lines our title
;;      overlays are anchored to.  We disable `dired-omit-mode' locally.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'ls-lisp)
(require 'subr-x)
(require 'transient)

(declare-function org-roam-db-query "org-roam-db" (sql &rest args))
(declare-function org-roam-node-read "org-roam-node")
(declare-function org-roam-node-at-point "org-roam-node")
(declare-function org-roam-node-file "org-roam-node")
(declare-function org-roam-node-title "org-roam-node")
(declare-function org-fold-show-entry "org-fold")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function evil-define-minor-mode-key "evil-core")
(defvar org-roam-directory)
(defvar org-roam-file-exclude-regexp)

;;; ------------------------------------------------------ customization

(defgroup org-roam-dired nil
  "Browse the org-roam link graph as a dired buffer."
  :group 'org-roam
  :prefix "org-roam-dired-")

(defcustom org-roam-dired-listing-switches "-lh"
  "Switches passed to `ls' for the graph listing.
Deliberately minimal.  `dired-insert-directory' calls `insert-directory'
once per entry when given an explicit file list, so any sort-order
switches are inert -- sorting is handled by `org-roam-dired-sort'."
  :type 'string
  :group 'org-roam-dired)

(defcustom org-roam-dired-title-width 60
  "Maximum display width of the node title annotation, in columns.
A fixed budget rather than one derived from the window: the annotation
is built during `dired-readin', which usually runs before the buffer is
displayed anywhere, so window width is not knowable at that point."
  :type 'integer
  :group 'org-roam-dired)

(defcustom org-roam-dired-hide-details t
  "When non-nil, enable `dired-hide-details-mode' in the graph buffer.
Hides permissions, owner, size and date, which leaves room for the node
title annotation."
  :type 'boolean
  :group 'org-roam-dired)

(defcustom org-roam-dired-auto-flip-on-empty t
  "When non-nil, fall back to the opposite direction on an empty listing.
Most nodes in a roam graph are outbound leaves, so descending into one
would otherwise dead-end.  With this set, such a node shows its backlinks
instead and says so in the echo area."
  :type 'boolean
  :group 'org-roam-dired)

(defcustom org-roam-dired-sort 'path
  "Initial sort order for graph listings.
Cycled in the buffer with \\<org-roam-dired-mode-map>\\[org-roam-dired-cycle-sort]."
  :type '(choice (const :tag "Relative path" path)
                 (const :tag "Node title"    title)
                 (const :tag "Modified time" mtime))
  :group 'org-roam-dired)

(defconst org-roam-dired-buffer-name "*org-roam-dired*"
  "Name of the single buffer used to browse the org-roam link graph.")

;;; ------------------------------------------------------ faces (catppuccin-mocha)

(defface org-roam-dired-title-face
  '((t (:foreground "#cba6f7" :weight bold)))
  "Face for the node title annotated onto a dired line."
  :group 'org-roam-dired)

(defface org-roam-dired-heading-face
  '((t (:foreground "#f9e2af")))
  "Face for a title that came from a heading-level node.
Distinct from `org-roam-dired-title-face' so you can tell that the file
has no file-level node and the title belongs to a heading inside it."
  :group 'org-roam-dired)

(defface org-roam-dired-tags-face
  '((t (:foreground "#a6e3a1")))
  "Face for org-roam tags in the graph listing."
  :group 'org-roam-dired)

(defface org-roam-dired-count-face
  '((t (:foreground "#6c7086" :slant italic)))
  "Face for the \"(N nodes)\" marker on files holding several nodes."
  :group 'org-roam-dired)

;;; ------------------------------------------------------ buffer-local state

(defvar-local org-roam-dired--file nil
  "Absolute path of the file this view is rooted at.")

(defvar-local org-roam-dired--title nil
  "Display title of the root node.")

(defvar-local org-roam-dired--direction 'outbound
  "Either `outbound' (links from the root) or `backlink' (links to it).")

(defvar-local org-roam-dired--history nil
  "Stack of (FILE TITLE DIRECTION RETURN-FILE) for `org-roam-dired-back'.")

(defvar-local org-roam-dired--meta nil
  "Hash mapping absolute file name to a plist of node metadata.
Keys: :title :tags :count :level :id :pos.  Read by the annotation hook,
so it must be set before `revert-buffer' runs.")

(defvar-local org-roam-dired--sort nil
  "Buffer-local sort order; see `org-roam-dired-sort'.")

;;; ------------------------------------------------------ keymap and minor mode

;; This is layered onto a real dired buffer as a MINOR mode rather than
;; being a derived major mode, so that wdired, marks, `diredfl' and
;; `dired-get-marked-files' all keep working.  No `set-keymap-parent'
;; here: unlike the `tabulated-list-mode' pattern in `sf.el', the parent
;; map (`dired-mode-map') is already the local map underneath us.

(defvar org-roam-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")      #'org-roam-dired-descend)
    (define-key map (kbd "<return>") #'org-roam-dired-descend)
    (define-key map (kbd "TAB")      #'org-roam-dired-toggle-direction)
    (define-key map (kbd "<tab>")    #'org-roam-dired-toggle-direction)
    (define-key map "l" #'org-roam-dired-descend)
    (define-key map "h" #'org-roam-dired-back)
    (define-key map "-" #'org-roam-dired-back)
    (define-key map "^" #'org-roam-dired-back)
    (define-key map "F" #'org-roam-dired-forward-links)
    (define-key map "B" #'org-roam-dired-backlinks)
    (define-key map "o" #'org-roam-dired-open-other-window)
    (define-key map "O" #'org-roam-dired-open)
    (define-key map "." #'org-roam-dired-goto-root)
    (define-key map "H" #'org-roam-dired-history-jump)
    (define-key map "s" #'org-roam-dired-cycle-sort)
    (define-key map "?" #'org-roam-dired-dispatch)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `org-roam-dired-mode'.")

;; CLAUDE.md documents `evil-define-key*' as the fix for evil shadowing a
;; mode map.  That advice is for DERIVED MAJOR MODES and does not carry
;; over here.  `evil-define-key*' installs an *auxiliary* keymap on the
;; map you hand it, and an auxiliary map only fires if its host map is
;; consulted -- in normal state ours is not, because evil's state keymaps
;; sit ahead of `minor-mode-map-alist' in `emulation-mode-map-alists'.
;;
;; `evil-define-minor-mode-key' registers against the MODE SYMBOL in
;; `evil-minor-mode-keymaps-alist' instead, and `evil-state-keymaps'
;; (evil-core.el:664) orders the result
;;
;;     intercept -> local -> MINOR-MODE-MAPS -> aux -> overriding -> state
;;
;; so it beats both evil-collection's dired bindings (which land in the
;; aux maps) and evil's own h/l/RET (in the state map).
(with-eval-after-load 'evil
  (evil-define-minor-mode-key 'normal 'org-roam-dired-mode
    (kbd "RET")      #'org-roam-dired-descend
    (kbd "<return>") #'org-roam-dired-descend
    (kbd "TAB")      #'org-roam-dired-toggle-direction
    (kbd "<tab>")    #'org-roam-dired-toggle-direction
    "l"  #'org-roam-dired-descend
    "h"  #'org-roam-dired-back
    "-"  #'org-roam-dired-back
    "^"  #'org-roam-dired-back
    "F"  #'org-roam-dired-forward-links
    "B"  #'org-roam-dired-backlinks
    "o"  #'org-roam-dired-open-other-window
    "O"  #'org-roam-dired-open
    "."  #'org-roam-dired-goto-root
    "H"  #'org-roam-dired-history-jump
    "s"  #'org-roam-dired-cycle-sort
    "gr" #'org-roam-dired-refresh
    "?"  #'org-roam-dired-dispatch
    "q"  #'quit-window)
  (evil-define-minor-mode-key 'motion 'org-roam-dired-mode
    "h" #'org-roam-dired-back
    "l" #'org-roam-dired-descend))

(define-minor-mode org-roam-dired-mode
  "Layer org-roam graph navigation onto a real `dired-mode' buffer."
  :lighter " roam-graph"
  :keymap org-roam-dired-mode-map
  (when org-roam-dired-mode
    ;; One file per line is the whole premise of the annotation layer.
    (setq-local truncate-lines t)
    (when org-roam-dired-hide-details
      (dired-hide-details-mode 1))
    ;; `evil-define-minor-mode-key' entries are materialised into the
    ;; buffer-local `evil-mode-map-alist' by `evil-normalize-keymaps'.
    ;; Forcing a normalise here means a `SPC h r r' reload takes effect
    ;; without having to kill the buffer first (CLAUDE.md footgun #4).
    (when (fboundp 'evil-normalize-keymaps)
      (evil-normalize-keymaps))))

;;; ------------------------------------------------------ data layer

(defun org-roam-dired--require ()
  "Signal a `user-error' unless org-roam is available."
  (unless (require 'org-roam nil t)
    (user-error "org-roam is not available")))

(defun org-roam-dired--root ()
  "Return `org-roam-directory' expanded and in directory-name syntax."
  (file-name-as-directory (expand-file-name org-roam-directory)))

(defun org-roam-dired--node-ids (file)
  "Return a vector of every org-roam node ID living in FILE."
  (apply #'vector
         (mapcar #'car
                 (org-roam-db-query [:select id :from nodes :where (= file $s1)]
                                    (expand-file-name file)))))

(defun org-roam-dired--links (ids direction)
  "Return rows (FILE TITLE ID LEVEL POS) linked to node IDS.
IDS is a vector of node IDs.  DIRECTION is `outbound' for nodes that IDS
link to, or `backlink' for nodes that link to IDS.

This is deliberately one bulk query per view.  The `links' table carries
no index on either `source' or `dest', so both directions full-scan; that
costs about two milliseconds over this graph, which is only acceptable
because it never runs per line.  For the same reason we read raw rows
rather than building `org-roam-node' structs -- `org-roam-node-from-id'
goes through `org-roam-populate', which is five queries per node."
  (when (> (length ids) 0)
    (if (eq direction 'outbound)
        (org-roam-db-query
         [:select :distinct [nodes:file nodes:title nodes:id nodes:level nodes:pos]
          :from links :join nodes :on (= links:dest nodes:id)
          :where (in links:source $v1) :and (= links:type "id")]
         ids)
      (org-roam-db-query
       [:select :distinct [nodes:file nodes:title nodes:id nodes:level nodes:pos]
        :from links :join nodes :on (= links:source nodes:id)
        :where (in links:dest $v1) :and (= links:type "id")]
       ids))))

(defun org-roam-dired--tags (ids)
  "Return a hash mapping node ID to its list of tags, for node IDS."
  (let ((table (make-hash-table :test #'equal)))
    (when (> (length ids) 0)
      (pcase-dolist (`(,id ,tag)
                     (org-roam-db-query
                      [:select [node-id tag] :from tags :where (in node-id $v1)]
                      ids))
        (push tag (gethash id table))))
    table))

(defun org-roam-dired--excluded-p (file)
  "Non-nil when FILE matches `org-roam-file-exclude-regexp'.
Defensive only -- `org-roam-db-sync' already honours this -- but the
variable accepts both a string and a list of strings, so handle both
rather than assuming the shape the config happens to use today."
  (let ((rx (and (boundp 'org-roam-file-exclude-regexp)
                 org-roam-file-exclude-regexp)))
    (cond ((null rx) nil)
          ((stringp rx) (string-match-p rx file))
          ((listp rx) (cl-some (lambda (r) (string-match-p r file)) rx)))))

(defun org-roam-dired--merge-row (table row)
  "Fold ROW into TABLE, which maps absolute file name to a metadata plist.
Several nodes can share a file, so each file gets one entry.  A
file-level node's title wins over a heading node's, since that is the
title of the file itself; :count records how many nodes in the file were
actually hit by the query."
  (pcase-let* ((`(,file ,title ,id ,level ,pos) row)
               (key (expand-file-name file))
               (cur (gethash key table))
               (count (1+ (or (plist-get cur :count) 0))))
    ;; Take this row's identity when it is the first one seen for the file,
    ;; or when it is a file-level node displacing a heading-level one.
    (if (or (null cur)
            (and (= level 0) (> (or (plist-get cur :level) 0) 0)))
        (puthash key (list :title title :id id :level level
                           :pos pos :count count)
                 table)
      (puthash key (plist-put (copy-sequence cur) :count count) table))))

(defun org-roam-dired--collect (file direction &optional sort)
  "Return (RELS . META) describing FILE's DIRECTION neighbourhood.
RELS is the list of file names to hand dired, ordered by SORT and
relative to `org-roam-directory' where possible.  META is a hash keyed by
ABSOLUTE file name -- that is what `dired-get-filename' returns, and the
annotation hook looks entries up by it."
  (let* ((root (org-roam-dired--root))
         (self (expand-file-name file))
         (ids (org-roam-dired--node-ids self))
         (rows (org-roam-dired--links ids direction))
         (table (make-hash-table :test #'equal))
         (stale (make-hash-table :test #'equal))
         (hit-ids nil))
    (dolist (row rows)
      (let ((abs (expand-file-name (car row))))
        (cond
         ;; Drop the root's own file: self-links and links between two
         ;; headings of the same file would otherwise show as a self-loop.
         ((equal abs self))
         ((org-roam-dired--excluded-p abs))
         ;; A row whose file is gone means the DB is behind the filesystem.
         ;; Left in, dired pops a *ls error* buffer and leaves a stray line.
         ((not (file-exists-p abs)) (puthash abs t stale))
         (t (push (nth 2 row) hit-ids)
            (org-roam-dired--merge-row table (cons abs (cdr row)))))))
    (when (> (hash-table-count stale) 0)
      (message "org-roam-dired: %d stale entr%s skipped (run M-x org-roam-db-sync)"
               (hash-table-count stale)
               (if (= (hash-table-count stale) 1) "y" "ies")))
    ;; Tags come from one extra query scoped to the nodes we actually kept.
    (let ((tags (org-roam-dired--tags (apply #'vector hit-ids))))
      (maphash (lambda (key meta)
                 (puthash key
                          (plist-put meta :tags
                                     (gethash (plist-get meta :id) tags))
                          table))
               table))
    (cons (org-roam-dired--sorted-rels table root sort) table)))

(defun org-roam-dired--sorted-rels (table root sort)
  "Return the file names in TABLE, relative to ROOT, ordered by SORT."
  (let ((files nil))
    (maphash (lambda (key _meta) (push key files)) table)
    (setq files
          (pcase (or sort org-roam-dired-sort)
            ('title (sort files
                          (lambda (a b)
                            (string-lessp
                             (downcase (or (plist-get (gethash a table) :title) ""))
                             (downcase (or (plist-get (gethash b table) :title) ""))))))
            ('mtime (sort files
                          (lambda (a b)
                            (time-less-p (file-attribute-modification-time
                                          (file-attributes b))
                                         (file-attribute-modification-time
                                          (file-attributes a))))))
            (_ (sort files #'string-lessp))))
    ;; Relative names keep lines short, and they are what
    ;; `dired-get-filename' re-expands against `dired-current-directory'.
    ;; Anything outside the roam tree stays absolute; dired handles both.
    (mapcar (lambda (f)
              (if (string-prefix-p root f) (file-relative-name f root) f))
            files)))

;;; ------------------------------------------------------ title annotations

(defun org-roam-dired--annotation (meta)
  "Build the annotation after-string for META."
  (let* ((title (or (plist-get meta :title) ""))
         (tags (plist-get meta :tags))
         (count (plist-get meta :count))
         (heading-p (> (or (plist-get meta :level) 0) 0))
         (tag-str (when tags (concat "  :" (string-join (sort (copy-sequence tags)
                                                              #'string-lessp)
                                                        ":") ":")))
         (avail (max 16 org-roam-dired-title-width)))
    (when (> (string-width title) avail)
      (setq title (concat (truncate-string-to-width title (1- avail)) "…")))
    (concat "  "
            (propertize title 'face (if heading-p
                                        'org-roam-dired-heading-face
                                      'org-roam-dired-title-face))
            (when (and count (> count 1))
              (propertize (format " (%d nodes)" count)
                          'face 'org-roam-dired-count-face))
            (when tag-str
              (propertize tag-str 'face 'org-roam-dired-tags-face)))))

(defun org-roam-dired--annotate ()
  "Attach node-title overlays to every file line in the buffer.
Buffer-local member of `dired-after-readin-hook'.  `dired-revert' binds
that hook to nil around `dired-readin' and runs it once afterwards, so
this fires exactly once per redisplay with `dired-subdir-alist' built."
  (when org-roam-dired--meta
    ;; `erase-buffer' inside `dired-readin' collapses old overlays onto
    ;; point-min rather than deleting them, so clear ours by hand.
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'org-roam-dired)
        (delete-overlay ov)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((file (dired-get-filename nil t)) ; nil on the header line
               (meta (and file (gethash file org-roam-dired--meta))))
          (when meta
            (let* ((eol (line-end-position))
                   (ov (make-overlay eol eol nil t t)))
              (overlay-put ov 'org-roam-dired t)
              ;; `global-hl-line-mode' is on in this config; hl-line's
              ;; overlay sits at priority -50, so a higher priority keeps
              ;; our foreground colours while still picking up hl-line's
              ;; background -- the annotation highlights with its row.
              (overlay-put ov 'priority 100)
              (overlay-put ov 'after-string
                           (org-roam-dired--annotation meta)))))
        (forward-line 1)))))

;;; ------------------------------------------------------ buffer construction

(defun org-roam-dired--render (rels state)
  "Render RELS in the graph buffer with buffer-local STATE, and return it.
RELS must be non-empty and is relative to `org-roam-directory' where
possible.  STATE is a plist of :file :title :direction :meta.

This deliberately bypasses `dired', `dired-noselect' and
`dired-internal-noselect'.  `dirvish-override-dired-mode' advises
`dired-noselect', and `dirvish-dired-noselect-a' keys its session buffers
by directory into `dv-roots', only calling the real `dired-noselect' when
that key is new.  Every view here roots at `org-roam-directory', so the
second view would silently hand back the first buffer.  Building by hand
also skips `dirvish--setup-dired', which keeps the local map as
`dired-mode-map' and leaves `dirvish-curr' nil."
  (cl-assert rels t "org-roam-dired: refusing to render an empty file list")
  (let* ((root (org-roam-dired--root))
         (spec (cons root rels))
         (buf (get-buffer org-roam-dired-buffer-name))
         (fresh (not (and buf (buffer-live-p buf)
                          (with-current-buffer buf (derived-mode-p 'dired-mode))))))
    ;; A live buffer in the wrong major mode is unusable; start clean.
    (when (and buf fresh)
      (kill-buffer buf)
      (setq buf nil))
    (unless buf
      (setq buf (get-buffer-create org-roam-dired-buffer-name)))
    (with-current-buffer buf
      (let ((history org-roam-dired--history)
            (sort org-roam-dired--sort))
        (if (not fresh)
            ;; The re-root idiom `dired-internal-noselect' itself uses:
            ;; swap `dired-directory' and revert.  `dired-revert' preserves
            ;; a cons `dired-directory', so the explicit list survives.
            (setq dired-directory spec)
          (setq default-directory root) ; must precede `dired-mode'
          ;; Shadow `dired-buffers' so `dired-advertise' cannot register
          ;; this synthetic listing as *the* dired buffer for the notes
          ;; tree -- otherwise a plain `C-x d ~/Documents/notes/' could be
          ;; handed our graph view instead of the real directory.
          (let ((dired-buffers dired-buffers))
            (dired-mode spec org-roam-dired-listing-switches))
          ;; Everything below must come after `dired-mode', which opens
          ;; with `kill-all-local-variables'.
          ;;
          ;; `dired-omit-mode' is in `dired-mode-hook' under this config,
          ;; and `dired-omit-expunge' sits on the *global*
          ;; `dired-after-readin-hook'.  Global hook entries run after
          ;; buffer-local ones, so it would delete lines our overlays are
          ;; anchored to.  Its docstring: "Do nothing if ...
          ;; `dired-omit-mode' is nil".
          (when (fboundp 'dired-omit-mode)
            (dired-omit-mode -1))
          ;; With an explicit file list `dired-insert-directory' calls
          ;; `insert-directory' once per entry, which with the external
          ;; ls means one subprocess per file: measured at 434ms for a
          ;; 116-entry listing versus 68ms letting ls-lisp do it
          ;; in-process.  Buffer-local so ordinary dired is unaffected.
          (setq-local ls-lisp-use-insert-directory-program nil)
          (add-hook 'dired-after-readin-hook #'org-roam-dired--annotate nil t)
          (org-roam-dired-mode 1))
        ;; State must land before `revert-buffer' fires the annotate hook.
        (setq org-roam-dired--file (plist-get state :file)
              org-roam-dired--title (plist-get state :title)
              org-roam-dired--direction (plist-get state :direction)
              org-roam-dired--meta (plist-get state :meta)
              org-roam-dired--history history
              org-roam-dired--sort (or sort org-roam-dired-sort)))
      (setq mode-line-process
            (format " [%s %s]"
                    (if (eq org-roam-dired--direction 'outbound) "→" "←")
                    (or org-roam-dired--title "")))
      (revert-buffer)
      (goto-char (point-min))
      (dired-next-line 1))
    buf))

;;; ------------------------------------------------------ navigation

(defun org-roam-dired--check ()
  "Signal a `user-error' unless the current buffer is a graph view."
  (unless org-roam-dired--file
    (user-error "Not in an org-roam-dired buffer")))

(defun org-roam-dired--visit (file &optional title direction push)
  "Root the graph buffer at FILE, listing DIRECTION links, and show it.
TITLE is the display name for FILE.  When PUSH is non-nil, remember the
current view on the history stack first."
  (org-roam-dired--require)
  (let* ((file (expand-file-name file))
         (title (or title (file-name-nondirectory file)))
         (direction (or direction 'outbound))
         ;; Sort order lives in the graph buffer, but `--visit' can be
         ;; called from anywhere, so read it across explicitly.
         (sort (let ((buf (get-buffer org-roam-dired-buffer-name)))
                 (or (and buf (buffer-live-p buf)
                          (buffer-local-value 'org-roam-dired--sort buf))
                     org-roam-dired-sort)))
         (res (org-roam-dired--collect file direction sort))
         (flipped nil))
    ;; Most nodes are outbound leaves, so an empty result is the normal
    ;; case rather than an error.  Try the other direction before giving up.
    (when (and (null (car res)) org-roam-dired-auto-flip-on-empty)
      (let* ((other (if (eq direction 'outbound) 'backlink 'outbound))
             (alt (org-roam-dired--collect file other sort)))
        (when (car alt)
          (setq res alt direction other flipped t))))
    (unless (car res)
      ;; Never hand dired an empty file list: it reads (DIR . nil) as
      ;; "list DIR itself" and would dump the entire notes tree.  Bailing
      ;; here also means a failed descend leaves the current view intact.
      (user-error "%s is a leaf: no id links in either direction" title))
    (let ((history (when (buffer-live-p (get-buffer org-roam-dired-buffer-name))
                     (with-current-buffer org-roam-dired-buffer-name
                       (if (and push org-roam-dired--file)
                           (cons (list org-roam-dired--file
                                       org-roam-dired--title
                                       org-roam-dired--direction
                                       (dired-get-filename nil t))
                                 org-roam-dired--history)
                         org-roam-dired--history)))))
      (let ((buf (org-roam-dired--render
                  (car res)
                  (list :file file :title title
                        :direction direction :meta (cdr res)))))
        (with-current-buffer buf
          (setq org-roam-dired--history history))
        (pop-to-buffer-same-window buf)))
    (when flipped
      (message "No outbound links from %s — showing backlinks instead" title))))

(defun org-roam-dired--file-title (file)
  "Return the best display title for FILE from the roam database."
  (or (caar (org-roam-db-query
             [:select title :from nodes :where (= file $s1) :and (= level 0)]
             (expand-file-name file)))
      (caar (org-roam-db-query
             [:select title :from nodes :where (= file $s1)]
             (expand-file-name file)))
      (file-name-nondirectory file)))

(defun org-roam-dired-descend ()
  "Browse the graph neighbourhood of the file at point."
  (interactive)
  (org-roam-dired--check)
  (let ((file (dired-get-filename nil t)))
    (unless file
      (user-error "No file on this line"))
    (org-roam-dired--visit file
                           (or (plist-get (gethash file org-roam-dired--meta) :title)
                               (org-roam-dired--file-title file))
                           org-roam-dired--direction
                           t)))

(defun org-roam-dired-back ()
  "Return to the previous view on the history stack."
  (interactive)
  (org-roam-dired--check)
  (unless org-roam-dired--history
    (user-error "No previous view"))
  (pcase-let* ((`(,file ,title ,direction ,return-file)
                (pop org-roam-dired--history))
               (history org-roam-dired--history))
    (org-roam-dired--visit file title direction nil)
    (with-current-buffer org-roam-dired-buffer-name
      (setq org-roam-dired--history history)
      (when return-file
        (ignore-errors (dired-goto-file return-file))))))

(defun org-roam-dired--set-direction (direction)
  "Re-render the current root in DIRECTION without touching history."
  (org-roam-dired--check)
  (let ((history org-roam-dired--history))
    (org-roam-dired--visit org-roam-dired--file org-roam-dired--title
                           direction nil)
    (with-current-buffer org-roam-dired-buffer-name
      (setq org-roam-dired--history history))))

(defun org-roam-dired-toggle-direction ()
  "Flip between outbound links and backlinks for the current root."
  (interactive)
  (org-roam-dired--check)
  (org-roam-dired--set-direction
   (if (eq org-roam-dired--direction 'outbound) 'backlink 'outbound)))

(defun org-roam-dired-forward-links ()
  "Show outbound links for the current root."
  (interactive)
  (org-roam-dired--set-direction 'outbound))

(defun org-roam-dired-backlinks ()
  "Show backlinks for the current root."
  (interactive)
  (org-roam-dired--set-direction 'backlink))

(defun org-roam-dired-refresh ()
  "Re-query the database and re-render the current view.
Unlike a plain `revert-buffer', which would replay the file list this
buffer was built with, this picks up changes made since."
  (interactive)
  (org-roam-dired--check)
  (org-roam-dired--set-direction org-roam-dired--direction))

(defun org-roam-dired-cycle-sort ()
  "Cycle the listing between path, title and modified-time order."
  (interactive)
  (org-roam-dired--check)
  (setq org-roam-dired--sort
        (pcase org-roam-dired--sort
          ('path 'title)
          ('title 'mtime)
          (_ 'path)))
  (message "Sorting by %s" org-roam-dired--sort)
  (org-roam-dired-refresh))

(defun org-roam-dired-history-jump ()
  "Jump to an earlier view chosen from the history stack."
  (interactive)
  (org-roam-dired--check)
  (unless org-roam-dired--history
    (user-error "No previous view"))
  (let* ((choices (mapcar (lambda (entry)
                            (cons (format "%s  [%s]"
                                          (nth 1 entry)
                                          (if (eq (nth 2 entry) 'outbound)
                                              "outbound" "backlinks"))
                                  entry))
                          org-roam-dired--history))
         (pick (completing-read "Back to: " choices nil t))
         (entry (cdr (assoc pick choices)))
         ;; Truncate the stack at the chosen entry so `back' stays coherent.
         (rest (cdr (memq entry org-roam-dired--history))))
    (org-roam-dired--visit (nth 0 entry) (nth 1 entry) (nth 2 entry) nil)
    (with-current-buffer org-roam-dired-buffer-name
      (setq org-roam-dired--history rest)
      (when (nth 3 entry)
        (ignore-errors (dired-goto-file (nth 3 entry)))))))

;;; ------------------------------------------------------ opening

(defun org-roam-dired--open (file other-window)
  "Open FILE at its recorded node position, in OTHER-WINDOW when non-nil."
  (let* ((meta (and org-roam-dired--meta (gethash file org-roam-dired--meta)))
         (pos (plist-get meta :pos)))
    (if other-window
        (find-file-other-window file)
      (find-file file))
    ;; A heading-level hit should land on its heading, not the top of file.
    (when (and pos (> (or (plist-get meta :level) 0) 0))
      (goto-char pos)
      (cond ((fboundp 'org-fold-show-entry) (org-fold-show-entry))
            ((fboundp 'org-show-entry) (org-show-entry))))))

(defun org-roam-dired-open ()
  "Open the file at point in this window."
  (interactive)
  (org-roam-dired--check)
  (org-roam-dired--open (or (dired-get-filename nil t)
                            (user-error "No file on this line"))
                        nil))

(defun org-roam-dired-open-other-window ()
  "Open the file at point in another window."
  (interactive)
  (org-roam-dired--check)
  (org-roam-dired--open (or (dired-get-filename nil t)
                            (user-error "No file on this line"))
                        t))

(defun org-roam-dired-goto-root ()
  "Open the file this view is rooted at."
  (interactive)
  (org-roam-dired--check)
  (find-file org-roam-dired--file))

;;; ------------------------------------------------------ dispatcher

(transient-define-prefix org-roam-dired-dispatch ()
  "All org-roam graph-browser actions."
  [["Navigate"
    ("RET" "descend"      org-roam-dired-descend)
    ("h"   "back"         org-roam-dired-back)
    ("H"   "history"      org-roam-dired-history-jump)
    ("."   "root node"    org-roam-dired-goto-root)]
   ["Direction"
    ("TAB" "toggle"       org-roam-dired-toggle-direction)
    ("F"   "outbound"     org-roam-dired-forward-links)
    ("B"   "backlinks"    org-roam-dired-backlinks)]]
  [["Open"
    ("o"   "other window" org-roam-dired-open-other-window)
    ("O"   "this window"  org-roam-dired-open)]
   ["View"
    ("s"   "cycle sort"   org-roam-dired-cycle-sort)
    ("g"   "refresh"      org-roam-dired-refresh)
    ("q"   "quit"         quit-window)]])

;;; ------------------------------------------------------ entry points

;;;###autoload
(defun org-roam-dired ()
  "Browse the org-roam link graph as a dired buffer.
Prompts for the node to start from."
  (interactive)
  (org-roam-dired--require)
  (let ((node (org-roam-node-read nil nil nil t "Browse graph from node: ")))
    (org-roam-dired--visit (org-roam-node-file node)
                           (org-roam-node-title node)
                           'outbound
                           nil)))

;;;###autoload
(defun org-roam-dired-at-point ()
  "Browse the org-roam link graph starting from the node or file at point.
Uses the node at point in an Org buffer, the file at point in a Dired
buffer, and the visited file otherwise."
  (interactive)
  (org-roam-dired--require)
  (let* ((node (and (derived-mode-p 'org-mode) (org-roam-node-at-point)))
         (file (cond (node (org-roam-node-file node))
                     ((derived-mode-p 'dired-mode) (dired-get-filename nil t))
                     (buffer-file-name buffer-file-name))))
    (unless file
      (user-error "No org-roam node or file at point"))
    (org-roam-dired--visit file
                           (if node
                               (org-roam-node-title node)
                             (org-roam-dired--file-title file))
                           'outbound
                           nil)))

;;; ------------------------------------------------------ completion UI

;; Node selection is an ordinary `completing-read', so by default it
;; lands in the short vertico popup at the foot of the frame -- 17 rows
;; to pick from among ~1150 nodes, with the three-column display
;; template squeezed into whatever width is left.  Show it in a
;; full-frame buffer instead.  `vertico-multiform-commands' scopes this
;; to these two commands, so every other completion is untouched.
(with-eval-after-load 'vertico-multiform
  (require 'vertico-buffer nil t)
  (dolist (cmd '(org-roam-dired org-roam-dired-at-point))
    (add-to-list 'vertico-multiform-commands
                 `(,cmd buffer
                   (vertico-buffer-display-action . (display-buffer-full-frame))))))

;;; ------------------------------------------------------ keybindings

(map! :leader
      :desc "Roam graph as dired" "n g" #'org-roam-dired
      :desc "Roam graph at point" "n G" #'org-roam-dired-at-point)

(provide 'org-roam-dired)
;;; org-roam-dired.el ends here
