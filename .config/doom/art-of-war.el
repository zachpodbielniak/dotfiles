;;; art-of-war.el --- Daily-study client for Sun Tzu's Art of War -*- lexical-binding: t; -*-

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

;; Treats The Art of War as a daily-study text, the way someone might
;; treat the Bible or the Stoics: a passage surfaces in the dashboard
;; each day, in the org-roam daily note's Reflection section, and is
;; available on demand via SPC s a.
;;
;; The canonical text and chapter/index org-roam nodes are generated
;; by `.scripts/aow-bootstrap.py' (run once); this module is a reader on
;; top of that tree.  Verses are addressed by `CHAPTER.VERSE' (e.g.
;; "3.18") resolved against `:AOW_REF:' properties in the translation
;; files.
;;
;; Entry points (under SPC s a):
;;   aow-todays-verse        — today's deterministic verse
;;   aow-random-verse        — random verse, "consult the master"
;;   aow-search              — full-text search across translations
;;   aow-open-giles          — open the canonical text
;;   aow-pick-chapter        — jump to a chapter roam node
;;   aow-multi-translation   — side-by-side N translations at one verse
;;   aow-open-index          — open the TOC roam node
;;
;; The bootstrap also overrides `org-roam-dailies-capture-templates'
;; so SPC m m d t produces a Reflection section pre-filled with today's
;; verse + a link back to the canonical passage.

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'org-element)

;; Forward declarations to silence byte-compile free-variable warnings
;; for symbols owned by packages loaded lazily via `with-eval-after-load'.
(defvar org-roam-dailies-capture-templates)
(defvar +dashboard-functions)

(defgroup aow nil
  "Art of War daily-study client."
  :group 'applications
  :prefix "aow-")

(defcustom aow-notes-dir (expand-file-name "~/Documents/notes")
  "Root of the PARA notes tree.
The Art of War study tree lives at `02_areas/study/art-of-war/'
under this directory."
  :type 'directory
  :group 'aow)

(defcustom aow-current-translation 'giles
  "Translation used as the canonical text for daily-contact commands.
Symbol naming a file under `translations/' (without the .org suffix)."
  :type '(choice (const :tag "Giles (1910, public domain)" giles)
                 (const :tag "Cleary"   cleary)
                 (const :tag "Sawyer"   sawyer)
                 (const :tag "Griffith" griffith))
  :group 'aow)

(defcustom aow-show-commentary t
  "When non-nil, include Giles's bracketed commentary in popup verse buffers."
  :type 'boolean
  :group 'aow)

(defcustom aow-dashboard-widget t
  "When non-nil, append a today's-verse widget to the Doom dashboard."
  :type 'boolean
  :group 'aow)

;;; ------------------------------------------------------ faces (catppuccin-mocha)

(defface aow-chapter-title-face
  '((t (:foreground "#cba6f7" :weight bold)))
  "Face for chapter titles in popup verse views."
  :group 'aow)

(defface aow-verse-ref-face
  '((t (:foreground "#f9e2af" :weight bold)))
  "Face for the chapter.verse label (e.g. III.18) in popup verse views."
  :group 'aow)

(defface aow-verse-text-face
  '((t (:foreground "#cdd6f4")))
  "Face for verse body text."
  :group 'aow)

(defface aow-commentary-face
  '((t (:foreground "#7f849c" :slant italic)))
  "Face for Giles's bracketed commentary."
  :group 'aow)

(defface aow-dashboard-footer-face
  '((t (:foreground "#6c7086" :slant italic)))
  "Face for the dashboard widget's footer hint."
  :group 'aow)

;;; ------------------------------------------------------ paths and meta

(defun aow--base-dir ()
  "Filesystem root of the Art of War study tree."
  (expand-file-name "02_areas/study/art-of-war/" aow-notes-dir))

(defun aow--translation-file (&optional translation)
  "Path to the .org file for TRANSLATION (defaults to `aow-current-translation')."
  (expand-file-name
   (format "translations/%s.org" (or translation aow-current-translation))
   (aow--base-dir)))

(defun aow--meta-file ()
  "Path to the elisp stamp written by .scripts/aow-bootstrap.py."
  (expand-file-name ".aow-meta.el" (aow--base-dir)))

(defvar aow--index-id nil
  "Org-roam ID of the index/TOC node.  Loaded from `.aow-meta.el'.")
(defvar aow--chapter-ids nil
  "Ordered list of org-roam IDs for the 13 chapter nodes.  From `.aow-meta.el'.")

(defun aow--load-meta ()
  "Load the elisp stamp file if present.
Bootstraps `aow--index-id' and `aow--chapter-ids'."
  (let ((meta (aow--meta-file)))
    (when (file-readable-p meta)
      (load-file meta))))

;;; ------------------------------------------------------ verse index

(defvar aow--verse-index nil
  "Alist mapping ref-string (\"3.18\") to a plist describing the verse.
Plist keys: :file :chapter :chapter-title :label :body :point.
Built lazily by `aow--ensure-index'.")

(defvar aow--verse-index-mtime nil
  "File modification time of the canonical text when the index was built.")

(defvar aow--verse-index-translation nil
  "Translation symbol that produced the cached index.")

(defun aow--available-translations ()
  "List of translation symbols for which a non-empty .org file exists."
  (let ((dir (expand-file-name "translations/" (aow--base-dir))))
    (when (file-directory-p dir)
      (cl-loop for f in (directory-files dir nil "\\.org\\'")
               for path = (expand-file-name f dir)
               when (> (file-attribute-size (file-attributes path)) 0)
               collect (intern (file-name-base f))))))

(defun aow--build-index-from-file (file)
  "Parse FILE and return an alist of ref → plist for every verse."
  (let (entries
        (mtime (file-attribute-modification-time (file-attributes file))))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer 'object) 'headline
        (lambda (h)
          (let* ((refs-prop  (org-element-property :AOW_REF h))
                 (chap-prop  (org-element-property :AOW_CHAPTER h))
                 (parent     (org-element-property :parent h))
                 (chap-num   (or (and chap-prop (string-to-number chap-prop))
                                 (when parent
                                   (let ((p (org-element-property :AOW_CHAPTER parent)))
                                     (and p (string-to-number p))))))
                 (chap-title (when parent
                               (org-element-property :raw-value parent))))
            (when refs-prop
              (let* ((begin   (org-element-property :contents-begin h))
                     (end     (org-element-property :contents-end h))
                     (body    (when (and begin end)
                                (string-trim
                                 (replace-regexp-in-string
                                  "^:PROPERTIES:\n\\(.\\|\n\\)*?\n:END:\n?"
                                  ""
                                  (buffer-substring-no-properties begin end)))))
                     (label   (org-element-property :raw-value h)))
                (dolist (ref (split-string refs-prop ","))
                  (let ((ref (string-trim ref)))
                    (push (cons ref
                                (list :file file
                                      :chapter chap-num
                                      :chapter-title chap-title
                                      :label label
                                      :body body
                                      :point (org-element-property :begin h)))
                          entries)))))))))
    (cons mtime (nreverse entries))))

(defun aow--ensure-index ()
  "Build `aow--verse-index' if missing or stale.  Returns the index alist."
  (let* ((file  (aow--translation-file))
         (trans aow-current-translation)
         (mtime (and (file-readable-p file)
                     (file-attribute-modification-time (file-attributes file)))))
    (unless (file-readable-p file)
      (user-error "Canonical text not found at %s — run .scripts/aow-bootstrap.py first" file))
    (when (or (null aow--verse-index)
              (not (eq trans aow--verse-index-translation))
              (not (equal mtime aow--verse-index-mtime)))
      (let ((built (aow--build-index-from-file file)))
        (setq aow--verse-index-mtime mtime
              aow--verse-index-translation trans
              aow--verse-index (cdr built))))
    aow--verse-index))

(defun aow--all-refs ()
  "Ordered list of every ref-string (\"1.1\", \"1.2\", ..., \"13.27\")."
  (mapcar #'car (aow--ensure-index)))

(defun aow--lookup (ref)
  "Return the plist for REF (e.g. \"3.18\"), or signal `user-error' if unknown."
  (or (cdr (assoc ref (aow--ensure-index)))
      (user-error "Unknown verse reference: %s" ref)))

;;; ------------------------------------------------------ daily / random selection

(defun aow--todays-ref ()
  "Deterministic ref for today (same date → same verse, regardless of restart)."
  (let* ((refs (aow--all-refs))
         (idx  (mod (time-to-days (current-time)) (length refs))))
    (nth idx refs)))

(defun aow--random-ref ()
  "A uniformly-random verse reference."
  (let ((refs (aow--all-refs)))
    (nth (random (length refs)) refs)))

(defun aow--roman (n)
  "Return the Roman-numeral string for 1..13."
  (nth (1- n) '("I" "II" "III" "IV" "V" "VI" "VII"
                "VIII" "IX" "X" "XI" "XII" "XIII")))

(defun aow--pretty-ref (ref)
  "Render REF (\"3.18\") as \"III.18\" for display."
  (let* ((parts (split-string ref "\\."))
         (c (string-to-number (car parts))))
    (format "%s.%s" (aow--roman c) (cadr parts))))

;;; ------------------------------------------------------ reader popup

(defvar aow--reader-current-ref nil
  "Ref string displayed in the current `aow-reader-mode' buffer.")
(make-variable-buffer-local 'aow--reader-current-ref)

(defvar aow-reader-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q")   #'quit-window)
    (define-key m (kbd "RET") #'aow-reader-jump-to-canonical)
    (define-key m (kbd "n")   #'aow-reader-next-verse)
    (define-key m (kbd "p")   #'aow-reader-prev-verse)
    (define-key m (kbd "r")   #'aow-random-verse)
    (define-key m (kbd "t")   #'aow-todays-verse)
    (define-key m (kbd "c")   #'aow-reader-open-chapter)
    (define-key m (kbd "m")   #'aow-reader-multi-translation)
    (define-key m (kbd "?")   #'aow-reader-show-keys)
    m)
  "Keymap for `aow-reader-mode'.")

;; Evil intercepts single-key motion/normal-state bindings (n=search-next,
;; r=replace, p=paste, RET=evil-ret, ...) before they reach the major-mode
;; map.  Re-bind them in evil's normal+motion states so the reader's keys
;; actually fire under Doom.
(with-eval-after-load 'evil
  (evil-define-key* '(normal motion) aow-reader-mode-map
    (kbd "q")   #'quit-window
    (kbd "RET") #'aow-reader-jump-to-canonical
    (kbd "n")   #'aow-reader-next-verse
    (kbd "p")   #'aow-reader-prev-verse
    (kbd "r")   #'aow-random-verse
    (kbd "t")   #'aow-todays-verse
    (kbd "c")   #'aow-reader-open-chapter
    (kbd "m")   #'aow-reader-multi-translation
    (kbd "?")   #'aow-reader-show-keys)
  (evil-set-initial-state 'aow-reader-mode 'motion))

(define-derived-mode aow-reader-mode special-mode "AoW"
  "Major mode for displaying a single verse in a popup buffer."
  (setq buffer-read-only t)
  (setq cursor-type nil))

(defun aow-reader-show-keys ()
  "Show the keymap for `aow-reader-mode'."
  (interactive)
  (message "q quit · RET canonical · n/p next/prev · r random · t today · c chapter · m translations"))

(defun aow--strip-commentary (body)
  "Remove bracketed commentary blocks from BODY when `aow-show-commentary' is nil."
  (if aow-show-commentary
      body
    (string-trim
     (replace-regexp-in-string "\\[[^]]*\\]\n?" "" body))))

(defun aow--insert-verse (plist ref)
  "Insert a formatted view of PLIST (the verse) into the current buffer."
  (let* ((title  (plist-get plist :chapter-title))
         (body   (aow--strip-commentary (plist-get plist :body)))
         (sep    (make-string 60 ?─)))
    (insert "\n  ")
    (insert (propertize (or title "")
                        'face 'aow-chapter-title-face))
    (insert "\n  ")
    (insert (propertize sep 'face 'aow-commentary-face))
    (insert "\n  ")
    (insert (propertize (aow--pretty-ref ref) 'face 'aow-verse-ref-face))
    (insert "\n\n")
    (dolist (line (split-string body "\n"))
      (let ((face (if (string-match-p "\\`\\s-*\\[" line)
                      'aow-commentary-face
                    'aow-verse-text-face)))
        (insert "  ")
        (insert (propertize line 'face face))
        (insert "\n")))
    (insert "\n  ")
    (insert (propertize sep 'face 'aow-commentary-face))
    (insert "\n  ")
    (insert (propertize "q quit · RET canonical · n/p next/prev · r random · c chapter · m translations · ? help"
                        'face 'aow-dashboard-footer-face))
    (insert "\n")))

(defun aow--show-verse (ref)
  "Display REF in a popup `aow-reader-mode' buffer."
  (let* ((plist (aow--lookup ref))
         (buf   (get-buffer-create "*art-of-war*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (aow-reader-mode)
        (setq aow--reader-current-ref ref)
        (aow--insert-verse plist ref)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun aow-reader-jump-to-canonical ()
  "Open the canonical text and jump to the verse displayed here."
  (interactive)
  (unless aow--reader-current-ref
    (user-error "No verse displayed"))
  (aow-jump-to-verse aow--reader-current-ref))

(defun aow-reader-next-verse ()
  "Show the next verse in canonical order."
  (interactive)
  (let* ((refs (aow--all-refs))
         (cur  (cl-position aow--reader-current-ref refs :test #'equal))
         (next (and cur (nth (mod (1+ cur) (length refs)) refs))))
    (when next (aow--show-verse next))))

(defun aow-reader-prev-verse ()
  "Show the previous verse in canonical order."
  (interactive)
  (let* ((refs (aow--all-refs))
         (cur  (cl-position aow--reader-current-ref refs :test #'equal))
         (prev (and cur (nth (mod (1- cur) (length refs)) refs))))
    (when prev (aow--show-verse prev))))

(defun aow-reader-open-chapter ()
  "Open the org-roam chapter node for the verse displayed here."
  (interactive)
  (unless aow--reader-current-ref
    (user-error "No verse displayed"))
  (let* ((plist (aow--lookup aow--reader-current-ref))
         (chap  (plist-get plist :chapter))
         (id    (nth (1- chap) aow--chapter-ids)))
    (if id
        (org-id-goto id)
      (user-error "Chapter %d roam node not registered — re-run .scripts/aow-bootstrap.py" chap))))

(defun aow-reader-multi-translation ()
  "Open the verse displayed here side-by-side across translations."
  (interactive)
  (unless aow--reader-current-ref
    (user-error "No verse displayed"))
  (aow-multi-translation aow--reader-current-ref))

;;; ------------------------------------------------------ interactive commands

;;;###autoload
(defun aow-todays-verse ()
  "Open today's deterministic verse (same date → same verse)."
  (interactive)
  (aow--show-verse (aow--todays-ref)))

;;;###autoload
(defun aow-random-verse ()
  "Open a random verse — \"consult the master\"."
  (interactive)
  (aow--show-verse (aow--random-ref)))

;;;###autoload
(defun aow-open-giles ()
  "Open the canonical text file."
  (interactive)
  (find-file (aow--translation-file 'giles)))

;;;###autoload
(defun aow-open-index ()
  "Open the index/TOC org-roam node."
  (interactive)
  (let ((path (expand-file-name "index.org" (aow--base-dir))))
    (if (file-readable-p path)
        (find-file path)
      (user-error "Index not found — run .scripts/aow-bootstrap.py first"))))

;;;###autoload
(defun aow-pick-chapter (n)
  "Jump to the org-roam node for chapter N (prompted, 1..13)."
  (interactive
   (list (read-number "Chapter (1-13): "
                      (or (and aow--reader-current-ref
                               (plist-get (aow--lookup aow--reader-current-ref) :chapter))
                          1))))
  (unless (and (integerp n) (<= 1 n 13))
    (user-error "Chapter must be 1..13"))
  (let ((id (nth (1- n) aow--chapter-ids)))
    (if id
        (org-id-goto id)
      (user-error "Chapter %d roam node not registered — re-run .scripts/aow-bootstrap.py" n))))

;;;###autoload
(defun aow-search (&optional all-translations)
  "Search the canonical text by keyword.
With prefix ALL-TRANSLATIONS, search across every translation file."
  (interactive "P")
  (let ((dir (if all-translations
                 (expand-file-name "translations/" (aow--base-dir))
               (aow--translation-file))))
    (cond
     ((fboundp 'consult-ripgrep)
      (if all-translations
          (consult-ripgrep dir)
        (consult-ripgrep (file-name-directory dir)
                         (concat "-- " (file-name-nondirectory dir)))))
     ((fboundp '+default/search-project)
      (let ((default-directory (file-name-directory dir)))
        (call-interactively #'+default/search-project)))
     (t
      (let ((default-directory (file-name-directory dir)))
        (call-interactively #'rgrep))))))

;;;###autoload
(defun aow-jump-to-verse (ref)
  "Open the canonical text at REF (\"C.V\")."
  (interactive
   (list (completing-read "Verse: " (aow--all-refs) nil t
                          (and aow--reader-current-ref
                               aow--reader-current-ref))))
  (let* ((plist (aow--lookup ref))
         (file  (plist-get plist :file))
         (pos   (plist-get plist :point)))
    (find-file file)
    (goto-char pos)
    (org-reveal)
    (recenter 2)))

;;;###autoload
(defun aow-multi-translation (ref)
  "Open REF side-by-side across all available translation files."
  (interactive
   (list (completing-read "Verse: " (aow--all-refs) nil t
                          (and aow--reader-current-ref
                               aow--reader-current-ref))))
  (let* ((translations (aow--available-translations))
         (n (length translations)))
    (when (zerop n)
      (user-error "No translation files found under %s"
                  (expand-file-name "translations/" (aow--base-dir))))
    (delete-other-windows)
    (let ((first t))
      (dolist (trans translations)
        (let* ((file (aow--translation-file trans))
               (aow-current-translation trans)
               (aow--verse-index nil)            ; force per-translation rebuild
               (plist (aow--lookup ref))
               (pos   (plist-get plist :point)))
          (unless first (split-window-right) (other-window 1))
          (find-file file)
          (goto-char pos)
          (org-reveal)
          (recenter 2)
          (setq first nil))))
    (balance-windows)
    (setq aow--verse-index nil)))                ; clear cache, will rebuild lazily

;;; ------------------------------------------------------ capture-template helper

(defun aow--todays-verse-as-org-block ()
  "Return today's verse formatted as an org quote block.
Used by `org-roam-dailies-capture-templates' below."
  (condition-case nil
      (let* ((ref   (aow--todays-ref))
             (plist (aow--lookup ref))
             (title (or (plist-get plist :chapter-title) ""))
             (body  (string-trim (aow--strip-commentary (plist-get plist :body))))
             (chap  (plist-get plist :chapter))
             (file  (plist-get plist :file))
             (id    (nth (1- chap) aow--chapter-ids)))
        (concat "#+begin_quote\n"
                (format "*Art of War — %s* — /%s/\n\n" (aow--pretty-ref ref) title)
                body "\n\n"
                "—\n"
                (format "[[file:%s::*%s][Open in canonical]]" file title)
                (when id (format " · [[id:%s][Chapter %s roam node]]" id (aow--roman chap)))
                "\n#+end_quote"))
    (error
     "#+begin_quote\nArt of War: canonical text not yet bootstrapped.\nRun .scripts/aow-bootstrap.py to populate ~/Documents/notes/02_areas/study/art-of-war/.\n#+end_quote")))

;;; ------------------------------------------------------ dashboard widget

(defun aow--dashboard-widget ()
  "Insert today's-verse widget into the Doom dashboard buffer."
  (when (and aow-dashboard-widget
             (file-readable-p (aow--translation-file)))
    (condition-case nil
        (let* ((ref   (aow--todays-ref))
               (plist (aow--lookup ref))
               (title (plist-get plist :chapter-title))
               (body  (string-trim (aow--strip-commentary (plist-get plist :body))))
               (width (min 78 (- (window-width) 4))))
          (insert "\n")
          (insert (propertize (format "%s · %s\n"
                                      (aow--pretty-ref ref)
                                      (or title ""))
                              'face 'aow-verse-ref-face))
          (insert "\n")
          (dolist (paragraph (split-string body "\n\n"))
            (let* ((collapsed (replace-regexp-in-string "\n" " " paragraph))
                   (wrapped   (with-temp-buffer
                                (insert collapsed)
                                (let ((fill-column width)) (fill-region (point-min) (point-max)))
                                (buffer-string))))
              (insert (propertize wrapped
                                  'face (if (string-match-p "\\`\\s-*\\[" wrapped)
                                            'aow-commentary-face
                                          'aow-verse-text-face)))
              (insert "\n\n")))
          (insert (propertize "  SPC s a t · today    SPC s a r · random    SPC s a / · search\n"
                              'face 'aow-dashboard-footer-face))
          (insert "\n"))
      (error nil))))

;;; ------------------------------------------------------ org-roam wiring

(with-eval-after-load 'org-roam
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry
           "* %?"
           :target (file+head
                    "%<%Y-%m-%d>.org"
                    ,(concat "#+title: %<%Y-%m-%d>\n\n"
                             "* Reflection\n"
                             "%(aow--todays-verse-as-org-block)\n\n"))))))

;; Note: previously this section also scheduled `org-roam-db-sync' on a
;; 2-second idle timer to make sure the bootstrap-generated chapter :ID:
;; nodes were immediately resolvable.  In practice this caused Emacs to
;; appear to hang shortly after startup on machines with a large notes
;; tree (the sync prompts for schema upgrades and processes every .org
;; file synchronously).  Run `M-x org-roam-db-sync' manually when needed
;; — or enable `org-roam-db-autosync-mode' if you want background sync.

;;; ------------------------------------------------------ dashboard hook
;;;
;;; The `:ui dashboard' module (the Doom 3.x replacement for the older
;;; `:ui doom-dashboard') defcustoms `+dashboard-functions' during module
;;; init — before user config.el — so we can splice into it directly.
;;; Position our widget right after `+dashboard-widget-banner' so the verse
;;; renders just below the splash image.
;;;
;;; Disabled for now — uncomment the form below to re-enable.  The widget
;;; function `aow--dashboard-widget' itself is still defined and can be
;;; invoked interactively (e.g. M-: (aow--dashboard-widget)) for testing.

;; (when (and aow-dashboard-widget
;;            (boundp '+dashboard-functions))
;;   (let ((rebuilt '())
;;         (banner '+dashboard-widget-banner))
;;     (dolist (fn +dashboard-functions)
;;       (push fn rebuilt)
;;       (when (eq fn banner)
;;         (push 'aow--dashboard-widget rebuilt)))
;;     (unless (memq 'aow--dashboard-widget rebuilt)
;;       (push 'aow--dashboard-widget rebuilt))
;;     (setq +dashboard-functions
;;           (delete-dups (nreverse rebuilt)))))

;;; ------------------------------------------------------ org-remark in reader buffers

(with-eval-after-load 'org-remark
  ;; When visiting the canonical Giles text, auto-enable org-remark so
  ;; the green/red/blue pens (defined in org-remark.el) are immediately
  ;; available without the user toggling org-remark-mode.
  (add-hook 'find-file-hook
            (lambda ()
              (when (and buffer-file-name
                         (string-prefix-p (aow--base-dir) (expand-file-name buffer-file-name))
                         (string-match-p "/translations/" buffer-file-name))
                (org-remark-mode 1)))))

;;; ------------------------------------------------------ init

(aow--load-meta)

;;; ------------------------------------------------------ keybindings

(map! :leader
      (:prefix ("s a" . "art of war")
       :desc "Today's verse"         "t" #'aow-todays-verse
       :desc "Random verse"          "r" #'aow-random-verse
       :desc "Jump to verse"         "j" #'aow-jump-to-verse
       :desc "Search canonical"      "/" #'aow-search
       :desc "Open Giles"            "o" #'aow-open-giles
       :desc "Pick chapter"          "c" #'aow-pick-chapter
       :desc "Multi-translation"     "m" #'aow-multi-translation
       :desc "Open index"            "i" #'aow-open-index))

(provide 'art-of-war)
;;; art-of-war.el ends here
