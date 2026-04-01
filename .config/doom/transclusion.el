;;; transclusion.el -*- lexical-binding: t; -*-
;;
;; Inline transclusion system for markdown notes — Doom Emacs port of the
;; transclusion mappings from the Neovim mappings.lua.  Handles file links,
;; command links, Transcription UUIDs, and PARA note integration.
;;
;; Link formats recognised:
;;   ![[path/to/file.md]]         — embed file content
;;   ![[!command here]]           — run command, use output
;;   [title](path/to/file.md)    — standard markdown file link
;;   [title](!command here)       — link that runs command
;;   Transcription UUID: abcd-... — look up transcription


;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defvar transclusion-notes-dir "~/Documents/notes/"
  "Root directory for the PARA knowledge-base.")

(defvar transclusion-para-subdirs
  '("00_inbox" "01_projects" "02_areas" "03_resources" "04_archives")
  "PARA sub-directories to scan when inserting links.")


;; ---------------------------------------------------------------------------
;; Core Helpers
;; ---------------------------------------------------------------------------

(defun transclusion--notes-dir ()
  "Return the fully expanded notes directory path."
  (expand-file-name transclusion-notes-dir))

(defun transclusion--resolve-path (path)
  "Expand PATH relative to `transclusion--notes-dir'.
Return the absolute path when the file exists, nil otherwise."
  (let ((full (expand-file-name path (transclusion--notes-dir))))
    (when (file-exists-p full)
      full)))

(defun transclusion--parse-link-at-point ()
  "Parse the current line for a transclusion or markdown link.

Returns a plist (:type TYPE :target TARGET :start START :end END)
where TYPE is one of `file', `command', `transcription', `url', or nil
if nothing matched.  START and END are column positions of the link
on the line (0-indexed)."
  (let ((line (thing-at-point 'line t))
        (col  (current-column)))
    (when line
      ;; Strip trailing newline so positions stay on-line
      (setq line (replace-regexp-in-string "\n\\'" "" line))

      (cond
       ;; 1. Transcription UUID — matches anywhere on the line
       ((string-match
         "Transcription UUID:\\s-*\\([[:xdigit:]-]+\\)" line)
        (list :type 'transcription
              :target (match-string 1 line)
              :start  (match-beginning 0)
              :end    (match-end 0)))

       ;; 2. Command transclusion: ![[!command]]
       ((and (string-match "!\\[\\[!\\([^]]+\\)\\]\\]" line)
             (<= (match-beginning 0) col)
             (<= col (match-end 0)))
        (list :type 'command
              :target (match-string 1 line)
              :start  (match-beginning 0)
              :end    (match-end 0)))

       ;; 3. Markdown command link: [title](!command)
       ((and (string-match "\\[[^]]*\\](!\\([^)]+\\))" line)
             (<= (match-beginning 0) col)
             (<= col (match-end 0)))
        (list :type 'command
              :target (match-string 1 line)
              :start  (match-beginning 0)
              :end    (match-end 0)))

       ;; 4. File transclusion: ![[path]]
       ((and (string-match "!\\[\\[\\([^]]+\\)\\]\\]" line)
             (<= (match-beginning 0) col)
             (<= col (match-end 0)))
        (list :type 'file
              :target (match-string 1 line)
              :start  (match-beginning 0)
              :end    (match-end 0)))

       ;; 5. Markdown link: [title](target)
       ((and (string-match "\\[[^]]*\\](\\([^)]+\\))" line)
             (<= (match-beginning 0) col)
             (<= col (match-end 0)))
        (let ((target (match-string 1 line)))
          (if (string-match-p "\\`https?://" target)
              (list :type 'url
                    :target target
                    :start  (match-beginning 0)
                    :end    (match-end 0))
            (list :type 'file
                  :target target
                  :start  (match-beginning 0)
                  :end    (match-end 0)))))

       ;; 6. Bare ticket ID: PROJ-12345
       ((and (string-match "\\b\\([A-Z]+-[0-9]+\\)\\b" line)
             (<= (match-beginning 1) col)
             (<= col (match-end 1)))
        (list :type 'file
              :target (match-string 1 line)
              :start  (match-beginning 1)
              :end    (match-end 1)))

       ;; Nothing matched
       (t nil)))))


;; ---------------------------------------------------------------------------
;; Internal: run a shell command safely
;; ---------------------------------------------------------------------------

(defun transclusion--shell-command (cmd)
  "Run CMD via the shell and return a plist (:output STRING :exit INT)."
  (with-temp-buffer
    (let ((exit-code (call-process-shell-command cmd nil t nil)))
      (list :output (buffer-string)
            :exit   exit-code))))

(defun transclusion--shell-command-lines (cmd)
  "Run CMD via the shell and return output split into a list of lines."
  (let ((result (transclusion--shell-command cmd)))
    (split-string (plist-get result :output) "\n" nil)))


;; ---------------------------------------------------------------------------
;; Action: Run or Expand
;; ---------------------------------------------------------------------------

;;;###autoload
(defun transclusion-run-or-expand ()
  "Run the command link at point silently, or fall back to shell-on-region.

For command transclusions (![[!cmd]] / [title](!cmd)):
  - On success (exit 0): display a short message with the command name.
  - On failure (non-zero): open a new buffer with a markdown-formatted
    error report showing the command, exit code, and output.

When no link is found the current line is executed as a shell command
and its output replaces the line (Evil !!bash equivalent)."
  (interactive)
  (let ((link (transclusion--parse-link-at-point)))
    (if (and link (eq (plist-get link :type) 'command))
        ;; Run command transclusion silently
        (let* ((cmd    (plist-get link :target))
               (result (transclusion--shell-command cmd))
               (output (plist-get result :output))
               (exit   (plist-get result :exit)))
          (if (= exit 0)
              ;; Success — brief notification with first word of command
              (message "Done: %s" (car (split-string cmd)))
            ;; Failure — open error report in a new buffer
            (let ((buf (generate-new-buffer "*transclusion-error*")))
              (with-current-buffer buf
                (insert "# Command Failed\n\n")
                (insert (format "**Command**: `%s`\n" cmd))
                (insert (format "**Exit Code**: %d\n\n" exit))
                (insert "## Output\n\n")
                (insert output)
                (markdown-mode)
                (setq buffer-read-only t)
                (goto-char (point-min)))
              (switch-to-buffer buf))))
      ;; No command link — execute current line as shell command, replace it
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (shell-command-on-region beg end "bash" nil t)))))


;; ---------------------------------------------------------------------------
;; Action: Expand Inline
;; ---------------------------------------------------------------------------

;;;###autoload
(defun transclusion-expand-inline ()
  "Expand the link at point inline, inserting content below the current line.

Handles transcription UUIDs (via `transcriptions view'), command
transclusions, and file transclusions."
  (interactive)
  (let ((link (transclusion--parse-link-at-point)))
    (unless link
      (user-error "No transclusion link found on this line"))
    (let* ((type   (plist-get link :type))
           (target (plist-get link :target))
           (content
            (cond
             ;; Transcription UUID
             ((eq type 'transcription)
              (let ((cmd (format "transcriptions view %s -f simple"
                                 (shell-quote-argument target))))
                (transclusion--shell-command-lines cmd)))

             ;; Command transclusion
             ((eq type 'command)
              (transclusion--shell-command-lines target))

             ;; File transclusion
             ((eq type 'file)
              (let ((full (transclusion--resolve-path target)))
                (if full
                    (with-temp-buffer
                      (insert-file-contents full)
                      (split-string (buffer-string) "\n"))
                  (user-error "File not found: %s" target))))

             ;; URL — nothing to expand
             ((eq type 'url)
              (user-error "Cannot expand a URL inline"))

             (t (user-error "Unknown link type")))))
      ;; Insert content below the current line
      (save-excursion
        (end-of-line)
        (dolist (l content)
          (newline)
          (insert l))))))


;; ---------------------------------------------------------------------------
;; Action: Open in Popup
;; ---------------------------------------------------------------------------

;;;###autoload
(defun transclusion-open-float ()
  "Display the link target at point in a read-only popup buffer.

The popup uses `markdown-mode' and can be closed with `q'."
  (interactive)
  (let ((link (transclusion--parse-link-at-point)))
    (unless link
      (user-error "No transclusion link found on this line"))
    (let* ((type   (plist-get link :type))
           (target (plist-get link :target))
           (content
            (cond
             ;; Transcription UUID
             ((eq type 'transcription)
              (let ((cmd (format "transcriptions view %s -f simple"
                                 (shell-quote-argument target))))
                (string-join (transclusion--shell-command-lines cmd) "\n")))

             ;; Command
             ((eq type 'command)
              (plist-get (transclusion--shell-command target) :output))

             ;; File
             ((eq type 'file)
              (let ((full (transclusion--resolve-path target)))
                (if full
                    (with-temp-buffer
                      (insert-file-contents full)
                      (buffer-string))
                  (user-error "File not found: %s" target))))

             ;; URL — nothing to preview
             ((eq type 'url)
              (user-error "Cannot preview a URL in popup"))

             (t (user-error "Unknown link type")))))
      ;; Display in a popup buffer
      (let ((buf (generate-new-buffer "*transclusion-popup*")))
        (with-current-buffer buf
          (insert content)
          (markdown-mode)
          (setq buffer-read-only t)
          (goto-char (point-min))
          ;; q to close
          (local-set-key (kbd "q") #'kill-buffer-and-window))
        (display-buffer-in-side-window buf
                                       '((side . bottom)
                                         (window-height . 0.4)))
        (select-window (get-buffer-window buf))))))


;; ---------------------------------------------------------------------------
;; Action: Goto Link
;; ---------------------------------------------------------------------------

;;;###autoload
(defun transclusion-goto-link ()
  "Follow the link at point.

- Transcription UUID: run `transcriptions view', save to temp .md, open it.
- Command: run command, save output to temp .md, open it.
- File: open the file directly with `find-file'.
- URL: open with `browse-url'."
  (interactive)
  (let ((link (transclusion--parse-link-at-point)))
    (unless link
      (user-error "No transclusion link found on this line"))
    (let ((type   (plist-get link :type))
          (target (plist-get link :target)))
      (cond
       ;; Transcription UUID — view output in a temp file
       ((eq type 'transcription)
        (let* ((cmd    (format "transcriptions view %s -f simple"
                               (shell-quote-argument target)))
               (output (plist-get (transclusion--shell-command cmd) :output))
               (tmp    (concat (make-temp-file "transclusion-") ".md")))
          (with-temp-file tmp
            (insert output))
          (find-file tmp)))

       ;; Command — run and open output in a temp file
       ((eq type 'command)
        (let* ((output (plist-get (transclusion--shell-command target) :output))
               (tmp    (concat (make-temp-file "transclusion-") ".md")))
          (with-temp-file tmp
            (insert output))
          (find-file tmp)))

       ;; File — open directly
       ((eq type 'file)
        (let ((full (transclusion--resolve-path target)))
          (if full
              (find-file full)
            (user-error "File not found: %s" target))))

       ;; URL — hand off to the browser
       ((eq type 'url)
        (browse-url target))

       (t (user-error "Unknown link type"))))))


;; ---------------------------------------------------------------------------
;; PARA Integration: Insert Link
;; ---------------------------------------------------------------------------

;;;###autoload
(defun transclusion-insert-para-link ()
  "Insert a `![[...]]' transclusion link to a PARA note.

Recursively scans the PARA sub-directories for `.md' files and
presents them via `completing-read'.  The selected path is inserted
relative to `transclusion-notes-dir'."
  (interactive)
  (let* ((notes (transclusion--notes-dir))
         (all-files '()))
    ;; Collect all .md files from every PARA directory
    (dolist (subdir transclusion-para-subdirs)
      (let ((dir (expand-file-name subdir notes)))
        (when (file-directory-p dir)
          (dolist (f (directory-files-recursively dir "\\.md\\'"))
            (push f all-files)))))
    (unless all-files
      (user-error "No markdown files found in PARA directories"))
    ;; Build alist of relative-path -> absolute-path for display
    (let* ((relative-alist
            (mapcar (lambda (abs)
                      (cons (file-relative-name abs notes) abs))
                    (nreverse all-files)))
           (chosen (completing-read "Insert PARA link: "
                                    (mapcar #'car relative-alist)
                                    nil t))
           (link (concat "![[" chosen "]]")))
      (insert link))))


;; ---------------------------------------------------------------------------
;; Keybindings (Evil leader, Doom map!)
;; ---------------------------------------------------------------------------

;;; Transclusion bindings under SPC n x ("cross-inclusion")
;;; SPC n t = org-todo-list, SPC n c = org-clock toggle (Doom defaults)
;;; Org-transclusion (primary) + markdown transclusion (legacy, during migration)
(map! :leader
      (:prefix ("n" . "notes")
       (:prefix ("x" . "transclusion")
        ;; org-transclusion (primary — for .org files)
        :desc "Add at point"        "a" #'org-transclusion-add
        :desc "Add all in buffer"   "A" #'org-transclusion-add-all
        :desc "Remove at point"     "r" #'org-transclusion-remove
        :desc "Remove all"          "R" #'org-transclusion-remove-all
        :desc "Live edit"           "e" #'org-transclusion-live-sync-start
        ;; markdown transclusion (legacy — for .md files during migration)
        :desc "Insert PARA link"    "i" #'transclusion-insert-para-link
        :desc "Run/expand silently" "x" #'transclusion-run-or-expand
        :desc "Expand inline"       "E" #'transclusion-expand-inline
        :desc "Open in popup"       "f" #'transclusion-open-float
        :desc "Goto link"           "g" #'transclusion-goto-link)))

(provide 'transclusion)
;;; transclusion.el ends here
