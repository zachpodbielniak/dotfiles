;;; shell-runner.el -*- lexical-binding: t; -*-
;;
;; shell-runner.el - Shell command runner with history, filtering, and re-execution
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
;; Port of the Neovim shell.lua command runner module for Doom Emacs.
;; Runs shell commands and captures output into Emacs buffers with
;; history management, filetype detection, filtering, and re-execution.
;;
;; Entry points are bound under SPC ! prefix.  Output buffers are
;; read-only with Evil normal-state bindings for refresh (r), filter (|),
;; quit (q), and help (?).

;;; Code:

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defvar shell-runner-history-file
  (expand-file-name "~/.local/share/doom/shell_history.txt")
  "Path to the persistent shell command history file.")

(defvar shell-runner-max-history 100
  "Maximum number of history entries to persist.")

(defvar shell-runner-default-destination "split"
  "Default destination for command output.
One of \"split\", \"vsplit\", \"float\", \"tab\", or \"buffer\".")

(defvar shell-runner-max-output-lines 10000
  "Maximum number of output lines kept before truncation.")

;; ---------------------------------------------------------------------------
;; Module state
;; ---------------------------------------------------------------------------

(defvar shell-runner--history nil
  "In-memory list of shell command history strings (oldest first).")

(defvar shell-runner--last-command nil
  "The most recently executed shell command string.")

(defvar shell-runner--last-buffer nil
  "The buffer created by the most recent shell-runner invocation.")

;; Buffer-local variables set on every output buffer.
(defvar-local shell-runner--buffer-cmd nil
  "The shell command that produced this buffer's content.")

(defvar-local shell-runner--buffer-dest nil
  "The destination style used to open this buffer.")

;; ---------------------------------------------------------------------------
;; History Management
;; ---------------------------------------------------------------------------

(defun shell-runner--load-history ()
  "Read the history file into `shell-runner--history'.
Create the file (and parent directories) when it does not exist."
  (let ((dir (file-name-directory shell-runner-history-file)))
    ;; Ensure the parent directory tree exists.
    (unless (file-directory-p dir)
      (make-directory dir t))
    ;; Create the file if missing.
    (unless (file-exists-p shell-runner-history-file)
      (write-region "" nil shell-runner-history-file nil 'silent))
    ;; Read lines, dropping empty trailing entries.
    (setq shell-runner--history
          (let ((raw (split-string
                      (with-temp-buffer
                        (insert-file-contents shell-runner-history-file)
                        (buffer-string))
                      "\n" t "[ \t]+")))
            (seq-filter (lambda (s) (not (string-empty-p s))) raw)))))

(defun shell-runner--save-history ()
  "Write the last `shell-runner-max-history' entries to the history file."
  (let ((entries (seq-take (reverse shell-runner--history)
                           shell-runner-max-history)))
    ;; entries is most-recent-first; reverse back to oldest-first for the file.
    (setq entries (nreverse entries))
    (let ((dir (file-name-directory shell-runner-history-file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (with-temp-file shell-runner-history-file
      (insert (mapconcat #'identity entries "\n"))
      (insert "\n"))))

(defun shell-runner--add-to-history (cmd)
  "Add CMD to history, skipping empty strings and removing duplicates."
  (when (and cmd (not (string-empty-p (string-trim cmd))))
    (let ((trimmed (string-trim cmd)))
      ;; Remove any existing duplicate.
      (setq shell-runner--history
            (seq-remove (lambda (s) (string= s trimmed))
                        shell-runner--history))
      ;; Append at the end (most recent).
      (setq shell-runner--history
            (append shell-runner--history (list trimmed)))
      ;; Trim to max length from the front (drop oldest).
      (when (> (length shell-runner--history) shell-runner-max-history)
        (setq shell-runner--history
              (seq-drop shell-runner--history
                        (- (length shell-runner--history)
                           shell-runner-max-history))))
      (shell-runner--save-history))))

;; ---------------------------------------------------------------------------
;; Filetype Detection
;; ---------------------------------------------------------------------------

(defun shell-runner--detect-filetype (cmd output)
  "Return a major-mode symbol based on CMD and OUTPUT content.
CMD is the shell command string.  OUTPUT is the raw output string."
  (cond
   ;; git diff / git show -> diff-mode
   ((string-match-p "\\bgit\\s+\\(diff\\|show\\)\\b" cmd)
    'diff-mode)

   ;; git log -> magit-log-mode if available, else fundamental-mode
   ((string-match-p "\\bgit\\s+log\\b" cmd)
    (if (fboundp 'magit-log-mode)
        'magit-log-mode
      'fundamental-mode))

   ;; other git commands
   ((string-match-p "\\bgit\\b" cmd)
    'fundamental-mode)

   ;; JSON: explicit json file, jq, or yq -o json
   ((or (string-match-p "\\.json\\b" cmd)
        (string-match-p "\\bjq\\b" cmd)
        (string-match-p "\\byq\\b.*-o\\s+json" cmd))
    'json-mode)

   ;; YAML: explicit yaml/yml file, or yq without -o json
   ((or (string-match-p "\\.ya?ml\\b" cmd)
        (and (string-match-p "\\byq\\b" cmd)
             (not (string-match-p "\\byq\\b.*-o\\s+json" cmd))))
    'yaml-mode)

   ;; Fallback: inspect the first line of output for JSON / YAML patterns.
   (t
    (let ((first-line (car (split-string (or output "") "\n" t))))
      (cond
       ((and first-line (string-match-p "\\`\\s*[{[]" first-line))
        'json-mode)
       ((and first-line (string-match-p "\\`\\s*---" first-line))
        'yaml-mode)
       (t
        'fundamental-mode))))))

;; ---------------------------------------------------------------------------
;; Buffer Setup
;; ---------------------------------------------------------------------------

(defun shell-runner--setup-buffer (buf cmd destination)
  "Configure BUF as a shell-runner output buffer for CMD opened via DESTINATION."
  (with-current-buffer buf
    ;; Store command and destination as buffer-local variables.
    (setq-local shell-runner--buffer-cmd cmd)
    (setq-local shell-runner--buffer-dest destination)
    ;; Make the buffer read-only.
    (setq buffer-read-only t)
    ;; Evil normal-state local keybindings.
    (when (bound-and-true-p evil-mode)
      (evil-local-set-key 'normal (kbd "r") #'shell-runner-refresh-buffer)
      (evil-local-set-key 'normal (kbd "|") #'shell-runner-prompt-filter)
      (evil-local-set-key 'normal (kbd "q") #'kill-current-buffer)
      (evil-local-set-key 'normal (kbd "?") #'shell-runner-show-help))))

;; ---------------------------------------------------------------------------
;; Buffer Naming
;; ---------------------------------------------------------------------------

(defun shell-runner--buffer-name (cmd)
  "Return a buffer name for CMD, truncating the command portion to 40 chars."
  (let ((display-cmd (if (> (length cmd) 40)
                         (concat (substring cmd 0 40) "...")
                       cmd)))
    (format "*Shell: %s*" display-cmd)))

;; ---------------------------------------------------------------------------
;; Core Execution
;; ---------------------------------------------------------------------------

(defun shell-runner-run-command (cmd &optional destination)
  "Execute CMD in an inferior shell and display output.
DESTINATION controls where the buffer appears and defaults to
`shell-runner-default-destination'.  Valid values are \"split\",
\"vsplit\", \"float\", \"tab\", and \"buffer\"."
  (interactive "sShell command: ")
  (let* ((dest (or destination shell-runner-default-destination))
         ;; Run the command and capture both stdout/stderr and exit code.
         (exit-code nil)
         (raw-output
          (condition-case err
              (with-temp-buffer
                (setq exit-code
                      (call-process-shell-command cmd nil (current-buffer) nil))
                (buffer-string))
            (error
             (setq exit-code 1)
             (format "Error running command: %s" (error-message-string err)))))
         ;; Split into lines.
         (lines (split-string raw-output "\n"))
         ;; Handle empty output.
         (lines (if (and (= (length lines) 1)
                         (string-empty-p (car lines)))
                    '("(No output)")
                  lines))
         ;; Truncate if necessary.
         (truncated nil)
         (lines (if (> (length lines) shell-runner-max-output-lines)
                    (progn
                      (setq truncated t)
                      (append (seq-take lines shell-runner-max-output-lines)
                              (list ""
                                    (format "... (truncated at %d lines)"
                                            shell-runner-max-output-lines))))
                  lines))
         ;; Detect filetype.
         (mode-sym (shell-runner--detect-filetype cmd raw-output))
         ;; Build header.
         (header (list (format "$ %s" cmd)
                       (make-string 72 ?-)))
         ;; Full content.
         (content (mapconcat #'identity (append header lines) "\n"))
         ;; Buffer name.
         (buf-name (shell-runner--buffer-name cmd))
         (buf (get-buffer-create buf-name)))

    ;; Record history.
    (shell-runner--add-to-history cmd)

    ;; Populate buffer content.
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min))))

    ;; Display the buffer according to destination.
    (cond
     ;; split -- horizontal split below
     ((string= dest "split")
      (select-window (split-window-below))
      (switch-to-buffer buf))

     ;; vsplit -- vertical split right
     ((string= dest "vsplit")
      (select-window (split-window-right))
      (switch-to-buffer buf))

     ;; float -- use Doom popup or side-window
     ((string= dest "float")
      (if (fboundp '+popup-buffer)
          (+popup-buffer buf)
        (display-buffer buf
                        '(display-buffer-in-side-window
                          . ((side . bottom)
                             (window-height . 0.4))))))

     ;; tab -- new tab
     ((string= dest "tab")
      (tab-bar-new-tab)
      (switch-to-buffer buf))

     ;; buffer -- current window
     ((string= dest "buffer")
      (switch-to-buffer buf))

     ;; unknown destination falls back to split
     (t
      (select-window (split-window-below))
      (switch-to-buffer buf)))

    ;; Setup buffer (keybindings, read-only, locals).
    (shell-runner--setup-buffer buf cmd dest)

    ;; Activate detected major mode (do this after setup so read-only is set).
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (condition-case nil
            (funcall mode-sym)
          (error (fundamental-mode)))))

    ;; Track last command / buffer.
    (setq shell-runner--last-command cmd)
    (setq shell-runner--last-buffer buf)

    ;; Warn on non-zero exit code.
    (when (and exit-code (not (= exit-code 0)))
      (message "shell-runner: command exited with code %d" exit-code))

    buf))

;; ---------------------------------------------------------------------------
;; Prompt Wrappers
;; ---------------------------------------------------------------------------

(defun shell-runner-prompt-command (&optional destination)
  "Prompt for a shell command and run it.
DESTINATION controls output placement."
  (interactive)
  (let ((cmd (read-shell-command "Shell command: ")))
    (when (and cmd (not (string-empty-p cmd)))
      (shell-runner-run-command cmd destination))))

(defun shell-runner-prompt-split ()
  "Prompt for a command and display output in a horizontal split."
  (interactive)
  (shell-runner-prompt-command "split"))

(defun shell-runner-prompt-vsplit ()
  "Prompt for a command and display output in a vertical split."
  (interactive)
  (shell-runner-prompt-command "vsplit"))

(defun shell-runner-prompt-float ()
  "Prompt for a command and display output in a floating popup."
  (interactive)
  (shell-runner-prompt-command "float"))

(defun shell-runner-prompt-tab ()
  "Prompt for a command and display output in a new tab."
  (interactive)
  (shell-runner-prompt-command "tab"))

(defun shell-runner-prompt-buffer ()
  "Prompt for a command and display output in the current window."
  (interactive)
  (shell-runner-prompt-command "buffer"))

;; ---------------------------------------------------------------------------
;; History Picker
;; ---------------------------------------------------------------------------

(defun shell-runner-history (&optional destination)
  "Pick a command from history via `completing-read' and run it.
DESTINATION controls output placement, defaulting to
`shell-runner-default-destination'."
  (interactive)
  (if (null shell-runner--history)
      (message "shell-runner: history is empty")
    (let* ((candidates (reverse shell-runner--history))
           (chosen (completing-read "History: " candidates nil t)))
      (when (and chosen (not (string-empty-p chosen)))
        (shell-runner-run-command chosen (or destination
                                             shell-runner-default-destination))))))

;; ---------------------------------------------------------------------------
;; Re-execution
;; ---------------------------------------------------------------------------

(defun shell-runner-rerun ()
  "Re-run the last executed command with the default destination."
  (interactive)
  (if shell-runner--last-command
      (shell-runner-run-command shell-runner--last-command
                               shell-runner-default-destination)
    (message "shell-runner: no previous command to re-run")))

(defun shell-runner-refresh-buffer (&optional buf)
  "Re-execute the command stored in BUF (default: current buffer).
The buffer content is replaced in place."
  (interactive)
  (let* ((target (or buf (current-buffer)))
         (cmd (buffer-local-value 'shell-runner--buffer-cmd target))
         (dest (buffer-local-value 'shell-runner--buffer-dest target)))
    (if (not cmd)
        (message "shell-runner: no command associated with this buffer")
      ;; Re-execute and capture output.
      (let* ((exit-code nil)
             (raw-output
              (condition-case err
                  (with-temp-buffer
                    (setq exit-code
                          (call-process-shell-command cmd nil (current-buffer) nil))
                    (buffer-string))
                (error
                 (setq exit-code 1)
                 (format "Error running command: %s" (error-message-string err)))))
             (lines (split-string raw-output "\n"))
             (lines (if (and (= (length lines) 1)
                             (string-empty-p (car lines)))
                        '("(No output)")
                      lines))
             (lines (if (> (length lines) shell-runner-max-output-lines)
                        (append (seq-take lines shell-runner-max-output-lines)
                                (list ""
                                      (format "... (truncated at %d lines)"
                                              shell-runner-max-output-lines)))
                      lines))
             (mode-sym (shell-runner--detect-filetype cmd raw-output))
             (header (list (format "$ %s" cmd)
                           (make-string 72 ?-)))
             (content (mapconcat #'identity (append header lines) "\n")))
        ;; Replace buffer content.
        (with-current-buffer target
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert content)
            (goto-char (point-min))
            ;; Re-apply mode.
            (condition-case nil
                (funcall mode-sym)
              (error (fundamental-mode)))))
        ;; Re-apply buffer setup (keybindings, read-only, locals).
        (shell-runner--setup-buffer target cmd dest)
        ;; Warn on non-zero exit.
        (when (and exit-code (not (= exit-code 0)))
          (message "shell-runner: command exited with code %d" exit-code))
        (message "shell-runner: refreshed")))))

;; ---------------------------------------------------------------------------
;; Filtering
;; ---------------------------------------------------------------------------

(defvar shell-runner--filter-presets
  '("grep (pattern)"
    "grep -v (exclude)"
    "jq (JSON)"
    "yq (YAML)"
    "head"
    "tail"
    "sort"
    "sort -n"
    "uniq"
    "wc -l"
    "Custom...")
  "Preset filter choices presented to the user.")

(defun shell-runner-prompt-filter (&optional buf)
  "Prompt for a filter to pipe the command in BUF through.
BUF defaults to the current buffer.  The original command stored
in the buffer is piped through the chosen filter and the result
is run via `shell-runner-run-command'."
  (interactive)
  (let* ((target (or buf (current-buffer)))
         (cmd (buffer-local-value 'shell-runner--buffer-cmd target)))
    (if (not cmd)
        (message "shell-runner: no command associated with this buffer")
      (let* ((preset (completing-read "Filter: "
                                      shell-runner--filter-presets nil t))
             (filter-cmd
              (cond
               ;; Filters that require an argument.
               ((string= preset "grep (pattern)")
                (let ((pat (read-string "grep pattern: ")))
                  (format "grep %s" (shell-quote-argument pat))))

               ((string= preset "grep -v (exclude)")
                (let ((pat (read-string "grep -v pattern: ")))
                  (format "grep -v %s" (shell-quote-argument pat))))

               ((string= preset "jq (JSON)")
                (let ((expr (read-string "jq expression: " ".")))
                  (format "jq %s" (shell-quote-argument expr))))

               ((string= preset "yq (YAML)")
                (let ((expr (read-string "yq expression: " ".")))
                  (format "yq %s" (shell-quote-argument expr))))

               ((string= preset "head")
                (let ((n (read-string "head lines: " "20")))
                  (format "head -n %s" (shell-quote-argument n))))

               ((string= preset "tail")
                (let ((n (read-string "tail lines: " "20")))
                  (format "tail -n %s" (shell-quote-argument n))))

               ;; No-argument filters.
               ((string= preset "sort")    "sort")
               ((string= preset "sort -n") "sort -n")
               ((string= preset "uniq")    "uniq")
               ((string= preset "wc -l")   "wc -l")

               ;; Custom filter.
               ((string= preset "Custom...")
                (read-string "Custom filter: "))

               (t nil))))

        (when filter-cmd
          (let ((piped (format "%s | %s" cmd filter-cmd)))
            (shell-runner-run-command
             piped
             (or (buffer-local-value 'shell-runner--buffer-dest target)
                 shell-runner-default-destination))))))))

;; ---------------------------------------------------------------------------
;; Help
;; ---------------------------------------------------------------------------

(defun shell-runner-show-help ()
  "Display a help buffer listing shell-runner keybindings and commands."
  (interactive)
  (let ((buf (get-buffer-create "*Shell Runner Help*"))
        (help-text
         (concat
          "Shell Runner - Help\n"
          (make-string 72 ?=) "\n\n"
          "Global Keybindings (SPC ! ...)\n"
          (make-string 40 ?-) "\n"
          "  SPC ! s   Run command (horizontal split)\n"
          "  SPC ! v   Run command (vertical split)\n"
          "  SPC ! f   Run command (float / popup)\n"
          "  SPC ! t   Run command (new tab)\n"
          "  SPC ! b   Run command (current buffer)\n"
          "  SPC ! h   Pick from history\n"
          "  SPC ! r   Re-run last command\n"
          "  SPC ! R   Refresh current output buffer\n"
          "  SPC ! |   Filter current output buffer\n"
          "  SPC ! ?   Show this help\n"
          "\n"
          "Buffer-local Keybindings (normal state)\n"
          (make-string 40 ?-) "\n"
          "  r         Refresh (re-execute command)\n"
          "  |         Pipe output through a filter\n"
          "  q         Kill the output buffer\n"
          "  ?         Show this help\n"
          "\n"
          "Interactive Commands\n"
          (make-string 40 ?-) "\n"
          "  M-x shell-runner-run-command\n"
          "  M-x shell-runner-prompt-split\n"
          "  M-x shell-runner-prompt-vsplit\n"
          "  M-x shell-runner-prompt-float\n"
          "  M-x shell-runner-prompt-tab\n"
          "  M-x shell-runner-prompt-buffer\n"
          "  M-x shell-runner-history\n"
          "  M-x shell-runner-rerun\n"
          "  M-x shell-runner-refresh-buffer\n"
          "  M-x shell-runner-prompt-filter\n"
          "  M-x shell-runner-show-help\n"
          "\n"
          "Filter Presets\n"
          (make-string 40 ?-) "\n"
          "  grep (pattern)      Filter lines matching a pattern\n"
          "  grep -v (exclude)   Exclude lines matching a pattern\n"
          "  jq (JSON)           Process JSON with jq\n"
          "  yq (YAML)           Process YAML with yq\n"
          "  head                Show first N lines\n"
          "  tail                Show last N lines\n"
          "  sort                Sort lines alphabetically\n"
          "  sort -n             Sort lines numerically\n"
          "  uniq                Remove adjacent duplicates\n"
          "  wc -l               Count lines\n"
          "  Custom...           Enter an arbitrary filter command\n")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert help-text)
        (goto-char (point-min)))
      (setq buffer-read-only t)
      (when (bound-and-true-p evil-mode)
        (evil-local-set-key 'normal (kbd "q") #'kill-current-buffer)))
    ;; Show as a popup if possible, otherwise in a side window.
    (if (fboundp '+popup-buffer)
        (+popup-buffer buf)
      (display-buffer buf
                      '(display-buffer-in-side-window
                        . ((side . bottom)
                           (window-height . 0.5)))))))

;; ---------------------------------------------------------------------------
;; Setup
;; ---------------------------------------------------------------------------

(defun shell-runner-setup ()
  "Initialize shell-runner: load history from disk."
  (shell-runner--load-history))

;; Run setup at load time.
(shell-runner-setup)

;; ---------------------------------------------------------------------------
;; Keybindings (Doom Emacs map!)
;; ---------------------------------------------------------------------------

(map! :leader
      :desc "shell" "!" nil
      :desc "Run (split)"   "! s" #'shell-runner-prompt-split
      :desc "Run (vsplit)"  "! v" #'shell-runner-prompt-vsplit
      :desc "Run (float)"   "! f" #'shell-runner-prompt-float
      :desc "Run (tab)"     "! t" #'shell-runner-prompt-tab
      :desc "Run (buffer)"  "! b" #'shell-runner-prompt-buffer
      :desc "History"       "! h" #'shell-runner-history
      :desc "Re-run"        "! r" #'shell-runner-rerun
      :desc "Refresh"       "! R" #'shell-runner-refresh-buffer
      :desc "Filter"        "! |" #'shell-runner-prompt-filter
      :desc "Help"          "! ?" #'shell-runner-show-help)

(provide 'shell-runner)
;;; shell-runner.el ends here
