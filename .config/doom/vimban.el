;;; vimban.el -*- lexical-binding: t; -*-

;; vimban.el - Comprehensive Doom Emacs interface for vimban ticket management
;; Ported from lua/custom/vimban.lua for Neovim
;; Extends SPC v namespace with ticket viewing, creating, and updating

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defvar vimban-notes-dir (expand-file-name "~/Documents/notes/")
  "Root directory for notes managed by vimban.")

(defvar vimban-statuses
  '("backlog" "ready" "in_progress" "blocked" "review" "delegated" "done" "cancelled")
  "Available ticket statuses.")

(defvar vimban-ticket-types
  '("task" "bug" "story" "research" "epic" "sub-task")
  "Available ticket types.")

(defvar vimban-priorities
  '("critical" "high" "medium" "low")
  "Available ticket priorities.")

(defvar vimban-dashboard-types
  '("daily" "weekly" "sprint" "project" "person")
  "Available dashboard types.")

;;; ============================================================================
;;; Core Helpers
;;; ============================================================================

(defun vimban--run-cmd (cmd)
  "Run shell CMD synchronously, return output as a string.
Strips trailing whitespace from the result."
  (string-trim-right (shell-command-to-string cmd)))

(defun vimban--run-cmd-lines (cmd)
  "Run shell CMD synchronously, return output as a list of lines.
Empty trailing lines are filtered out."
  (let ((output (vimban--run-cmd cmd)))
    (if (string-empty-p output)
        nil
      (split-string output "\n" t "[ \t\n]+"))))

(defun vimban--open-popup (content &optional title)
  "Display CONTENT (list of strings) in a read-only markdown popup buffer.
Uses Doom's popup display rules. If TITLE is non-nil, use it as the buffer name.
Maps `q' to quit the popup in both normal and motion Evil states."
  (let* ((buf-name (or title "*vimban-popup*"))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join content "\n")))
      (goto-char (point-min))
      (markdown-mode)
      (read-only-mode 1)
      (evil-local-set-key 'normal (kbd "q") #'quit-window)
      (evil-local-set-key 'motion (kbd "q") #'quit-window))
    (pop-to-buffer buf
                   '((display-buffer-in-side-window)
                     (side . bottom)
                     (slot . 0)
                     (window-height . 0.6)))
    buf))

(defun vimban--get-ticket-id ()
  "Detect ticket ID from context.
Search order:
  1. YAML frontmatter `id:' field in the first 30 lines
  2. Transclusion `![[path]]' on current line -- extract ID from path or
     read the referenced file's frontmatter
  3. Bare ticket ID pattern `[A-Z]+-[0-9]+' on current line
Return nil if nothing is found."
  (let ((ticket-id nil))
    ;; 1. Check frontmatter (first 30 lines)
    (save-excursion
      (goto-char (point-min))
      (let ((limit (save-excursion
                     (forward-line 30)
                     (point))))
        (while (and (not ticket-id)
                    (< (point) limit)
                    (not (eobp)))
          (when (looking-at "^id:\\s-*\"?\\([^\"\n]+\\)\"?")
            (setq ticket-id (string-trim (match-string 1))))
          (forward-line 1))))
    ;; 2. Check current line for transclusion ![[path]]
    (unless ticket-id
      (let ((line (thing-at-point 'line t)))
        (when (and line (string-match "!\\[\\[\\([^]]+\\)\\]\\]" line))
          (let ((path (match-string 1 line)))
            ;; Try to extract ticket ID from path string
            (if (string-match "\\([A-Z]+-[0-9]+\\)" path)
                (setq ticket-id (match-string 1 path))
              ;; If path is a file, read its frontmatter for id:
              (let ((full-path (expand-file-name path vimban-notes-dir)))
                (when (file-readable-p full-path)
                  (with-temp-buffer
                    (insert-file-contents full-path nil 0 2048)
                    (goto-char (point-min))
                    (let ((limit (save-excursion
                                  (forward-line 30)
                                  (point))))
                      (while (and (not ticket-id)
                                  (< (point) limit)
                                  (not (eobp)))
                        (when (looking-at "^id:\\s-*\"?\\([^\"\n]+\\)\"?")
                          (setq ticket-id (string-trim (match-string 1))))
                        (forward-line 1)))))))))))
    ;; 3. Check current line for bare ticket ID
    (unless ticket-id
      (let ((line (thing-at-point 'line t)))
        (when (and line (string-match "\\([A-Z]+-[0-9]+\\)" line))
          (setq ticket-id (match-string 1 line)))))
    ticket-id))

(defun vimban--get-ticket-filepath (ticket-id)
  "Get the relative filepath for TICKET-ID by querying vimban JSON output.
Run `vimban -f json show TICKET-ID' and parse the `filepath' field.
Return nil on error or if the ticket is not found."
  (condition-case nil
      (let* ((cmd (format "vimban -f json show %s 2>/dev/null"
                          (shell-quote-argument ticket-id)))
             (json-str (vimban--run-cmd cmd))
             (data (json-read-from-string json-str)))
        (cdr (assq 'filepath data)))
    (error nil)))

;;; ============================================================================
;;; Dashboard Functions
;;; ============================================================================

(defun vimban-dashboard (&optional type)
  "Display the vimban dashboard in a popup.
TYPE defaults to \"daily\".  Runs `vimban -f md dashboard TYPE'."
  (interactive)
  (let* ((dtype (or type "daily"))
         (cmd (format "vimban -f md dashboard %s"
                      (shell-quote-argument dtype)))
         (content (vimban--run-cmd-lines cmd))
         (title (format "*Vimban Dashboard (%s)*" dtype)))
    (vimban--open-popup (or content '("No dashboard data.")) title)))

(defun vimban-dashboard-picker ()
  "Interactively pick a dashboard type and display it.
For \"project\" dashboards, prompt for the project name.
For \"person\" dashboards, prompt for the person name."
  (interactive)
  (let ((dtype (completing-read "Dashboard type: " vimban-dashboard-types nil t)))
    (cond
     ((string= dtype "project")
      (let ((project (read-string "Project: ")))
        (when (and project (not (string-empty-p project)))
          (let* ((cmd (format "vimban -f md dashboard project --project %s"
                              (shell-quote-argument project)))
                 (content (vimban--run-cmd-lines cmd)))
            (vimban--open-popup (or content '("No data."))
                                (format "*Vimban Project: %s*" project))))))
     ((string= dtype "person")
      (let ((person (read-string "Person: ")))
        (when (and person (not (string-empty-p person)))
          (let* ((cmd (format "vimban -f md dashboard person --person %s"
                              (shell-quote-argument person)))
                 (content (vimban--run-cmd-lines cmd)))
            (vimban--open-popup (or content '("No data."))
                                (format "*Vimban Person: %s*" person))))))
     (t (vimban-dashboard dtype)))))

;;; ============================================================================
;;; Kanban Functions
;;; ============================================================================

(defun vimban--kanban-get-ticket-under-cursor ()
  "Extract a ticket ID (e.g. PROJ-00001) from the current line.
Return nil if no ticket ID pattern is found."
  (let ((line (thing-at-point 'line t)))
    (when (and line (string-match "\\([A-Z]+-[0-9]+\\)" line))
      (match-string 1 line))))

(defun vimban--kanban-refresh (buf)
  "Refresh the kanban board content in buffer BUF."
  (let ((content (vimban--run-cmd "vimban -f md kanban --mine")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (insert content)
        (goto-char (min pos (point-max)))))))

(defun vimban-kanban-board ()
  "Display an interactive kanban board in a popup buffer.
Evil normal mode keybindings:
  RET - show ticket float for ticket under cursor
  o   - open ticket file
  m   - move ticket status
  c   - add comment
  e   - edit ticket
  r   - refresh board
  q   - kill buffer
  ?   - show help"
  (interactive)
  (let* ((buf-name "*Vimban Kanban*")
         (buf (get-buffer-create buf-name))
         (content (vimban--run-cmd "vimban -f md kanban --mine")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (goto-char (point-min))
      (markdown-mode)
      (read-only-mode 1)
      ;; Evil keymaps for the kanban buffer
      (evil-local-set-key 'normal (kbd "RET")
                          (lambda ()
                            (interactive)
                            (let ((id (vimban--kanban-get-ticket-under-cursor)))
                              (when id (vimban-show-ticket-float id)))))
      (evil-local-set-key 'normal (kbd "o")
                          (lambda ()
                            (interactive)
                            (let ((id (vimban--kanban-get-ticket-under-cursor)))
                              (when id
                                (let ((path (vimban--get-ticket-filepath id)))
                                  (when (and path (not (string-empty-p path)))
                                    (quit-window)
                                    (find-file (expand-file-name path vimban-notes-dir))))))))
      (evil-local-set-key 'normal (kbd "m")
                          (lambda ()
                            (interactive)
                            (let ((id (vimban--kanban-get-ticket-under-cursor)))
                              (when id
                                (vimban-move-status id)
                                (vimban--kanban-refresh (current-buffer))))))
      (evil-local-set-key 'normal (kbd "c")
                          (lambda ()
                            (interactive)
                            (let ((id (vimban--kanban-get-ticket-under-cursor)))
                              (when id
                                (vimban-add-comment id)
                                (vimban--kanban-refresh (current-buffer))))))
      (evil-local-set-key 'normal (kbd "e")
                          (lambda ()
                            (interactive)
                            (let ((id (vimban--kanban-get-ticket-under-cursor)))
                              (when id
                                (vimban-edit-ticket id)
                                (vimban--kanban-refresh (current-buffer))))))
      (evil-local-set-key 'normal (kbd "r")
                          (lambda ()
                            (interactive)
                            (vimban--kanban-refresh (current-buffer))
                            (message "Kanban board refreshed.")))
      (evil-local-set-key 'normal (kbd "q") #'quit-window)
      (evil-local-set-key 'normal (kbd "?")
                          (lambda ()
                            (interactive)
                            (vimban--open-popup
                             '("# Kanban Keybinds"
                               ""
                               "| Key | Action |"
                               "|-----|--------|"
                               "| RET | View ticket in float |"
                               "| o | Open ticket file |"
                               "| m | Move ticket status |"
                               "| c | Add comment |"
                               "| e | Edit ticket fields |"
                               "| r | Refresh board |"
                               "| q | Close |"
                               "| ? | Show this help |")
                             "*Kanban Help*"))))
    (pop-to-buffer buf
                   '((display-buffer-in-side-window)
                     (side . bottom)
                     (slot . 0)
                     (window-height . 0.7)))))

;;; ============================================================================
;;; Ticket List / Picker Functions
;;; ============================================================================

(defun vimban-list-tickets (&optional filter)
  "List tickets via completing-read and open the selected ticket file.
FILTER defaults to \"--mine\".  Runs `vimban --no-color -f plain list FILTER --no-header'
and presents the output lines for selection."
  (interactive)
  (let* ((filt (or filter "--mine"))
         (cmd (format "vimban --no-color -f plain list %s --no-header" filt))
         (lines (vimban--run-cmd-lines cmd)))
    (if (not lines)
        (message "No tickets found.")
      (let* ((choice (completing-read "Ticket: " lines nil t))
             (ticket-id (when (string-match "\\([A-Z]+-[0-9]+\\)" choice)
                          (match-string 1 choice))))
        (when ticket-id
          (let ((path (vimban--get-ticket-filepath ticket-id)))
            (if (and path (not (string-empty-p path)))
                (find-file (expand-file-name path vimban-notes-dir))
              (message "Could not find filepath for %s" ticket-id))))))))

(defun vimban-list-filtered ()
  "Prompt for a filter preset and list tickets matching it.
Available presets:
  My tickets, In Progress, Blocked, Due This Week,
  Overdue, All tickets, Custom..."
  (interactive)
  (let* ((presets '(("My tickets"    . "--mine")
                    ("In Progress"   . "--mine --status in_progress")
                    ("Blocked"       . "--blocked")
                    ("Due This Week" . "--mine --due-soon 7")
                    ("Overdue"       . "--overdue")
                    ("All tickets"   . "")
                    ("Custom..."     . nil)))
         (labels (mapcar #'car presets))
         (choice (completing-read "Filter: " labels nil t))
         (filter-val (cdr (assoc choice presets))))
    (if filter-val
        (vimban-list-tickets filter-val)
      ;; Custom filter
      (let ((custom (read-string "vimban list ")))
        (when custom
          (vimban-list-tickets custom))))))

;;; ============================================================================
;;; Create / Edit Functions
;;; ============================================================================

(defun vimban-create-ticket ()
  "Interactive wizard to create a new ticket.
Prompts for type, title, priority, and whether to assign to self.
Runs `vimban create TYPE TITLE --priority PRIORITY [--assignee ...]'.
On success, offers to open the newly created ticket file."
  (interactive)
  (let* ((ttype (completing-read "Ticket type: " vimban-ticket-types nil t))
         (title (read-string "Title: "))
         (priority (completing-read "Priority: " vimban-priorities nil t))
         (assign (completing-read "Assign to me? " '("Yes" "No") nil t))
         (cmd (format "vimban create %s %s --priority %s"
                      (shell-quote-argument ttype)
                      (shell-quote-argument title)
                      (shell-quote-argument priority))))
    (when (string-empty-p title)
      (user-error "Title cannot be empty"))
    (when (string= assign "Yes")
      (setq cmd (concat cmd " --assignee \"![[05_people/zach-podbielniak.md]]\"")))
    (let ((result (vimban--run-cmd cmd)))
      (message "%s" result)
      ;; Extract ticket ID from output and offer to open
      (when (string-match "\\([A-Z]+-[0-9]+\\)" result)
        (let ((new-id (match-string 1 result)))
          (when (y-or-n-p (format "Open ticket %s? " new-id))
            (let ((path (vimban--get-ticket-filepath new-id)))
              (when (and path (not (string-empty-p path)))
                (find-file (expand-file-name path vimban-notes-dir))))))))))

(defun vimban-edit-ticket (&optional ticket-id)
  "Edit fields of a ticket identified by TICKET-ID.
If TICKET-ID is nil, detect it from context.
Prompts for which field to edit: Status, Priority, Assignee, Due Date,
Add Tag, Remove Tag, or Progress."
  (interactive)
  (let ((id (or ticket-id (vimban--get-ticket-id))))
    (unless id
      (user-error "No ticket ID found"))
    (let ((field (completing-read "Edit field: "
                                  '("Status" "Priority" "Assignee" "Due Date"
                                    "Add Tag" "Remove Tag" "Progress")
                                  nil t)))
      (cond
       ((string= field "Status")
        (vimban-move-status id))
       ((string= field "Priority")
        (let ((p (completing-read "Priority: " vimban-priorities nil t)))
          (vimban--run-cmd (format "vimban edit %s --priority %s"
                                   (shell-quote-argument id)
                                   (shell-quote-argument p)))
          (message "Priority updated to %s" p)
          (when (buffer-file-name) (revert-buffer t t t))))
       ((string= field "Assignee")
        (let ((a (read-string "Assignee (person ref): ")))
          (when (not (string-empty-p a))
            (vimban--run-cmd (format "vimban edit %s --assignee %s"
                                     (shell-quote-argument id)
                                     (shell-quote-argument a)))
            (message "Assignee updated.")
            (when (buffer-file-name) (revert-buffer t t t)))))
       ((string= field "Due Date")
        (let ((d (read-string "Due date (YYYY-MM-DD or +7d): ")))
          (when (not (string-empty-p d))
            (vimban--run-cmd (format "vimban edit %s --due-date %s"
                                     (shell-quote-argument id)
                                     (shell-quote-argument d)))
            (message "Due date updated.")
            (when (buffer-file-name) (revert-buffer t t t)))))
       ((string= field "Add Tag")
        (let ((tag (read-string "Tag to add: ")))
          (when (not (string-empty-p tag))
            (vimban--run-cmd (format "vimban edit %s --add-tag %s"
                                     (shell-quote-argument id)
                                     (shell-quote-argument tag)))
            (message "Tag '%s' added." tag)
            (when (buffer-file-name) (revert-buffer t t t)))))
       ((string= field "Remove Tag")
        (let ((tag (read-string "Tag to remove: ")))
          (when (not (string-empty-p tag))
            (vimban--run-cmd (format "vimban edit %s --remove-tag %s"
                                     (shell-quote-argument id)
                                     (shell-quote-argument tag)))
            (message "Tag '%s' removed." tag)
            (when (buffer-file-name) (revert-buffer t t t)))))
       ((string= field "Progress")
        (let ((p (read-string "Progress (0-100): ")))
          (when (not (string-empty-p p))
            (vimban--run-cmd (format "vimban edit %s --progress %s"
                                     (shell-quote-argument id)
                                     (shell-quote-argument p)))
            (message "Progress updated to %s%%." p)
            (when (buffer-file-name) (revert-buffer t t t)))))))))

(defun vimban-move-status (&optional ticket-id)
  "Move a ticket to a new status.
If TICKET-ID is nil, detect it from context.
Prompts for the new status.  Adds `--resolve' when moving to \"done\"."
  (interactive)
  (let ((id (or ticket-id (vimban--get-ticket-id))))
    (unless id
      (user-error "No ticket ID found"))
    (let* ((status (completing-read "New status: " vimban-statuses nil t))
           (cmd (format "vimban move %s %s"
                        (shell-quote-argument id)
                        (shell-quote-argument status))))
      (when (string= status "done")
        (setq cmd (concat cmd " --resolve")))
      (vimban--run-cmd cmd)
      (message "Status updated to %s" status)
      (when (buffer-file-name) (revert-buffer t t t)))))

(defun vimban-add-comment (&optional ticket-id)
  "Add a comment to a ticket.
If TICKET-ID is nil, detect it from context.
Prompts for the comment text."
  (interactive)
  (let ((id (or ticket-id (vimban--get-ticket-id))))
    (unless id
      (user-error "No ticket ID found"))
    (let ((text (read-string "Comment: ")))
      (when (not (string-empty-p text))
        (let ((result (vimban--run-cmd
                       (format "vimban comment %s %s"
                               (shell-quote-argument id)
                               (shell-quote-argument text)))))
          (message "%s" result)
          (when (buffer-file-name) (revert-buffer t t t)))))))

(defun vimban-reply-comment (&optional ticket-id)
  "Reply to a specific comment on a ticket.
If TICKET-ID is nil, detect it from context.
Prompts for the comment number and the reply text."
  (interactive)
  (let ((id (or ticket-id (vimban--get-ticket-id))))
    (unless id
      (user-error "No ticket ID found"))
    (let ((reply-to (read-string "Reply to comment #: ")))
      (when (not (string-empty-p reply-to))
        (let ((text (read-string "Reply: ")))
          (when (not (string-empty-p text))
            (let ((result (vimban--run-cmd
                           (format "vimban comment %s %s --reply-to %s"
                                   (shell-quote-argument id)
                                   (shell-quote-argument text)
                                   (shell-quote-argument reply-to)))))
              (message "%s" result)
              (when (buffer-file-name) (revert-buffer t t t)))))))))

;;; ============================================================================
;;; Search Functions
;;; ============================================================================

(defun vimban-search-tickets ()
  "Search tickets by query string.
Prompts for a search query, runs `vimban search QUERY --context-lines 2',
and displays results in a popup.  RET on a line containing a ticket ID
opens the ticket file."
  (interactive)
  (let ((query (read-string "Search query: ")))
    (when (string-empty-p query)
      (user-error "Query cannot be empty"))
    (let* ((cmd (format "vimban search %s --context-lines 2"
                        (shell-quote-argument query)))
           (content (vimban--run-cmd-lines cmd)))
      (if (not content)
          (message "No results found.")
        (let ((buf (vimban--open-popup content
                                       (format "*Vimban Search: %s*" query))))
          (with-current-buffer buf
            (evil-local-set-key 'normal (kbd "RET")
                                (lambda ()
                                  (interactive)
                                  (let* ((line (thing-at-point 'line t))
                                         (id (when (and line
                                                        (string-match "\\([A-Z]+-[0-9]+\\)" line))
                                               (match-string 1 line))))
                                    (when id
                                      (quit-window)
                                      (let ((path (vimban--get-ticket-filepath id)))
                                        (when (and path (not (string-empty-p path)))
                                          (find-file (expand-file-name path vimban-notes-dir))))))))))))))

;;; ============================================================================
;;; People Functions
;;; ============================================================================

(defun vimban-people-dashboard ()
  "Pick a person via completing-read and display their dashboard.
Lists people from `vimban people list', then shows the selected
person's dashboard in a popup."
  (interactive)
  (let* ((cmd "vimban --no-color -f plain people list --no-header")
         (lines (vimban--run-cmd-lines cmd)))
    (if (not lines)
        (message "No people found.")
      (let* ((choice (completing-read "Person: " lines nil t))
             (name (when (string-match "^\\(\\S-+\\)" choice)
                     (match-string 1 choice))))
        (when name
          (let* ((dash-cmd (format "vimban -f md dashboard person --person %s"
                                   (shell-quote-argument name)))
                 (content (vimban--run-cmd-lines dash-cmd)))
            (vimban--open-popup (or content '("No dashboard data."))
                                (format "*Vimban Person: %s*" name))))))))

;;; ============================================================================
;;; Link / Navigation Functions
;;; ============================================================================

(defun vimban-insert-link ()
  "Insert a transclusion link `![[filepath]]' at point.
Lists all tickets via completing-read, resolves the filepath for
the selected ticket, and inserts the link."
  (interactive)
  (let* ((cmd "vimban --no-color -f plain list --no-header")
         (lines (vimban--run-cmd-lines cmd)))
    (if (not lines)
        (message "No tickets found.")
      (let* ((choice (completing-read "Ticket: " lines nil t))
             (ticket-id (when (string-match "\\([A-Z]+-[0-9]+\\)" choice)
                          (match-string 1 choice))))
        (when ticket-id
          (let ((path (vimban--get-ticket-filepath ticket-id)))
            (if (and path (not (string-empty-p path)))
                (insert (format "![[%s]]" path))
              (message "Could not find filepath for %s" ticket-id))))))))

(defun vimban-show-ticket-float (&optional ticket-id)
  "Show ticket details in a popup buffer with interactive keybindings.
If TICKET-ID is nil, detect it from context.
Evil normal mode keybindings:
  RET - open ticket file
  m   - move status
  c   - add comment
  e   - edit ticket
  q   - close
  ?   - help"
  (interactive)
  (let ((id (or ticket-id (vimban--get-ticket-id))))
    (unless id
      (user-error "No ticket ID found"))
    (let* ((cmd (format "vimban -f md show %s" (shell-quote-argument id)))
           (content (vimban--run-cmd-lines cmd))
           (buf (vimban--open-popup (or content '("No ticket data."))
                                    (format "*Vimban: %s*" id))))
      (with-current-buffer buf
        (evil-local-set-key 'normal (kbd "RET")
                            (lambda ()
                              (interactive)
                              (quit-window)
                              (let ((path (vimban--get-ticket-filepath id)))
                                (when (and path (not (string-empty-p path)))
                                  (find-file (expand-file-name path vimban-notes-dir))))))
        (evil-local-set-key 'normal (kbd "m")
                            (lambda ()
                              (interactive)
                              (vimban-move-status id)))
        (evil-local-set-key 'normal (kbd "c")
                            (lambda ()
                              (interactive)
                              (vimban-add-comment id)))
        (evil-local-set-key 'normal (kbd "e")
                            (lambda ()
                              (interactive)
                              (vimban-edit-ticket id)))
        (evil-local-set-key 'normal (kbd "?")
                            (lambda ()
                              (interactive)
                              (vimban--open-popup
                               '("# Ticket Float Keybinds"
                                 ""
                                 "| Key | Action |"
                                 "|-----|--------|"
                                 "| RET | Open ticket file |"
                                 "| m | Move ticket status |"
                                 "| c | Add comment |"
                                 "| e | Edit ticket fields |"
                                 "| q | Close |"
                                 "| ? | Show this help |")
                               "*Ticket Help*")))))))

(defun vimban-goto-ticket ()
  "Navigate to the ticket file referenced on the current line.
Detection order:
  1. Transclusion `![[path]]' under cursor
  2. Markdown link `[title](path)' under cursor
  3. Bare ticket ID pattern `[A-Z]+-[0-9]+'
Opens the ticket file in the current window."
  (interactive)
  (let ((line (thing-at-point 'line t))
        (col (current-column)))
    (cond
     ;; 1. Transclusion: ![[path]]
     ((and line (string-match "!\\[\\[\\([^]]+\\)\\]\\]" line))
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (path (match-string 1 line)))
        (if (and (>= col match-start) (<= col match-end))
            (let ((full (expand-file-name path vimban-notes-dir)))
              (if (file-readable-p full)
                  (find-file full)
                (message "File not readable: %s" full)))
          ;; cursor not on the transclusion, fall through
          (vimban--goto-ticket-fallback line))))
     ;; 2. Markdown link: [title](path)
     ((and line (string-match "\\[\\([^]]+\\)\\](\\([^)]+\\))" line))
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (path (match-string 2 line)))
        (if (and (>= col match-start) (<= col match-end)
                 (not (string-match-p "^https?://" path)))
            (let ((full (expand-file-name path vimban-notes-dir)))
              (if (file-readable-p full)
                  (find-file full)
                (message "File not readable: %s" full)))
          (vimban--goto-ticket-fallback line))))
     ;; 3. Bare ticket ID
     (t (vimban--goto-ticket-fallback line)))))

(defun vimban--goto-ticket-fallback (line)
  "Fallback for `vimban-goto-ticket': extract a bare ticket ID from LINE.
Look up the ticket filepath and open it."
  (if (and line (string-match "\\([A-Z]+-[0-9]+\\)" line))
      (let* ((id (match-string 1 line))
             (path (vimban--get-ticket-filepath id)))
        (if (and path (not (string-empty-p path)))
            (find-file (expand-file-name path vimban-notes-dir))
          (message "Could not find filepath for %s" id)))
    (message "No ticket reference found on current line.")))

(defun vimban-regenerate-section ()
  "Regenerate a VIMBAN section in the current buffer.
Searches backward for `VIMBAN:SECTION_NAME:START' and forward for
`VIMBAN:SECTION_NAME:END', then replaces the content between them
with the output of `vimban dashboard --section NAME --person \"![[filepath]]\"'."
  (interactive)
  (save-excursion
    (let ((start-line nil)
          (end-line nil)
          (section nil))
      ;; Search backward for start marker
      (when (re-search-backward "VIMBAN:\\([A-Z_]+\\):START" nil t)
        (setq start-line (line-number-at-pos))
        (setq section (match-string 1)))
      ;; Search forward for end marker
      (when section
        (goto-char (point))
        (when (re-search-forward "VIMBAN:[A-Z_]+:END" nil t)
          (setq end-line (line-number-at-pos))))
      (if (and start-line end-line section)
          (let* ((file (buffer-file-name))
                 (rel (when file
                        (string-remove-prefix
                         (expand-file-name "~/Documents/notes/")
                         file)))
                 (cmd (format "vimban dashboard --section %s --person \"![[%s]]\""
                              (downcase section)
                              (or rel "")))
                 (new-content (vimban--run-cmd cmd))
                 ;; Calculate buffer positions for the lines between markers
                 (start-pos (save-excursion
                              (goto-char (point-min))
                              (forward-line start-line)
                              (point)))
                 (end-pos (save-excursion
                            (goto-char (point-min))
                            (forward-line (1- end-line))
                            (line-beginning-position))))
            (let ((inhibit-read-only t))
              (delete-region start-pos end-pos)
              (goto-char start-pos)
              (insert new-content "\n"))
            (message "Section %s regenerated." section))
        (message "No VIMBAN section markers found around cursor.")))))

(defun vimban-create-from-selection ()
  "Create a ticket from the currently selected region text.
Prompts for the ticket type and runs `vimban create TYPE SELECTION'."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (title (string-trim (replace-regexp-in-string "\n" " " text)))
         (ttype (completing-read "Ticket type: " vimban-ticket-types nil t)))
    (let ((result (vimban--run-cmd
                   (format "vimban create %s %s"
                           (shell-quote-argument ttype)
                           (shell-quote-argument title)))))
      (message "%s" result)
      (deactivate-mark))))

;;; ============================================================================
;;; Help System
;;; ============================================================================

(defun vimban-show-help ()
  "Display a formatted help buffer showing all vimban keybindings."
  (interactive)
  (vimban--open-popup
   '("# Vimban Keybinds"
     ""
     "## Global Mappings (SPC v)"
     ""
     "| Key | Description |"
     "|-----|-------------|"
     "| `SPC v d` | Daily dashboard |"
     "| `SPC v D` | Dashboard picker (daily/weekly/sprint/project/person) |"
     "| `SPC v k` | Kanban board (interactive) |"
     "| `SPC v l` | List tickets (my tickets) |"
     "| `SPC v L` | List with filter menu |"
     "| `SPC v c` | Create ticket wizard |"
     "| `SPC v C` | Add comment to current ticket |"
     "| `SPC v r` | Reply to comment |"
     "| `SPC v m` | Move ticket status |"
     "| `SPC v e` | Edit ticket fields |"
     "| `SPC v s` | Search tickets |"
     "| `SPC v p` | People picker/dashboard |"
     "| `SPC v i` | Insert transclusion link |"
     "| `SPC v f` | View ticket under cursor in float |"
     "| `SPC v g` | Go to ticket file from transclusion |"
     "| `SPC v R` | Regenerate VIMBAN section |"
     "| `SPC v ?` | Show this help |"
     ""
     "## Visual Mode"
     ""
     "| Key | Description |"
     "|-----|-------------|"
     "| `SPC v c` | Create ticket from selection |"
     ""
     "## Float Window Actions"
     ""
     "| Key | Action |"
     "|-----|--------|"
     "| `q` | Close window |"
     "| `RET` | Open/View ticket |"
     "| `m` | Move status |"
     "| `c` | Add comment |"
     "| `e` | Edit fields |"
     "| `r` | Refresh (kanban) |"
     "| `o` | Open file (kanban) |"
     "| `?` | Show context help |"
     ""
     "## Buffer-Local Mappings (in ticket files)"
     ""
     "| Key | Description |"
     "|-----|-------------|"
     "| `SPC m m` | Move this ticket |"
     "| `SPC m c` | Add comment |"
     "| `SPC m e` | Edit fields |")
   "*Vimban Help*"))

;;; ============================================================================
;;; Autocmds and Buffer-Local Setup
;;; ============================================================================

(defun vimban-setup-buffer-local ()
  "Set up buffer-local Evil keymaps for ticket files.
Checks if the current buffer has an `id:' field in its frontmatter.
If so, binds SPC m m/c/e to move, comment, and edit."
  (save-excursion
    (goto-char (point-min))
    (let ((limit (save-excursion
                   (forward-line 30)
                   (point)))
          (is-ticket nil))
      (while (and (not is-ticket)
                  (< (point) limit)
                  (not (eobp)))
        (when (looking-at "^id:\\s-*\"?[A-Z]+-[0-9]+")
          (setq is-ticket t))
        (forward-line 1))
      (when is-ticket
        (evil-local-set-key 'normal (kbd (concat doom-localleader-key " m")) #'vimban-move-status)
        (evil-local-set-key 'normal (kbd (concat doom-localleader-key " c")) #'vimban-add-comment)
        (evil-local-set-key 'normal (kbd (concat doom-localleader-key " e")) #'vimban-edit-ticket)))))

(defun vimban--maybe-setup-buffer ()
  "Hook function for `find-file-hook'.
Checks if the opened file matches ticket file patterns and sets up
buffer-local keymaps after a short delay to allow the buffer to load."
  (let ((file (buffer-file-name)))
    (when (and file
               (or (string-match-p "/01_projects/.*\\.md$" file)
                   (string-match-p "/02_areas/.*\\.md$" file)
                   (string-match-p "/\\.vimban/.*\\.md$" file)))
      (run-with-timer 0.1 nil
                      (lambda (buf)
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (vimban-setup-buffer-local))))
                      (current-buffer)))))

(defun vimban-setup ()
  "Initialize the vimban module.
Registers `find-file-hook' to detect ticket files and set up
buffer-local keymaps."
  (add-hook 'find-file-hook #'vimban--maybe-setup-buffer))

;;; ============================================================================
;;; Keybindings
;;; ============================================================================

;; Global leader keybindings under SPC v
(map! :leader
      :desc "vimban" "v" nil
      :desc "Dashboard"          "v d" #'vimban-dashboard
      :desc "Dashboard picker"   "v D" #'vimban-dashboard-picker
      :desc "Kanban board"       "v k" #'vimban-kanban-board
      :desc "List tickets"       "v l" #'vimban-list-tickets
      :desc "List filtered"      "v L" #'vimban-list-filtered
      :desc "Create ticket"      "v c" #'vimban-create-ticket
      :desc "Add comment"        "v C" #'vimban-add-comment
      :desc "Reply comment"      "v r" #'vimban-reply-comment
      :desc "Move status"        "v m" #'vimban-move-status
      :desc "Edit ticket"        "v e" #'vimban-edit-ticket
      :desc "Search tickets"     "v s" #'vimban-search-tickets
      :desc "People"             "v p" #'vimban-people-dashboard
      :desc "Insert link"        "v i" #'vimban-insert-link
      :desc "Show ticket"        "v f" #'vimban-show-ticket-float
      :desc "Goto ticket"        "v g" #'vimban-goto-ticket
      :desc "Regenerate section" "v R" #'vimban-regenerate-section
      :desc "Help"               "v ?" #'vimban-show-help)

;; Visual mode keybinding for creating ticket from selection
(map! :leader
      :desc "Create from selection" :v "v c" #'vimban-create-from-selection)

;;; Initialize on load
(vimban-setup)

(provide 'vimban)
;;; vimban.el ends here
