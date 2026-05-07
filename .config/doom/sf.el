;;; sf.el --- Salesforce Service Cloud client (case triage) -*- lexical-binding: t; -*-

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

;; Single-file Doom Emacs client for Salesforce Service Cloud, focused
;; on case triage workflows.  Backed by the `sf' CLI (formerly `sfdx'),
;; which already handles SSO auth and the default target-org selection
;; via `sf config set target-org', so elisp shell-outs are pure
;; `call-process "sf" ...' with no auth or org-selection plumbing.
;;
;; Entry points (under SPC o s):
;;   sf-queues       — list Case-routing queues + open counts
;;   sf-unassigned   — open cases owned by any queue (no individual owner)
;;   sf-case         — view one case by CaseNumber, Id, or pasted URL
;;   sf-my-cases     — open cases owned by the current user
;;
;; See ./.plan/SF-PLAN.org for the full design doc and phase-2 roadmap
;; (posting comments, reassignment, status/field updates).

;;; Code:

(require 'tabulated-list)
(require 'subr-x)
(require 'seq)
(require 'browse-url)
(require 'shr)

(defgroup sf nil
  "Salesforce Service Cloud client."
  :group 'applications
  :prefix "sf-")

(defcustom sf-instance-url nil
  "Salesforce instance URL (e.g. https://acme.my.salesforce.com).
If nil, auto-discovered via `sf org display' on first use and cached."
  :type '(choice (const :tag "Auto-detect" nil) string)
  :group 'sf)

(defcustom sf-cases-list-limit 500
  "Max rows returned by case-list buffers (queue drill-down, unassigned, etc.)."
  :type 'integer
  :group 'sf)

(defcustom sf-case-comments-limit 50
  "Max number of CaseComments shown in a case detail buffer.
Comments are ordered newest-first; older comments past this limit are
not fetched."
  :type 'integer
  :group 'sf)

(defcustom sf-case-emails-limit 50
  "Max number of EmailMessages shown in a case detail buffer.
Emails are ordered newest-first; older messages past this limit are
not fetched."
  :type 'integer
  :group 'sf)

(defcustom sf-case-conversation-limit 100
  "Max number of ConversationEntry rows (chat / messaging) shown per case.
Pulled via a semi-join through MessagingSession; entries are merged into
the Activity timeline alongside CaseComments and EmailMessages.  Older
entries past this limit aren't fetched."
  :type 'integer
  :group 'sf)

(defface sf-internal-comment
  '((((background dark))
     :background "#3d3528" :extend t)
    (t
     :background "#fff5dc" :extend t))
  "Background tint for internal CaseComments (IsPublished = false).
Distinguishes team-only comments from customer-visible ones in
`sf-case-mode'.  Defaults complement the Catppuccin Mocha palette
(warm dark yellow-brown on a `#1e1e2e' base); customize with
\\[customize-face]."
  :group 'sf)

(defface sf-public-comment
  '((((background dark))
     :background "#283a2e" :extend t)
    (t
     :background "#e3f5e1" :extend t))
  "Background tint for outbound public messages.
Used for `IsPublished = true' CaseComments and outgoing EmailMessages
(replies sent from support → customer).  Green tint reads as \"our
voice, customer-facing\" on Catppuccin Mocha."
  :group 'sf)

(defface sf-incoming-email
  '((((background dark))
     :background "#283744" :extend t)
    (t
     :background "#e1ecf5" :extend t))
  "Background tint for incoming EmailMessages (customer → support).
Blue tint distinguishes the customer's voice from our outbound replies
in the activity timeline."
  :group 'sf)

(defcustom sf-case-detail-fields
  '("CaseNumber" "Subject" "Status" "Priority"
    "Owner.Name" "Account.Name" "Contact.Name" "ContactEmail"
    "RecordType.Name" "CreatedDate" "LastModifiedDate" "IsEscalated"
    "Severity__c" "Internal_Priority__c" "Status_Category__c"
    "Sub_Status__c" "Customer_Type__c" "Product_Area__c"
    "Tier_2_Support_Team__c" "Hot_Ticket__c" "Escalation_Status__c"
    "Last_Re_Opened_Date_Time__c" "Re_Open_Count__c" "Issue__c")
  "Fields shown in the `sf-case-mode' detail buffer, top-to-bottom.
Each entry is a SOQL field path; dotted paths like \"Owner.Name\"
traverse relationships and are walked into nested alists at render time."
  :type '(repeat string)
  :group 'sf)

;;; ----------------------------------------------------------- backend

(defun sf--call (args)
  "Run `sf' with ARGS plus --json, return the parsed `.result' value.
ARGS is a list of strings.  Errors via `user-error' on non-zero exit
or non-JSON output."
  (with-temp-buffer
    (let* ((exit (apply #'call-process "sf" nil t nil
                        (append args '("--json"))))
           (output (buffer-string))
           (parsed (condition-case _
                       (progn (goto-char (point-min))
                              (json-parse-buffer
                               :object-type 'alist
                               :null-object nil
                               :false-object :false))
                     (error nil))))
      (cond
       ((not (zerop exit))
        (user-error "sf failed (exit %d): %s" exit
                    (or (and parsed (alist-get 'message parsed)) output)))
       ((null parsed)
        (user-error "sf returned non-JSON output: %s" output))
       (t (alist-get 'result parsed))))))

(defun sf--query (soql)
  "Run a SOQL query, return the records vector."
  (alist-get 'records (sf--call (list "data" "query" "-q" soql))))

(defun sf--instance-url ()
  "Return the Salesforce instance URL, caching after first call."
  (or sf-instance-url
      (setq sf-instance-url
            (alist-get 'instanceUrl (sf--call '("org" "display"))))))

(defun sf--current-username ()
  "Return the username of the default target-org's authenticated user."
  (alist-get 'username (sf--call '("org" "display"))))

(defun sf--case-url (id)
  "Return the Lightning UI URL for case ID."
  (format "%s/lightning/r/Case/%s/view" (sf--instance-url) id))

(defun sf--escape (s)
  "Backslash-escape single quotes in S for embedding in a SOQL string literal."
  (replace-regexp-in-string "'" "\\\\'" s))

;;; --------------------------------------------------- detection helper

(defun sf--detect-case-key (input)
  "Classify INPUT as a Salesforce case identifier.
Returns a cons (TYPE . VALUE) where TYPE is one of:
  `id'         — 15- or 18-character Salesforce Id (must contain a letter)
  `casenumber' — pure digits (CaseNumber field).
URL inputs are unwrapped to their Id segment.  Errors on no match."
  (let ((s (string-trim input)))
    (cond
     ;; Full Salesforce URL — extract the /Case/<id>/ segment
     ((string-match "/Case/\\([a-zA-Z0-9]\\{15,18\\}\\)" s)
      (cons 'id (match-string 1 s)))
     ;; Bare 15/18-char Id (must contain at least one letter)
     ((and (string-match-p "\\`[a-zA-Z0-9]\\{15,18\\}\\'" s)
           (string-match-p "[a-zA-Z]" s))
      (cons 'id s))
     ;; Pure digits → CaseNumber
     ((string-match-p "\\`[0-9]+\\'" s)
      (cons 'casenumber s))
     (t (user-error "Not a recognizable case Id, CaseNumber, or URL: %s" s)))))

;;; --------------------------------------------------- value formatting

(defun sf--render-html-to-text (html)
  "Render HTML string HTML to formatted text using `shr'.
Returns the rendered string with shr's text properties (faces, bullets,
links) intact, or nil if libxml isn't available or parsing fails so the
caller can fall back to plain rendering."
  (when (and (stringp html) (not (string-empty-p html))
             (fboundp 'libxml-parse-html-region))
    (with-temp-buffer
      (insert html)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (when dom
          (erase-buffer)
          (let ((shr-width 80)
                (shr-use-fonts nil))
            (shr-insert-document dom))
          (string-trim-right (buffer-string) "[\n\r]+"))))))

(defun sf--format-date (iso)
  "Trim a Salesforce ISO timestamp ISO to YYYY-MM-DD HH:MM."
  (if (and (stringp iso) (>= (length iso) 16))
      (concat (substring iso 0 10) " " (substring iso 11 16))
    (or iso "")))

(defun sf--format-value (v)
  "Stringify a JSON value V for display in a case detail buffer."
  (cond
   ((null v)        "")
   ((eq v :false)   "false")
   ((eq v t)        "true")
   ((stringp v)
    (if (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" v)
        (sf--format-date v)
      v))
   ((numberp v)     (number-to-string v))
   (t               (prin1-to-string v))))

(defun sf--nested-get (record path)
  "Walk PATH (list of field-name strings) through nested alists in RECORD."
  (let ((cur record))
    (dolist (key path cur)
      (when cur
        (setq cur (alist-get (intern key) cur))))))

;;; ------------------------------------------------------- help buffers

(defun sf--show-keys (title rows)
  "Display a help buffer named *TITLE-keys* listing keybinding ROWS.
ROWS is a list of (KEY DESCRIPTION) pairs."
  (let ((buf-name (format "*%s-keys*" title)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "%s keybindings\n" title)
                            'face '(:weight bold)))
        (insert (make-string (+ (length title) 13) ?=) "\n\n")
        (dolist (row rows)
          (insert (format "  %-8s %s\n" (car row) (cadr row))))
        (insert "\n  q        quit this help buffer\n"))
      (goto-char (point-min))
      (special-mode))
    (display-buffer buf-name
                    '(display-buffer-at-bottom
                      . ((window-height . fit-window-to-buffer))))))

(defun sf-queues-show-keys ()
  "Display keybindings for `sf-queues-mode'."
  (interactive)
  (sf--show-keys
   "sf-queues"
   '(("RET" "drill into selected queue's open cases")
     ("g"   "refresh queue list + open-case counts")
     ("S"   "sort by column at point (tabulated-list)")
     ("?"   "this help"))))

(defun sf-cases-show-keys ()
  "Display keybindings for `sf-cases-mode'."
  (interactive)
  (sf--show-keys
   "sf-cases"
   '(("RET" "open case detail buffer for case at point")
     ("o"   "open case in browser (Lightning UI)")
     ("g"   "refresh — re-runs this buffer's source query")
     ("S"   "sort by column at point (tabulated-list)")
     ("?"   "this help"))))

(defun sf-case-show-keys ()
  "Display keybindings for `sf-case-mode'."
  (interactive)
  (sf--show-keys
   "sf-case"
   '(("g" "re-fetch and re-render this case (incl. comments)")
     ("o" "open case in browser (Lightning UI)")
     ("y" "copy case URL to kill ring")
     ("c" "jump to the Activity section (comments + emails)")
     ("?" "this help"))))

;;; ----------------------------------------------------- queues mode

(defvar sf-queues-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'sf-queues-drill)
    (define-key map "g"   #'sf-queues-refresh)
    (define-key map "?"   #'sf-queues-show-keys)
    (define-key map "q"   #'quit-window)
    map)
  "Keymap for `sf-queues-mode'.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal sf-queues-mode-map
    (kbd "RET") #'sf-queues-drill
    "g"         #'sf-queues-refresh
    "?"         #'sf-queues-show-keys
    "q"         #'quit-window))

(defun sf--queues-numeric-cmp (a b)
  "Numeric sort comparator on the \"Open\" column."
  (< (string-to-number (aref (cadr a) 1))
     (string-to-number (aref (cadr b) 1))))

(define-derived-mode sf-queues-mode tabulated-list-mode "SF-Queues"
  "Major mode for browsing Salesforce Case-routing queues."
  (setq tabulated-list-format
        `[("Queue Name"     40 t)
          ("Open"            6 ,#'sf--queues-numeric-cmp :right-align t)
          ("Developer Name" 30 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Queue Name" . nil))
  (tabulated-list-init-header))

(defun sf--queues-fetch ()
  "Fetch queues + open-case counts, return tabulated-list entries."
  (let* ((queues (sf--query
                  "SELECT Queue.Id, Queue.Name, Queue.DeveloperName
                   FROM QueueSobject WHERE SobjectType = 'Case'
                   ORDER BY Queue.Name"))
         (counts (sf--query
                  "SELECT OwnerId, COUNT(Id) cnt
                   FROM Case
                   WHERE Owner.Type = 'Queue' AND IsClosed = false
                   GROUP BY OwnerId"))
         (tbl (make-hash-table :test 'equal)))
    (mapc (lambda (r)
            (puthash (alist-get 'OwnerId r) (alist-get 'cnt r) tbl))
          (append counts nil))
    (mapcar
     (lambda (r)
       (let* ((q    (alist-get 'Queue r))
              (id   (alist-get 'Id q))
              (name (or (alist-get 'Name q) ""))
              (dev  (or (alist-get 'DeveloperName q) ""))
              (cnt  (or (gethash id tbl) 0)))
         (list id (vector name (number-to-string cnt) dev))))
     (append queues nil))))

(defun sf-queues-refresh ()
  "Re-fetch and redraw the queues buffer."
  (interactive)
  (setq tabulated-list-entries (sf--queues-fetch))
  (tabulated-list-print t))

(defun sf-queues-drill ()
  "Open a case-list buffer for the queue at point."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (qname (and entry (aref entry 0))))
    (unless (and qname (not (string-empty-p qname)))
      (user-error "No queue at point"))
    (sf--open-cases-buffer
     (format "*sf-cases: queue %s*" qname)
     (let ((q qname))
       (lambda ()
         (sf--query
          (format
           "SELECT Id, CaseNumber, Subject, Status, Priority,
                   Owner.Name, Account.Name, CreatedDate
            FROM Case
            WHERE Owner.Type = 'Queue' AND Owner.Name = '%s'
              AND IsClosed = false
            ORDER BY Priority, CreatedDate DESC
            LIMIT %d"
           (sf--escape q) sf-cases-list-limit)))))))

;;;###autoload
(defun sf-queues ()
  "Open the Salesforce queues overview buffer."
  (interactive)
  (let ((buf (get-buffer-create "*sf-queues*")))
    (with-current-buffer buf
      (sf-queues-mode)
      (sf-queues-refresh))
    (pop-to-buffer buf)))

;;; ----------------------------------------------------- cases list mode

(defvar sf-cases-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'sf-cases-open-detail)
    (define-key map "g"   #'sf-cases-refresh)
    (define-key map "o"   #'sf-cases-browse-url)
    (define-key map "?"   #'sf-cases-show-keys)
    (define-key map "q"   #'quit-window)
    map)
  "Keymap for `sf-cases-mode'.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal sf-cases-mode-map
    (kbd "RET") #'sf-cases-open-detail
    "g"         #'sf-cases-refresh
    "o"         #'sf-cases-browse-url
    "?"         #'sf-cases-show-keys
    "q"         #'quit-window))

(define-derived-mode sf-cases-mode tabulated-list-mode "SF-Cases"
  "Major mode for browsing Salesforce cases."
  (setq tabulated-list-format
        [("Case #"  10 t)
         ("Pri"      8 t)
         ("Status"  12 t)
         ("Owner"   20 t)
         ("Account" 25 t)
         ("Subject" 60 t)
         ("Created" 18 t)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defvar-local sf--cases-source nil
  "Closure (no args) returning a records vector for this buffer.
Re-invoked by `sf-cases-refresh'.")

(defun sf--cases-render (records)
  "Convert RECORDS vector to tabulated-list entries."
  (mapcar
   (lambda (r)
     (let* ((id       (alist-get 'Id r))
            (case-num (or (alist-get 'CaseNumber r) ""))
            (subject  (or (alist-get 'Subject r) ""))
            (status   (or (alist-get 'Status r) ""))
            (priority (or (alist-get 'Priority r) ""))
            (owner    (or (alist-get 'Name (alist-get 'Owner r)) ""))
            (account  (or (alist-get 'Name (alist-get 'Account r)) ""))
            (created  (sf--format-date (alist-get 'CreatedDate r))))
       (list id (vector case-num priority status owner account subject created))))
   (append records nil)))

(defun sf-cases-refresh ()
  "Re-run `sf--cases-source' for this buffer and redraw."
  (interactive)
  (unless sf--cases-source
    (user-error "No source closure for this cases buffer"))
  (setq tabulated-list-entries (sf--cases-render (funcall sf--cases-source)))
  (tabulated-list-print t))

(defun sf-cases-open-detail ()
  "Open the case detail buffer for the case at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No case at point"))
    (sf--open-case-by-id id)))

(defun sf-cases-browse-url ()
  "Open the case at point in the browser."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id (user-error "No case at point"))
    (browse-url-xdg-open (sf--case-url id))))

(defun sf--open-cases-buffer (buf-name source-fn)
  "Show a cases buffer named BUF-NAME, populated by SOURCE-FN closure."
  (let ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (sf-cases-mode)
      (setq sf--cases-source source-fn)
      (sf-cases-refresh))
    (pop-to-buffer buf)))

;;;###autoload
(defun sf-unassigned ()
  "Show all open cases sitting in queues (queue-owned, not closed)."
  (interactive)
  (sf--open-cases-buffer
   "*sf-cases: unassigned*"
   (lambda ()
     (sf--query
      (format
       "SELECT Id, CaseNumber, Subject, Status, Priority,
               Owner.Name, Account.Name, CreatedDate
        FROM Case
        WHERE Owner.Type = 'Queue' AND IsClosed = false
        ORDER BY Priority, CreatedDate DESC
        LIMIT %d"
       sf-cases-list-limit)))))

;;;###autoload
(defun sf-my-cases ()
  "Show open cases owned by the default target-org's authenticated user."
  (interactive)
  (sf--open-cases-buffer
   "*sf-cases: mine*"
   (lambda ()
     (let ((me (sf--current-username)))
       (sf--query
        (format
         "SELECT Id, CaseNumber, Subject, Status, Priority,
                 Owner.Name, Account.Name, CreatedDate
          FROM Case
          WHERE Owner.Username = '%s' AND IsClosed = false
          ORDER BY Priority, CreatedDate DESC
          LIMIT %d"
         (sf--escape me) sf-cases-list-limit))))))

;;; ----------------------------------------------------- case detail

(defvar sf-case-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'sf-case-refresh)
    (define-key map "o" #'sf-case-browse-url)
    (define-key map "y" #'sf-case-yank-url)
    (define-key map "c" #'sf-case-jump-to-activity)
    (define-key map "?" #'sf-case-show-keys)
    map)
  "Keymap for `sf-case-mode'.")

(with-eval-after-load 'evil
  (evil-define-key* 'normal sf-case-mode-map
    "g" #'sf-case-refresh
    "o" #'sf-case-browse-url
    "y" #'sf-case-yank-url
    "c" #'sf-case-jump-to-activity
    "?" #'sf-case-show-keys
    "q" #'quit-window))

(define-derived-mode sf-case-mode special-mode "SF-Case"
  "Major mode for viewing a single Salesforce case.")

(defvar-local sf--case-id nil "Salesforce Id of the case rendered in this buffer.")

(defun sf--fetch-case-detail (where-clause)
  "Run case-detail SOQL with WHERE-CLAUSE, augment with ConversationEntries.
Returns the augmented Case record alist with `ConversationEntries' added
as a vector under that key (possibly empty).  Two SOQL roundtrips: one
for the case + standard children (CaseComments, EmailMessages), one for
ConversationEntry via MessagingSession semi-join.

Falls back gracefully if the second query fails (e.g. the org doesn't
have Messaging enabled): emits a non-blocking `message' and renders the
case without conversation entries."
  (let ((recs (sf--query (sf--case-detail-soql where-clause))))
    (when (zerop (length recs))
      (user-error "No case found"))
    (let* ((rec     (aref recs 0))
           (case-id (alist-get 'Id rec))
           (entries (condition-case err
                        (sf--query
                         (format
                          "SELECT Id, ActorName, ActorType, MessageContent,
                                  MessageType, ClientTimestamp
                           FROM ConversationEntry
                           WHERE EntryType = 'Message'
                             AND ConversationId IN
                                 (SELECT Id FROM MessagingSession WHERE CaseId = '%s')
                           ORDER BY ClientTimestamp DESC
                           LIMIT %d"
                          (sf--escape case-id) sf-case-conversation-limit))
                      (error
                       (message "sf: ConversationEntry query skipped: %s"
                                (error-message-string err))
                       []))))
      (cons (cons 'ConversationEntries entries) rec))))

(defun sf--case-detail-soql (where-clause)
  "Build the case detail SOQL given WHERE-CLAUSE (e.g. \"Id = '500…'\").
Includes subqueries for CaseComments (capped by `sf-case-comments-limit')
and EmailMessages (capped by `sf-case-emails-limit'), both newest first.
Also pulls SuppliedName/SuppliedEmail/CreatedBy.Name and the rich-text
`DescriptionRichText__c' custom field for attributing and rendering the
synthetic case-opening Activity entry."
  (format "SELECT Id, Description, DescriptionRichText__c,
                  SuppliedName, SuppliedEmail, CreatedBy.Name, %s,
                  (SELECT Id, CommentBody, CreatedBy.Name, CreatedDate, IsPublished
                   FROM CaseComments ORDER BY CreatedDate DESC LIMIT %d),
                  (SELECT Id, Subject, FromName, FromAddress, ToAddress,
                          TextBody, MessageDate, CreatedDate, Incoming, Status
                   FROM EmailMessages ORDER BY MessageDate DESC LIMIT %d)
           FROM Case WHERE %s LIMIT 1"
          (string-join sf-case-detail-fields ", ")
          sf-case-comments-limit
          sf-case-emails-limit
          where-clause))

(defun sf--collect-activity (rec)
  "Return a list of activity items from REC, sorted newest-first.
Each item is a plist (:type TYPE :date STRING :data ALIST) where TYPE
is `initial', `comment', `email', or `message'."
  (let ((comments (alist-get 'records (alist-get 'CaseComments    rec)))
        (emails   (alist-get 'records (alist-get 'EmailMessages   rec)))
        (entries  (alist-get 'ConversationEntries rec))
        (rich     (alist-get 'DescriptionRichText__c rec))
        (plain    (alist-get 'Description rec))
        (acc nil))
    (when (or (and (stringp rich)  (not (string-empty-p rich)))
              (and (stringp plain) (not (string-empty-p plain))))
      (push (list :type 'initial
                  :date (or (alist-get 'CreatedDate rec) "")
                  :data rec)
            acc))
    (dotimes (i (length comments))
      (let ((c (aref comments i)))
        (push (list :type 'comment
                    :date (or (alist-get 'CreatedDate c) "")
                    :data c)
              acc)))
    (dotimes (i (length emails))
      (let ((e (aref emails i)))
        (push (list :type 'email
                    :date (or (alist-get 'MessageDate e)
                              (alist-get 'CreatedDate e) "")
                    :data e)
              acc)))
    (dotimes (i (length entries))
      (let ((e (aref entries i)))
        (push (list :type 'message
                    :date (or (alist-get 'ClientTimestamp e) "")
                    :data e)
              acc)))
    (sort acc (lambda (a b) (string> (plist-get a :date) (plist-get b :date))))))

(defun sf--render-comment-item (c)
  "Render a single CaseComment alist C into the current buffer."
  (let* ((author    (or (alist-get 'Name (alist-get 'CreatedBy c)) "?"))
         (created   (sf--format-date (alist-get 'CreatedDate c)))
         (internal  (eq (alist-get 'IsPublished c) :false))
         (visibility  (if internal "internal" "public"))
         (block-face  (if internal 'sf-internal-comment 'sf-public-comment))
         (header-face (if internal
                          '(:inherit shadow :slant italic)
                        'shadow))
         (body      (or (alist-get 'CommentBody c) ""))
         (start     (point)))
    (insert "\n"
            (propertize (format "── comment · %s · %s · %s ──\n"
                                author created visibility)
                        'face header-face))
    (insert body
            (if (string-suffix-p "\n" body) "" "\n"))
    (add-face-text-property start (point) block-face)))

(defun sf--render-email-item (e)
  "Render a single EmailMessage alist E into the current buffer."
  (let* ((from-name (or (alist-get 'FromName e) ""))
         (from-addr (or (alist-get 'FromAddress e) ""))
         (to-addr   (or (alist-get 'ToAddress e) ""))
         (subject   (or (alist-get 'Subject e) "(no subject)"))
         (date      (sf--format-date
                     (or (alist-get 'MessageDate e)
                         (alist-get 'CreatedDate e))))
         (incoming  (eq (alist-get 'Incoming e) t))
         (direction (if incoming "incoming" "outgoing"))
         (block-face (if incoming 'sf-incoming-email 'sf-public-comment))
         (sender    (cond
                     ((and (not (string-empty-p from-name))
                           (not (string-empty-p from-addr)))
                      (format "%s <%s>" from-name from-addr))
                     ((not (string-empty-p from-name)) from-name)
                     ((not (string-empty-p from-addr)) from-addr)
                     (t "?")))
         (body      (replace-regexp-in-string
                     "\r" "" (or (alist-get 'TextBody e) "")))
         (start     (point)))
    (insert "\n"
            (propertize (format "── email · %s · %s · %s ──\n"
                                sender date direction)
                        'face 'shadow))
    (insert (propertize (format "  Subject: %s\n" subject) 'face 'shadow))
    (when (and (not incoming) (not (string-empty-p to-addr)))
      (insert (propertize (format "  To: %s\n" to-addr) 'face 'shadow)))
    (insert "\n")
    (insert (if (string-empty-p body) "(empty body)\n" body)
            (if (or (string-empty-p body) (string-suffix-p "\n" body)) "" "\n"))
    (add-face-text-property start (point) block-face)))

(defun sf--render-initial-item (rec)
  "Render the case-opening Activity entry, sourced from the Description.
Prefers `DescriptionRichText__c' (rendered via `shr' since it's HTML),
falls back to plain `Description'.  Attributed to the case opener via
SuppliedName/SuppliedEmail (or Contact.Name / CreatedBy.Name fallbacks)."
  (let* ((rich    (alist-get 'DescriptionRichText__c rec))
         (plain   (alist-get 'Description rec))
         (rendered (cond
                    ((and (stringp rich) (not (string-empty-p rich)))
                     (or (sf--render-html-to-text rich)
                         ;; libxml unavailable — strip tags as a fallback
                         (replace-regexp-in-string "<[^>]+>" "" rich)))
                    ((and (stringp plain) (not (string-empty-p plain)))
                     (replace-regexp-in-string "\r" "" plain))
                    (t "(empty)")))
         (created (sf--format-date (alist-get 'CreatedDate rec)))
         (s-name  (alist-get 'SuppliedName rec))
         (s-mail  (alist-get 'SuppliedEmail rec))
         (c-name  (alist-get 'Name (alist-get 'Contact rec)))
         (creator (alist-get 'Name (alist-get 'CreatedBy rec)))
         (sender  (cond
                   ((and s-name s-mail
                         (not (string-empty-p s-name))
                         (not (string-empty-p s-mail)))
                    (format "%s <%s>" s-name s-mail))
                   ((and s-name (not (string-empty-p s-name))) s-name)
                   ((and s-mail (not (string-empty-p s-mail))) s-mail)
                   ((and c-name (not (string-empty-p c-name))) c-name)
                   ((and creator (not (string-empty-p creator))) creator)
                   (t "(unknown)")))
         (start   (point)))
    (insert "\n"
            (propertize (format "── case opened · %s · %s ──\n" sender created)
                        'face 'shadow))
    (insert rendered (if (string-suffix-p "\n" rendered) "" "\n"))
    (add-face-text-property start (point) 'sf-incoming-email)))

(defun sf--render-message-item (e)
  "Render a single ConversationEntry alist E (chat / messaging message).
EndUser → blue tint (`sf-incoming-email'), Agent/Bot → green tint
(`sf-public-comment'), System → no tint."
  (let* ((actor      (or (alist-get 'ActorName e) "?"))
         (actor-type (or (alist-get 'ActorType e) ""))
         (msg-type   (or (alist-get 'MessageType e) ""))
         (content    (or (alist-get 'MessageContent e) ""))
         (timestamp  (sf--format-date (alist-get 'ClientTimestamp e)))
         (direction  (cond ((string= actor-type "EndUser") "incoming")
                           ((string= actor-type "Agent")   "outgoing")
                           ((string= actor-type "Bot")     "bot")
                           ((string= actor-type "System")  "system")
                           (t (downcase actor-type))))
         (block-face (cond ((string= actor-type "EndUser") 'sf-incoming-email)
                           ((string= actor-type "Agent")   'sf-public-comment)
                           ((string= actor-type "Bot")     'sf-public-comment)
                           (t nil)))
         (extra      (if (or (string-empty-p msg-type)
                             (string= msg-type "Text"))
                         ""
                       (format " · %s" (downcase msg-type))))
         (start      (point)))
    (insert "\n"
            (propertize (format "── message · %s · %s · %s%s ──\n"
                                actor timestamp direction extra)
                        'face 'shadow))
    (insert (if (string-empty-p content) "(empty message)\n" content)
            (if (or (string-empty-p content) (string-suffix-p "\n" content))
                "" "\n"))
    (when block-face
      (add-face-text-property start (point) block-face))))

(defun sf--render-activity (rec)
  "Render a combined Activity timeline (comments + emails + messages) from REC.
Inserts an `Activity' header followed by one block per item, newest first."
  (let* ((items (sf--collect-activity rec))
         (n     (length items)))
    (insert "\n" (propertize "Activity" 'face '(:weight bold)))
    (cond
     ((zerop n) (insert " (none)\n"))
     (t
      (insert (format " (%d, newest first)\n" n))
      (insert (make-string 72 ?─) "\n")
      (dolist (item items)
        (pcase (plist-get item :type)
          ('initial (sf--render-initial-item (plist-get item :data)))
          ('comment (sf--render-comment-item (plist-get item :data)))
          ('email   (sf--render-email-item   (plist-get item :data)))
          ('message (sf--render-message-item (plist-get item :data)))))))))

(defun sf--render-case (rec)
  "Render alist REC as a case detail buffer; pop to it."
  (let* ((id       (alist-get 'Id rec))
         (case-num (or (alist-get 'CaseNumber rec) "?"))
         (subject  (or (alist-get 'Subject rec) ""))
         (buf      (get-buffer-create
                    (format "*sf-case: %s*" case-num))))
    (with-current-buffer buf
      (sf-case-mode)
      (setq-local sf--case-id id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Case %s — %s\n" case-num subject)
                            'face '(:weight bold :height 1.2)))
        (insert (make-string 72 ?─) "\n\n")
        (dolist (field-path sf-case-detail-fields)
          (let* ((path (split-string field-path "\\."))
                 (val  (sf--nested-get rec path)))
            (insert (propertize (format "%-32s " field-path)
                                'face 'font-lock-keyword-face))
            (insert (sf--format-value val) "\n")))
        (let ((descr (alist-get 'Description rec)))
          (when (and descr (stringp descr) (not (string-empty-p descr)))
            (insert "\n" (propertize "Description\n" 'face '(:weight bold)))
            (insert (make-string 72 ?─) "\n")
            (insert descr (if (string-suffix-p "\n" descr) "" "\n"))))
        (sf--render-activity rec))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun sf--open-case-by-id (id)
  "Fetch the case with Id ID and render it (incl. ConversationEntries)."
  (sf--render-case (sf--fetch-case-detail (format "Id = '%s'" (sf--escape id)))))

;;;###autoload
(defun sf-case (input)
  "Open a case detail buffer.
INPUT is a CaseNumber, Salesforce Id, or full Lightning URL.
Auto-detects type via `sf--detect-case-key'."
  (interactive (list (read-string "Case (number / Id / URL): ")))
  (let* ((kv    (sf--detect-case-key input))
         (where (pcase (car kv)
                  ('id         (format "Id = '%s'"         (sf--escape (cdr kv))))
                  ('casenumber (format "CaseNumber = '%s'" (sf--escape (cdr kv)))))))
    (sf--render-case (sf--fetch-case-detail where))))

(defun sf-case-refresh ()
  "Re-fetch and re-render the case in this buffer."
  (interactive)
  (unless sf--case-id (user-error "No case Id in this buffer"))
  (sf--open-case-by-id sf--case-id))

(defun sf-case-browse-url ()
  "Open the current case in the browser."
  (interactive)
  (unless sf--case-id (user-error "No case Id in this buffer"))
  (browse-url-xdg-open (sf--case-url sf--case-id)))

(defun sf-case-yank-url ()
  "Copy the current case URL to the kill ring."
  (interactive)
  (unless sf--case-id (user-error "No case Id in this buffer"))
  (let ((url (sf--case-url sf--case-id)))
    (kill-new url)
    (message "Yanked: %s" url)))

(defun sf-case-jump-to-activity ()
  "Jump to the Activity section (comments + emails) in this case buffer."
  (interactive)
  (let ((origin (point)))
    (goto-char (point-min))
    (if (re-search-forward "^Activity " nil t)
        (beginning-of-line)
      (goto-char origin)
      (user-error "No Activity section in this buffer"))))

;;; ------------------------------------------------------ keybindings

(map! :leader
      (:prefix ("o s" . "salesforce")
       :desc "Queues"           "q" #'sf-queues
       :desc "Unassigned cases" "u" #'sf-unassigned
       :desc "View case"        "c" #'sf-case
       :desc "My cases"         "m" #'sf-my-cases))

(provide 'sf)
;;; sf.el ends here
