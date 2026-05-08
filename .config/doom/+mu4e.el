;;; +mu4e.el -*- lexical-binding: t; -*-
;;
;; +mu4e.el - Email via mu4e + Proton Mail Bridge + msmtp
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
;; Email through Proton Mail Bridge (IMAP/SMTP on localhost):
;;   - Maildir at ~/.local/share/mail/proton synced by mbsync.
;;   - Sending via msmtp (config at ~/.config/msmtp/config).
;;   - mu4e + gnus for richer HTML rendering.
;;
;; Custom behavior:
;;   - `trash' mark moves into /Trash WITHOUT setting the \Deleted (T)
;;     flag.  Otherwise mbsync's `Expunge Both' sees a new local file
;;     with T set + no remote UID and silently expunges it locally,
;;     so the delete never reaches Proton.
;;   - After `mu4e-mark-execute-all' (the `x' command), automatically
;;     run `mu4e-update-mail-and-index' so local changes propagate
;;     up to Proton via mbsync.
;;
;; macOS-only `load-path' hack for finding the homebrew-installed
;; mu4e site-lisp directory lives in `config.el' (must run before
;; the `:email mu4e' Doom module loads, which is earlier than this
;; file).

;;; Code:

(after! mu4e
  (setq mu4e-maildir (expand-file-name "~/.local/share/mail/proton")
        mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc proton"
        mu4e-update-interval (* 5 60)
        mu4e-change-filenames-when-moving t  ;; required for mbsync
        mu4e-use-fancy-chars t
        mu4e-view-show-images t
        mu4e-view-use-gnus t            ;; richer HTML rendering via gnus/shr
        shr-inhibit-images nil          ;; let shr fetch images
        gnus-blocked-images nil         ;; don't block remote images
        mu4e-view-show-addresses t
        mu4e-compose-format-flowed t
        mu4e-confirm-quit nil
        mu4e-attachment-dir "~/Downloads"
        ;; Identity
        user-mail-address "zach@podbielniak.com"
        user-full-name "Zach Podbielniak"
        ;; Folder mapping — Proton Bridge exposes standard IMAP folders
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder   "/Sent"
        mu4e-refile-folder "/Archive"
        mu4e-trash-folder  "/Trash"
        ;; Proton keeps a copy in Sent automatically — don't double-save
        mu4e-sent-messages-behavior 'delete
        ;; Sending via msmtp
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)

  ;; Quick bookmarks for mu4e main view
  (setq mu4e-bookmarks
        '((:name "Unread"    :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today"     :query "date:today..now"                  :key ?t)
          (:name "This week" :query "date:7d..now"                     :key ?w)
          (:name "Flagged"   :query "flag:flagged"                     :key ?f)))

  ;; Redefine the `trash` mark: move into the Trash maildir WITHOUT setting
  ;; the \Deleted (T) flag. mbsync's `Expunge Both` sees a new local file with
  ;; T set and no remote UID as "marked for deletion, never uploaded" and
  ;; silently expunges it locally — so the delete never reaches Proton. Moving
  ;; into /Trash is already the delete action on Proton; the T flag is
  ;; redundant and destructive here.
  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
          :prompt "dtrash"
          :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
          :action (lambda (docid msg target)
                    (mu4e--server-move docid
                                       (mu4e--mark-check-target target)
                                       "-N"))))

  ;; After executing marks (x), push the local changes back to the IMAP server
  ;; by running mbsync again. mbsync is bidirectional, so deletions/moves/flag
  ;; changes made locally propagate up to Proton.
  (advice-add 'mu4e-mark-execute-all :after
              (lambda (&rest _)
                (mu4e-update-mail-and-index t))))

(provide '+mu4e)
;;; +mu4e.el ends here
