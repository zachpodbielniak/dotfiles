;;; +jira.el -*- lexical-binding: t; -*-
;;
;; +jira.el - Jira issue tracker (jira.el)
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
;; Jira via `jira.el'.  Requires Emacs 30+ (uses newer rx syntax that
;; doesn't compile on 29.x), so the whole `(use-package! jira …)' form
;; is `:when'-guarded.
;;
;; Auth: store credentials in `~/.authinfo.gpg':
;;   machine <instance>.atlassian.net login <email> port https password <api-token>
;;
;; Two evil-keymapped views:
;;   - `jira-issues-mode' — issue list
;;   - `jira-detail-mode' — single-issue view (with comment editing)
;;
;; Both bind `?' to a buffer-local cheat-sheet.

;;; Code:

;;; Jira issue tracker (jira.el) — requires Emacs 30+ (rx compat)
;;; Auth: store credentials in ~/.authinfo.gpg:
;;;   machine <instance>.atlassian.net login <email> port https password <api-token>
;;; Deferred — ~1.2s configure cost; loads on first `SPC J' / `M-x jira-issues'.
(use-package! jira
  :when (>= emacs-major-version 30)
  :defer t
  :commands (jira-issues)
  :config
  (setq jira-base-url "https://dt-rnd.atlassian.net"
        jira-api-version 3
        auth-sources '("~/.authinfo"))
  ;; Evil overrides in jira-issues-mode
  (evil-define-key* 'normal jira-issues-mode-map
    (kbd "RET") (lambda () (interactive)
                  (jira-detail-show-issue (jira-utils-marked-item)))
    "go"        (lambda () (interactive)
                  (jira-actions-open-issue (jira-utils-marked-item)))
    "gf"        (lambda () (interactive)
                  (jira-detail-find-issue-by-key))
    "gc"        (lambda () (interactive)
                  (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item)))
    "gC"        #'jira-actions-change-issue-menu
    "gw"        #'jira-actions-add-worklog-menu
    "ge"        #'jira-export-menu
    "gl"        #'jira-issues-menu
    "gH"        #'jira-issues--switch-host-and-refresh
    "?"         (lambda () (interactive)
                  (with-current-buffer (get-buffer-create "*jira-keys*")
                    (erase-buffer)
                    (insert "Jira Issues Keybindings\n"
                            "=======================\n\n"
                            "RET  Open issue details\n"
                            "gl   Filter/search menu\n"
                            "gf   Find issue by key\n"
                            "go   Open in browser\n"
                            "gc   Copy issue ID\n"
                            "gC   Change issue status\n"
                            "gw   Add worklog\n"
                            "ge   Export menu\n"
                            "gH   Switch Jira host\n"
                            "?    This help\n")
                    (goto-char (point-min))
                    (special-mode))
                  (pop-to-buffer "*jira-keys*")))
  ;; Evil overrides in jira-detail-mode
  (evil-define-key* 'normal jira-detail-mode-map
    "+"         (lambda () (interactive) (jira-detail--add-comment))
    "-"         (lambda () (interactive) (jira-detail--remove-comment-at-point))
    "ge"        (lambda () (interactive) (jira-detail--edit-comment-at-point))
    "gC"        #'jira-actions-change-issue-menu
    "go"        (lambda () (interactive)
                  (jira-actions-open-issue jira-detail--current-key))
    "gU"        (lambda () (interactive) (jira-detail--update-field))
    "gf"        (lambda () (interactive) (jira-detail-find-issue-by-key))
    "gw"        (lambda () (interactive) (jira-detail--watchers-menu))
    "gP"        (lambda () (interactive) (jira-detail--show-parent-issue))
    "gS"        (lambda () (interactive) (jira-detail--create-subtask))
    "gc"        (lambda () (interactive)
                  (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key))
    "gr"        (lambda () (interactive)
                  (jira-detail-show-issue jira-detail--current-key))
    "?"         (lambda () (interactive)
                  (with-current-buffer (get-buffer-create "*jira-keys*")
                    (erase-buffer)
                    (insert "Jira Detail Keybindings\n"
                            "=======================\n\n"
                            "+    Add comment\n"
                            "-    Remove comment\n"
                            "ge   Edit comment\n"
                            "gC   Change status\n"
                            "gU   Update field\n"
                            "gf   Find issue by key\n"
                            "go   Open in browser\n"
                            "gc   Copy issue ID\n"
                            "gP   Show parent issue\n"
                            "gS   Create subtask\n"
                            "gr   Refresh\n"
                            "?    This help\n")
                    (goto-char (point-min))
                    (special-mode))
                  (pop-to-buffer "*jira-keys*"))))

;; Leader binding at top level so it's active without loading jira; first
;; press triggers the use-package!  `:config' block above.  Lives under
;; `SPC o' ("open") next to monday and the other launch-style commands.
(when (>= emacs-major-version 30)
  (map! :leader :desc "Jira issues" "o J" #'jira-issues))

(provide '+jira)
;;; +jira.el ends here
