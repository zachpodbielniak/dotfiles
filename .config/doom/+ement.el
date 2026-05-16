;;; +ement.el -*- lexical-binding: t; -*-
;;
;; +ement.el - Matrix client (Ement.el via pantalaimon for E2EE)
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
;; Native Matrix client.  Connects through pantalaimon
;; (http://localhost:8009) for E2EE, since Ement itself doesn't speak
;; the encryption protocol.
;;
;; Custom safety: `ement-room--format-message-body' occasionally
;; tries to render image events with nil metadata (pantalaimon
;; sometimes returns incomplete info dicts) and signals
;; `wrong-type-argument' — caught here and replaced with a
;; placeholder.

;;; Code:

;;; Ement.el: native Matrix client (E2EE via pantalaimon on localhost:8009)
;;; Deferred — ~0.9s configure cost; loads on first `SPC M M' / `M-x ement-…'.
(use-package! ement
  :defer t
  :commands (ement-connect ement-room-list ement-view-room ement-disconnect)
  :config
  ;; setopt (not setq) so the defcustom :set hook adds kill-emacs-hook
  (setopt ement-save-sessions t)
  (setq plz-curl-program "/usr/bin/curl")
  ;; Guard against nil image metadata in encrypted rooms (pantalaimon
  ;; sometimes returns incomplete info dicts for media events)
  (defadvice! zach/ement--image-safe (fn event &rest args)
    :around #'ement-room--format-message-body
    (condition-case err
        (apply fn event args)
      (wrong-type-argument
       (propertize "[image unavailable]" 'face 'font-lock-comment-face)))))

;; Forward declarations to silence byte-compile warnings — ement isn't
;; loaded at compile time and these references are inside a function
;; that does `(require 'ement)' before touching them.
(defvar ement-sessions)
(declare-function ement-connect "ement")
(declare-function ement--read-sessions "ement")

;; Connect through pantalaimon for E2EE support.  Lives at top level so
;; the `SPC M M' keybinding can autoload it; explicitly requires ement
;; before calling its functions.
(defun zach/ement-connect ()
  "Connect to Matrix via pantalaimon proxy.
Restores saved session if available, otherwise prompts for login."
  (interactive)
  (require 'ement)
  (if ement-sessions
      ;; Already connected — just reconnect the first session
      (ement-connect :session (cdar ement-sessions))
    ;; Try to restore saved session from disk
    (condition-case nil
        (let ((saved (ement--read-sessions)))
          (if saved
              (ement-connect :session (cdar saved))
            (error "No saved session")))
      (error
       ;; No saved session — fresh login through pantalaimon
       (let ((user-id (read-string "User ID: " nil 'ement-connect-user-id-history))
             (password (read-passwd "Password: ")))
         (ement-connect :uri-prefix "http://localhost:8009"
                        :user-id user-id
                        :password password))))))

(map! :leader
      :desc "Matrix connect"    "M M" #'zach/ement-connect
      :desc "Matrix rooms"      "M r" #'ement-room-list
      :desc "Matrix view"       "M t" #'ement-view-room
      :desc "Matrix disconnect" "M d" #'ement-disconnect)

(provide '+ement)
;;; +ement.el ends here
