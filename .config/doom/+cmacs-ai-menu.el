;;; +cmacs-ai-menu.el --- C-a keybindings for the cmacs AI menu -*- lexical-binding: t; -*-

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

;; Keyboard access to everything on the cmacs AI right-click menu, under a
;; `C-a' prefix mirroring the menu's own shape:
;;
;;   C-a a   Ask AI      summarize / rephrase / reply / explain / ask / ...
;;   C-a c   Chat        carry this into a conversation
;;   C-a b   Brigade     hand this to an agent
;;   C-a t   Tools       your own `cmacs-brigade-deftool' forms
;;   C-a g   Git         draft a commit message from the diff
;;
;; In magit and commit buffers `c' is rebound to the commit drafter --
;; `c' means commit everywhere else in magit -- and the Chat group moves
;; to `C' there, so nothing is lost.
;;
;; Every command acts on the same target the right-click would: the region
;; if one is highlighted, otherwise whatever the resolvers make of point --
;; the file under the cursor in dired, the hunk in a diff, the last command
;; in a vterm, the heading in org.  So the bindings work everywhere the
;; menu does, and mean the buffer-appropriate thing in each.
;;
;; The first key of every group re-opens that group as a completion list
;; (`C-a a a', `C-a c c', `C-a b b').  That is the binding that stays
;; correct as things are registered: a fixed key per action cannot reach an
;; action added later, but the group picker can, including any deftool you
;; publish with :menu.
;;
;; On taking `C-a': Doom binds it only in the minibuffer and evil-ex maps
;; (see modules/config/default/+evil-bindings.el), which are unaffected --
;; those keymaps win in the places they apply.  What this does displace is
;; the global-map `move-beginning-of-line', which insert state falls back
;; to.  `C-a C-a' is bound to it so nothing is actually lost; in normal
;; state you have `0' and `^' as usual.  To back the whole thing out,
;; comment the `load!' in config.el.

;;; Code:

;; None of these are required at load time: cmacs-ai's Elisp loads lazily
;; and the autoloads are already in place, so binding the symbols is
;; enough.  A build without --with-cmacs-ai simply has commands that
;; report the feature is missing, which is the honest failure.

;;; ---------------------------------------------------------------- prefix

(defvar +cmacs-ai-map (make-sparse-keymap)
  "Prefix keymap for the cmacs AI menu, bound to `C-a'.")

(defvar +cmacs-ai-ask-map (make-sparse-keymap)
  "Ask AI actions: answer something about the thing at point.")

(defvar +cmacs-ai-chat-map (make-sparse-keymap)
  "Chat actions: carry the thing at point into a conversation.")

(defvar +cmacs-ai-brigade-map (make-sparse-keymap)
  "Brigade actions: hand the thing at point to an agent.")

;;; ------------------------------------------------------------------- ask
;;
;; Everything that answers a question about the target and streams into a
;; result window (`q' / `C-c C-k' to close, `w' to copy, `g' to re-run).
;; The exceptions are marked: they change your buffer instead.

(map! :map +cmacs-ai-ask-map
      :desc "Pick an Ask action"      "a" #'cmacs-ai-menu-pick-ask
      :desc "Ask about this..."       "?" #'cmacs-ai-ask
      :desc "Summarize..."            "s" #'cmacs-ai-summarize
      :desc "Rephrase..."             "r" #'cmacs-ai-rephrase
      :desc "Reply..."                "p" #'cmacs-ai-reply
      :desc "Explain..."              "e" #'cmacs-ai-explain
      ;; These three write into the buffer rather than a result window.
      :desc "Rewrite in place..."     "w" #'cmacs-ai-rewrite-region
      :desc "Document (insert)"       "d" #'cmacs-ai-doc-region
      :desc "Generate a test"         "t" #'cmacs-ai-test-region
      ;; Surface-specific: gsurf fetches the page body asynchronously, and
      ;; imgedit reads the image rather than any text.
      :desc "Summarize gsurf page"    "S" #'cmacs-gsurf-summarize
      :desc "Ask about gsurf page..." "G" #'cmacs-gsurf-ask
      :desc "Describe image"          "i" #'cmacs-imgedit-ai-describe
      :desc "Edit image by prompt..." "I" #'cmacs-imgedit-ai-prompt)

;;; ------------------------------------------------------------------ chat

(map! :map +cmacs-ai-chat-map
      :desc "Pick a Chat action"      "c" #'cmacs-ai-menu-pick-chat
      :desc "New chat with this"      "n" #'cmacs-ai-chat-with-this
      :desc "Send to an open chat..." "o" #'cmacs-ai-send-to-open-chat
      :desc "Send to libreclaw..."    "l" #'cmacs-ai-send-to-libreclaw
      ;; A plain chat, carrying nothing.  `cmacs-ai-chat' is the
      ;; autoloaded entry point; `cmacs-ai-chat-open' is internal and not
      ;; reachable before cmacs-ai-chat.el has loaded.
      :desc "Open an empty chat"      "C" #'cmacs-ai-chat)

;;; --------------------------------------------------------------- brigade
;;
;; Nothing here starts a run.  "Spawn" fills in the compose transient and
;; shows it to you; "Make a task" appends to a plan file without queueing
;; it.  Starting work is still a deliberate second step.

(map! :map +cmacs-ai-brigade-map
      :desc "Pick a Brigade action"   "b" #'cmacs-ai-menu-pick-brigade
      :desc "Spawn an agent on this..." "s" #'cmacs-ai-spawn-agent
      :desc "Send to a running task..." "m" #'cmacs-ai-send-to-task
      :desc "Pin as agent context"    "p" #'cmacs-ai-pin-context
      :desc "Unpin all context"       "P" #'cmacs-ai-unpin-all
      :desc "Make this a task"        "t" #'cmacs-ai-make-brigade-task
      ;; The brigade's own entry points, for when you are not acting on
      ;; anything in particular.
      :desc "Dashboard"               "d" #'cmacs-brigade-dashboard
      :desc "New task (describe it)"  "n" #'cmacs-brigade-compose-quick
      :desc "New task (dictate it)"   "v" #'cmacs-brigade-compose-voice)

;;; ------------------------------------------------------------- top level

(map! :map +cmacs-ai-map
      :desc "Ask AI"                  "a" +cmacs-ai-ask-map
      :desc "Chat"                    "c" +cmacs-ai-chat-map
      :desc "Brigade"                 "b" +cmacs-ai-brigade-map
      :desc "Tools (your deftools)"   "t" #'cmacs-ai-menu-pick-tools
      ;; Git, from anywhere in a worktree -- not only from magit.
      :desc "Draft a commit message"  "g" #'cmacs-ai-suggest-commit-message
      ;; The whole menu, the two ways the menu itself offers it.
      :desc "Open the AI menu"        "m" #'cmacs-ai-menu
      :desc "Pick any AI action"      "." #'cmacs-ai-menu-pick
      :desc "Run an action by name"   "x" #'cmacs-ai-run-action
      ;; What `C-a' used to do, kept one keystroke away.
      :desc "Beginning of line"       "C-a" #'move-beginning-of-line)

;;; ------------------------------------------------------- magit override
;;
;; In a magit or commit buffer, `c' should mean commit -- that is what it
;; means everywhere else in magit, and drafting the message is the thing
;; you actually want the AI for there.  So this map inherits the whole
;; C-a tree and rebinds just that one key, moving the Chat group to `C'
;; rather than displacing it.  Everything else is unchanged.

(defvar +cmacs-ai-magit-map (make-sparse-keymap)
  "C-a in magit and commit buffers: the usual tree, with `c' for commit.")

(set-keymap-parent +cmacs-ai-magit-map +cmacs-ai-map)

(map! :map +cmacs-ai-magit-map
      :desc "Draft a commit message"  "c" #'cmacs-ai-suggest-commit-message
      :desc "Chat"                    "C" +cmacs-ai-chat-map)

(map! :map (magit-mode-map magit-status-mode-map magit-diff-mode-map
            git-commit-mode-map)
      :nvie "C-a" +cmacs-ai-magit-map)

;;; The prefix itself, in the states where it is useful.  Normal, visual
;;; (acting on a selection is the common case), insert and emacs.
;;;
;;; Deliberately NOT also under the leader: `SPC a' is already the gptel /
;;; claude-code prefix in +ai.el, and rebinding it there would silently
;;; take out `SPC a c', `SPC a r' and the rest of that group.
(map! :nvie "C-a" +cmacs-ai-map)

(provide '+cmacs-ai-menu)

;;; +cmacs-ai-menu.el ends here
