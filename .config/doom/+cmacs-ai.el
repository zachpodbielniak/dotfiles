;;; +cmacs-ai.el --- cmacs-ai provider/model tweaks -*- lexical-binding: t; -*-

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

;; Per-provider model overrides for cmacs-ai.  The C layer bakes in a single
;; AI_*_DEFAULT_MODEL macro per provider; this file advises
;; `cmacs-ai-make-session' to consult `+cmacs-ai-provider-models' first so
;; individual provider defaults can be changed without recompiling.

;;; Code:

(defvar +cmacs-ai-provider-models
  '((ollama . "gemma4:26b")
    (grok   . "grok-4.6"))
  "Alist of (PROVIDER . MODEL) overrides applied when no explicit model is given.
Entries here take precedence over the C-level AI_*_DEFAULT_MODEL macros but
yield to any explicit model passed to `cmacs-ai-make-session' or to a non-nil
`cmacs-ai-default-model'.")

(defun +cmacs-ai--inject-provider-model (orig &optional provider model system-prompt)
  "Around-advice for `cmacs-ai-make-session' that applies per-provider defaults."
  (let* ((p (or provider cmacs-ai-default-provider))
         (m (or model
                (alist-get p +cmacs-ai-provider-models)
                cmacs-ai-default-model)))
    (funcall orig provider m system-prompt)))

(with-eval-after-load 'cmacs-ai
  (advice-add 'cmacs-ai-make-session :around #'+cmacs-ai--inject-provider-model))

;;; --------------------------------------------------------------- default

;; cmacs-ai has *two* independent notions of "default provider" and only one
;; of them is the Elisp layer's.  ai-glib's AiConfig reads
;; ~/.config/ai-glib/config.yaml (default_provider: grok), but that is
;; consulted only by `ai_simple_new'; every cmacs-ai entry point goes through
;; `cmacs-ai-make-session', which reads the `cmacs-ai-default-provider'
;; defcustom -- shipped as `claude'.  Unset, that is what M-x cmacs-ai-send
;; and M-x cmacs-ai-chat use, and it fails with an Anthropic 401.
;;
;; The model deliberately does NOT go in `cmacs-ai-default-model': that is
;; provider-agnostic, so it would hand "grok-4.6" to Claude on the next
;; `cmacs-ai-chat-with-provider'.  It belongs in the per-provider alist above,
;; which is what the advice consults first.

(with-eval-after-load 'cmacs-ai
  (setq cmacs-ai-default-provider 'grok))

;;; ---------------------------------------------------------------- genmail

;; GenMail's triage model is a brigade defcustom, not a cmacs-ai one, so it
;; is not reached by the advice above -- genmail passes an explicit model.
;; Set after the feature loads so this wins over the shipped default rather
;; than being clobbered by the `defcustom'.

(defvar cmacs-brigade-genmail-triage-model)

(with-eval-after-load 'cmacs-brigade-genmail
  (setq cmacs-brigade-genmail-triage-model "ollama/gemma4:31b-cloud"))

(provide '+cmacs-ai)

;;; +cmacs-ai.el ends here
