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
  '((ollama . "gemma4:26b"))
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

(provide '+cmacs-ai)

;;; +cmacs-ai.el ends here
