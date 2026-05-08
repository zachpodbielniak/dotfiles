;;; +ai.el -*- lexical-binding: t; -*-
;;
;; +ai.el - AI / LLM integration: gptel + claude-code
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
;; Three gptel backends mirroring the nvim avante.nvim setup:
;;   - Ollama (default)   — local gemma3:12b
;;   - OpenAI             — gpt-4o
;;   - Anthropic          — claude-sonnet-4-20250514
;;
;; Plus claude-code, a vterm-backed wrapper around the `claude' CLI.
;; All keys hang off the `SPC a' AI prefix.

;;; Code:

;;; AI/LLM (gptel, replaces avante.nvim)
;;; Three backends matching nvim config: ollama (default), openai, anthropic
(after! gptel
  ;; Default to Ollama with gemma3:12b
  (setq gptel-model 'gemma3:12b
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "127.0.0.1:11434"
                        :models '(gemma3:12b)
                        :stream t))

  ;; OpenAI backend
  (gptel-make-openai "OpenAI"
    :key 'gptel-api-key
    :models '(gpt-4o)
    :stream t)

  ;; Anthropic backend
  (gptel-make-anthropic "Anthropic"
    :key 'gptel-api-key
    :models '(claude-sonnet-4-20250514)
    :stream t))

;;; AI prefix (SPC a)
(map! :leader :desc "AI" "a" nil)

;;; Claude Code CLI (runs `claude` as subprocess via vterm)
(use-package! claude-code
  :config
  (setq claude-code-terminal-backend 'vterm)
  (map! :leader
        :desc "Claude Code"        "a c" #'claude-code-start
        :desc "Claude send region" "a s" #'claude-code-send-region
        :desc "Claude send buffer" "a b" #'claude-code-send-buffer
        :desc "Claude fix error"   "a f" #'claude-code-send-flymake-to-claude))

;;; gptel rewrite & context (missing keybindings for existing features)
(map! :leader
      :desc "AI rewrite region" "a r" #'gptel-rewrite
      :desc "AI add context"    "a a" #'gptel-add
      :desc "AI menu"           "a m" #'gptel-menu
      :desc "AI chat"           "a g" #'gptel)

(provide '+ai)
;;; +ai.el ends here
