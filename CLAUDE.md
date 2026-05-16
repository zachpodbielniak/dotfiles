# CLAUDE.md
This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Things to NEVER do 
- run `local_postgres drop` . never do this, instead prompt me to run `psql`. ALWAYS ask for permission to run psql commands.

## Build and Test Commands
- Test Qtile configuration: `just test`
- Install dotfiles: `just stow`
- Install dotfiles (alternative): `just stow_alt`
- Install without Podman quadlets: `just stow quadlets=false` (also works for `stow_alt` / `dry`)
- Uninstall dotfiles: `just unstow`
- Dry run installation: `just dry`
- Bootstrap dependencies: `just bootstrap`

## Code Style Guidelines
- All Scripts (Shell, Python, Perl, Haskell):
  - Must include AGPLv3 license header immediately after the shebang
  - License header format:
    ```
    # dotfiles - Personal configuration files and scripts
    # Copyright (C) <year>  Zach Podbielniak
    #
    # This program is free software: you can redistribute it and/or modify
    # it under the terms of the GNU Affero General Public License as published by
    # the Free Software Foundation, either version 3 of the License, or
    # (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU Affero General Public License for more details.
    #
    # You should have received a copy of the GNU Affero General Public License
    # along with this program.  If not, see <https://www.gnu.org/licenses/>.
    ```
  - For Haskell scripts, use `{- ... -}` comment style for the license header

- Shell Scripts:
  - All scripts use bash (`#!/bin/bash`) and often include `set -euo pipefail`
  - Parameter validation for scripts with user input
  - Meaningful variable names (e.g., `filename`, `notes_dir`)
  - Use double quotes for variables: `"${variable}"`
  - Proper error handling with helpful messages
  - Include usage help via `-h/--help` flags
  - Consistent indentation (4 spaces)
  - Exit codes for error handling
  - Graceful failure with informative messages
  - Support for debug logging in complex operations

- Common Patterns:
  - Conditional file checking (`[[ -f "${file}" ]]`)
  - Default values for parameters
  - Structured command-line argument parsing
  - Case statements for multiple options
  - Use of external tools like `yq` for YAML processing
  - Use of temporary files and proper cleanup
  - Support for both file and stdin input
  - Content type detection and appropriate handling

## Script Utilities
- `sbi` (Second Brain Ingest):
  - Helps ingest content into a PARA-organized knowledge base
  - Supports text, audio, video, and URLs (including YouTube)
  - Processes media files by transcribing and summarizing
  - Supports editing content before ingestion with `--edit`
  - Allows custom filenames with `--name-seed`
  - Automatically categorizes content into PARA structure
  - Directory listing with `--list` and `--list-files`
  - second-brain file located with our second-brain mcp at 02_areas/repos/dotfiles/sbi.md

- Media Processing:
  - `strip_audio`: Extracts audio from video files using FFmpeg
  - `transcribe_audio`: Transcribes audio to text using Whisper
  - `ai_summary_as_neorg`: Generates summaries in Neorg format
  - `ai_summary_as_markdown`: Generates summaries in Markdown format

## Environment
- Any file under ./bin/scripts is part of $PATH so no need for absolute pathing
- Distrobox is used for various tools (and this does not affect pathing for anything under ${HOME}):
  - "whisper" container for audio transcription
  - "dev" container for FFmpeg and other media tools
- External utilities:
  - `perpy`: Python CLI for AI-based text processing
  - `mton`: Converter from Markdown to Neorg format
  - `yt-dlp`: YouTube media downloader
  - `vipe`: Edit data in pipes using $EDITOR

## Second Brain Organization
- Knowledge is organized using the PARA method:
  - 00_inbox: Temporary storage for unsorted notes
  - 01_projects: Active projects with defined outcomes
  - 02_areas: Ongoing responsibilities with standards
  - 03_resources: Topics and themes of interest
  - 04_archives: Inactive items from other categories
- Default location: $HOME/Documents/notes
- can use our second_brain MCP to interact with this 

## Doom Emacs Config (`./.config/doom/`)

- Custom elisp files live flat in `./.config/doom/` and are loaded from `config.el` via `(load! "filename")` (no `.el` extension). See the loader block near `(load! "art-of-war")` for the pattern.
- All custom `.el` files use the AGPLv3 header from `art-of-war.el:1-16`. Use the `;;; filename.el --- short -*- lexical-binding: t; -*-` header line and end with `(provide 'filename)` + `;;; filename.el ends here`.
- Doom localleader is `SPC m`, **not** comma (comma is taken by evil-snipe). Use `:localleader "x"` bindings as `SPC m x`.
- Color palette for faces is catppuccin-mocha — see `art-of-war.el:87-112` for the swatch.

### Evil + tabulated-list-mode keybindings (the recurring footgun)

When you define a `tabulated-list-mode`-derived major mode and want `RET`, `g`, `c`, etc. to do something:

1. **`define-key` on the mode-map is not enough.** Under `doom/evil-collection`, these buffers land in evil **`normal` state**, where evil's own bindings (`RET` → `evil-ret`, `g` → `gg/gd` prefix, `G` → `evil-goto-line`, etc.) shadow the mode-map.
2. **`map!` with `:n` / `:nm` is unreliable.** It expands to lazy `evil-define-key` which uses `evil-delay`; the binding may not land before the buffer is entered, and reloads (`SPC h r r`) don't re-trigger the delay.
3. **The pattern that works** (see `sf.el:388` and `sf.el:486`):

   ```elisp
   (defvar my-mode-map
     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map tabulated-list-mode-map)
       (define-key map (kbd "RET")      #'my-action)
       (define-key map (kbd "<return>") #'my-action)  ; GUI emacs sends <return>
       (define-key map "g" #'my-refresh)
       map))

   (with-eval-after-load 'evil
     (evil-define-key* 'normal my-mode-map
       (kbd "RET")      #'my-action
       (kbd "<return>") #'my-action
       "g"              #'my-refresh))
   ```

   `evil-define-key*` (with the asterisk) is the **immediate, non-deferred** variant. Use it.

4. To pick up keybinding changes after editing, **kill the existing buffer** (`C-x k`) before `SPC h r r` — the old buffer's keymap inheritance is already snapshot.

### Reference patterns to copy

- License header + `defgroup`/`defcustom`/face block: `art-of-war.el:1-112`
- `tabulated-list-mode` derived mode with evil-aware keybindings: `sf.el:378-410`
- `url-retrieve-synchronously` + `json-parse-buffer` HTTP helper: `arr.el:147-228`, `jackett.el:144-148`
- `map!` `:leader` `:prefix` for user-facing commands: `art-of-war.el:601-610`
- `pluggable registry`-style data-driven dispatch: `container-registry-browse.el` (`--registries` alist + `-register` API)

## Script Documentation
- For any scripts in ./bin/scripts, to understand details:
  - Check the corresponding markkdown file at with out second-brain mcp 02_areas/repos/dotfiles/<script_name>.md
  - If the markdown file does not exist, create one with:
    - Script details and purpose
    - Logic flow
    - Requirements
    - Potential pitfalls and bugs
    - Maintenance guidelines
    - Ideas for future expansion
  - Update this markdown file whenever the script is modified
