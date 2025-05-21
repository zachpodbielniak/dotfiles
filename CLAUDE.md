# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test Commands

- Test Qtile configuration: `just test`
- Install dotfiles: `just stow`
- Install dotfiles (alternative): `just stow_alt`
- Uninstall dotfiles: `just unstow`
- Dry run installation: `just dry`
- Bootstrap dependencies: `just bootstrap`

## Code Style Guidelines

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
  - second-brain file located with our second-brain mcp at 02_areas/repos/dotfiles/sbi.norg

- Media Processing:
  - `strip_audio`: Extracts audio from video files using FFmpeg
  - `transcribe_audio`: Transcribes audio to text using Whisper
  - `ai_summary_as_neorg`: Generates summaries in Neorg format
  - `ai_summary_as_markdown`: Generates summaries in Markdown format

## Neorg
- Neorg format is used for a lot of outputs.
  - You can access the Neorg spec file under second-brain mcp at '03_resources/technical/software/neorg-spec.norg' and a cheatsheat at '03_resources/technical/software/neorg-cheatsheet.norg'
- Common Neorg patterns:
  - Document headers with `* Title`
  - Code blocks with `@code language` and `@end`
  - Bullets with `-` characters
  - Tagged sections for metadata

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
- Default location: ~/Documents/notes

## Script Documentation

- For any scripts in ./bin/scripts, to understand details:
  - Check the corresponding Neorg file at with out second-brain mcp 02_areas/repos/dotfiles/<script_name>.norg
  - If the Neorg file does not exist, create one with:
    - Script details and purpose
    - Logic flow
    - Requirements
    - Potential pitfalls and bugs
    - Maintenance guidelines
    - Ideas for future expansion
  - Update this Neorg file whenever the script is modified
