;;; +tramp-perf.el -*- lexical-binding: t; -*-
;;
;; +tramp-perf.el - Make TRAMP / remote file access usable
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
;; Without these guards, opening /ssh:host:/path hangs Emacs because
;; dirvish, vc, projectile, LSP, and syntax checkers all fire synchronous
;; subprocess calls over the SSH connection.
;;
;; What this file does:
;;   - Tunes TRAMP defaults (ssh method, low verbosity, cache hints).
;;   - Disables LSP/Eglot, projectile, flycheck/flymake, and dirvish
;;     preview attributes for buffers whose `default-directory' is remote.
;;   - Hardens recentf and auto-save against TRAMP paths.
;;   - Strips `ispell-completion-at-point' from text-mode-style buffers,
;;     since `ispell-completion-at-point' shells out to `look' over SSH
;;     on every keystroke.
;;
;; Dependencies: tramp, projectile, eglot, flycheck, flymake, dirvish, treemacs.
;; All guards use `after!' so they no-op when the underlying package isn't loaded.

;;; Code:

;; Use ssh directly (not scp) and keep connections alive
(after! tramp
  (setq tramp-default-method "ssh"
        tramp-verbose 1                         ;; minimal logging (raise to 6 to debug)
        tramp-auto-save-directory (expand-file-name "tramp-autosave" doom-cache-dir)
        remote-file-name-inhibit-cache nil       ;; cache remote stat results
        tramp-completion-reread-directory-timeout nil  ;; don't re-stat for completion
        vc-ignore-dir-regexp (format "%s\\|%s"
                                     vc-ignore-dir-regexp
                                     tramp-file-name-regexp))  ;; disable VC over TRAMP

  ;; Tell eshell to use TRAMP for remote paths (cd /ssh:host:/)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Container methods that aren't auto-registered (unlike docker/podman).
  ;; After enabling, `/distrobox:NAME:/path' and `/flatpak:APP-ID:/path'
  ;; resolve like any other TRAMP host.
  (tramp-enable-distrobox-method)
  (tramp-enable-flatpak-method))

;; text-mode (and markdown-mode, which inherits) adds `ispell-completion-at-point'
;; to `completion-at-point-functions'. It shells out to `look' via `process-file'
;; on every completion attempt — over TRAMP that's a full SSH round-trip per
;; keystroke and freezes Emacs. Kill it globally.
(setq text-mode-ispell-word-completion nil)

;; Keep auto-save files local instead of writing them through SSH each cycle
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" doom-cache-dir) t)))

(after! recentf
  (setq recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude tramp-file-name-regexp))

;; Belt and suspenders: if anything re-adds `ispell-completion-at-point' in a
;; remote buffer, strip it on find-file.
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local completion-at-point-functions
                          (remq 'ispell-completion-at-point
                                completion-at-point-functions)))))

;; Disable projectile on remote files — walking the tree over SSH is brutal
(after! projectile
  (defadvice! zach--projectile-skip-remote-a (fn &rest args)
    :around #'projectile-project-root
    (unless (file-remote-p default-directory)
      (apply fn args))))

;; Disable LSP/eglot on remote buffers
(after! eglot
  (defadvice! zach--eglot-skip-remote-a (fn &rest args)
    :around #'eglot-ensure
    (unless (file-remote-p default-directory)
      (apply fn args))))

;; Disable syntax checking on remote files
(after! flycheck
  (defadvice! zach--flycheck-skip-remote-a (fn &rest args)
    :around #'flycheck-mode
    (unless (file-remote-p default-directory)
      (apply fn args))))
(after! flymake
  (add-hook 'flymake-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (flymake-mode -1)))))

;; Dirvish: disable expensive attributes on remote directories
(after! dirvish
  (defadvice! zach--dirvish-simple-remote-a (fn &rest args)
    :around #'dirvish
    (if (file-remote-p default-directory)
        (let ((dirvish-attributes '(hl-line))
              (dirvish-preview-dispatchers nil))
          (apply fn args))
      (apply fn args)))
  ;; Also guard the dired override so plain dired on remote paths stays plain
  (defadvice! zach--dired-skip-dirvish-remote-a (fn &rest args)
    :around #'dired
    (if (and (car args) (file-remote-p (car args)))
        (let ((dirvish-override-dired-mode nil))
          (apply fn args))
        (apply fn args))))

;; Treemacs: don't set up file watchers on remote paths
(after! treemacs
  (defadvice! zach--treemacs-skip-remote-watch-a (fn &rest args)
    :around #'treemacs--start-watching
    (unless (file-remote-p default-directory)
      (apply fn args))))

(provide '+tramp-perf)
;;; +tramp-perf.el ends here
