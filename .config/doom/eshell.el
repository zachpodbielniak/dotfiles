;;; eshell.el --- Eshell performance tuning & TRAMP integration -*- lexical-binding: t; -*-

;;;; -------------------------------------------------------------------------
;;;; Remote-safe prompt
;;;; -------------------------------------------------------------------------
;;; Doom's +eshell-default-prompt-fn calls git + shrink-path on every
;;; prompt render.  Over TRAMP that's multiple blocking SSH round-trips
;;; per keystroke, eventually saturating the connection and hanging Emacs.

(after! eshell
  (defun zach--eshell-remote-prompt ()
    "Minimal prompt for remote directories — no git, no shrink-path."
    (concat
     (abbreviate-file-name (eshell/pwd))
     (if (= (file-user-uid) 0) " # " " λ ")))

  ;; Wrap the prompt function to use the lightweight version on remote dirs
  (defadvice! zach--eshell-prompt-remote-a (fn)
    :around #'+eshell-default-prompt-fn
    (if (file-remote-p default-directory)
        (zach--eshell-remote-prompt)
      (funcall fn)))

;;;; -------------------------------------------------------------------------
;;;; Remote feature toggling
;;;; -------------------------------------------------------------------------
;;; Disable expensive features when cd'ing into remote dirs, re-enable on local.

  ;; Disable eshell-syntax-highlighting on remote — it stats files for colorization
  (add-hook 'eshell-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (when (bound-and-true-p eshell-syntax-highlighting-mode)
                  (eshell-syntax-highlighting-mode -1)))))

  ;; Also disable it when cd'ing into a remote dir mid-session
  (add-hook 'eshell-directory-change-hook
            (lambda ()
              (if (file-remote-p default-directory)
                  (progn
                    (when (bound-and-true-p eshell-syntax-highlighting-mode)
                      (eshell-syntax-highlighting-mode -1))
                    ;; Disable pcomplete remote file stat'ing
                    (setq-local pcomplete-try-first-hook nil))
                ;; Re-enable when back to local
                (unless (bound-and-true-p eshell-syntax-highlighting-mode)
                  (eshell-syntax-highlighting-mode 1)))))

;;;; -------------------------------------------------------------------------
;;;; Completion performance
;;;; -------------------------------------------------------------------------
;;; pcmpl-args parses man pages on every TAB — very slow even locally.
;;; Disable it; built-in pcomplete (file/command completion) is fast enough.

  (setq pcmpl-args-debug nil)
  (defadvice! zach--pcmpl-args-disable-a (fn &rest args)
    "Skip pcmpl-args man-page parsing; fall through to basic pcomplete."
    :around #'pcmpl-args-pcomplete
    nil)

  ;; Limit the number of completions pcomplete generates (default is unlimited)
  (setq pcomplete-cycle-completions nil     ;; show list instead of cycling
        pcomplete-cycle-cutoff-length 5))   ;; cycle only when ≤5 candidates

;; Don't let corfu auto-popup in eshell — only complete on explicit TAB.
;; The auto-popup fires pcomplete continuously as you type, which is the
;; main source of lag.
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil
                        corfu-auto-delay 1.0)))
