;;; +emms.el -*- lexical-binding: t; -*-
;;
;; +emms.el - EMMS / MPD music player
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
;; MPD-only EMMS setup — no filesystem indexing.  `(emms-all)' would
;; call `(emms-cache 1)' (on-disk cache + find-file hooks that read
;; tags off the filesystem) and register `emms-info-native' /
;; `emms-info-libtag' as info sources.  We skip both.  The in-memory
;; emms-cache hash still exists — required for the browse-by-*
;; commands — and can be populated from MPD on demand with
;; `M-x emms-player-mpd-update-all-reset-cache' (no filesystem work:
;; MPD sends its library metadata over the protocol).
;;
;; The most surprising piece is the `:override' on
;; `emms-player-mpd-start' (`zach/emms-mpd-start-by-position') —
;; upstream's `start-and-sync-1' path can decide to clear MPD's
;; queue and re-add every EMMS playlist track on every RET press,
;; spawning thousands of MPD commands on a large queue.  We treat
;; MPD's queue as authoritative and just `play N'.
;;
;; The 1 Hz MPD poll is also disabled (`emms-player-mpd-check-interval
;; nil') because the timer rewrites the playlist buffer mid-play.
;; Keybinds hang off `SPC z'.

;;; Code:

;;; EMMS: MPD client for music playback
;;;
;;; MPD-only setup — no filesystem indexing.  `(emms-all)' would call
;;; `(emms-cache 1)' (on-disk cache + find-file hooks that read tags
;;; off the filesystem) and register `emms-info-native' /
;;; `emms-info-libtag' as info sources.  We skip both.  The in-memory
;;; emms-cache hash still exists — required for the browse-by-*
;;; commands — and can be populated from MPD on demand with
;;; `M-x emms-player-mpd-update-all-reset-cache' (no filesystem work:
;;; MPD sends its library metadata over the protocol).
(use-package! emms
  :config
  (emms-minimalistic)
  (require 'emms-playlist-mode)
  (require 'emms-info)
  (require 'emms-cache)
  (require 'emms-browser)
  (require 'emms-player-mpd)
  ;; Track-info + cache wiring (subset of what `emms-all' does).
  ;;
  ;; We DELIBERATELY DO NOT add `emms-info-initialize-track' to
  ;; `emms-track-initialize-functions'.  Upstream adds it, but for an
  ;; MPD-only setup it's a performance disaster:
  ;;   1. It calls `(file-attributes (emms-track-name track))' on
  ;;      every `emms-track' call — a filesystem stat per track,
  ;;      even though we want zero filesystem work.
  ;;   2. The stat usually returns nil (MPD's music_directory !=
  ;;      our path), so `info-mtime' never gets set, so on the next
  ;;      call it re-runs `emms-info-functions' (= `emms-info-mpd')
  ;;      without any pre-fetched info.  Every call to `emms-track'
  ;;      => a fresh MPD query.  For a large queue, that's thousands
  ;;      of redundant MPD round-trips on every playlist operation.
  ;;
  ;; Metadata is set explicitly by `emms-player-mpd-sync-from-mpd'
  ;; via `(emms-info-mpd track song-info)' WITH pre-fetched data, so
  ;; the init-hook re-query is pure waste.
  ;;
  ;; `(emms-cache 1)' enables the cache hash machinery (required by
  ;; `emms-cache-set-from-mpd-all').  It does NOT scan the
  ;; filesystem — only restores a previously saved snapshot and
  ;; persists on exit.
  (setq emms-playlist-default-major-mode #'emms-playlist-mode)
  (setq emms-track-description-function #'emms-info-track-description)
  (emms-cache 1)

  ;; Override `emms-player-mpd-start' so hitting RET on a track
  ;; simply tells MPD `play N' (N = that track's index in the
  ;; queue, counted from the EMMS playlist).  Upstream's
  ;; `start-and-sync-1' path decides, based on a
  ;; `buffer-modified-p' check that triggers on innocuous
  ;; interactions, to clear MPD's queue and re-add every EMMS
  ;; playlist track one by one via `sync-from-emms'.  For a queue
  ;; of any real size that's thousands of MPD commands plus
  ;; thousands of init-hook re-runs — the "spazz" the user sees.
  ;; We treat MPD's queue as authoritative (kept mirrored in the
  ;; EMMS playlist via `emms-player-mpd-connect'), so no sync is
  ;; needed on play.
  (defun zach/emms-mpd-start-by-position (_track)
    "Replacement for `emms-player-mpd-start'.
Play the currently-selected track by its position in the playlist,
without touching MPD's queue."
    (with-current-emms-playlist
      (save-excursion
        (goto-char (if (and emms-playlist-selected-marker
                            (marker-position emms-playlist-selected-marker))
                       emms-playlist-selected-marker
                     (point-min)))
        (let ((count 0))
          (condition-case nil
              (while t
                (emms-playlist-previous)
                (setq count (1+ count)))
            (error nil))
          (emms-player-mpd-play count)))))
  (advice-add 'emms-player-mpd-start
              :override #'zach/emms-mpd-start-by-position)
  ;; Disable modeline track display — causes UI lag with MPD polling
  (emms-mode-line-mode -1)
  (emms-playing-time-display-mode -1)
  ;; EMMS uses non-derived modes so Evil doesn't auto-detect them.
  ;; Bind directly on the mode keymaps with vim-style navigation.
  (evil-set-initial-state 'emms-browser-mode 'normal)
  (evil-set-initial-state 'emms-playlist-mode 'normal)

  (evil-define-key* 'normal emms-browser-mode-map
    "j"        #'next-line
    "k"        #'previous-line
    "h"        #'evil-backward-char
    "l"        #'evil-forward-char
    "q"        #'emms-browser-bury-buffer
    (kbd "RET") #'emms-browser-add-tracks-and-play
    (kbd "TAB") #'emms-browser-toggle-subitems
    (kbd "SPC") #'emms-browser-toggle-subitems
    "x"        #'emms-pause
    "X"        #'emms-stop
    "+"        #'emms-volume-raise
    "-"        #'emms-volume-lower
    "/"        #'emms-isearch-buffer
    "d"        #'emms-browser-view-in-dired
    "D"        #'emms-browser-delete-files
    "C"        #'emms-browser-clear-playlist
    "ga"       #'emms-browse-by-artist
    "gA"       #'emms-browse-by-album
    "gb"       #'emms-browse-by-genre
    "gy"       #'emms-browse-by-year)

  (evil-define-key* 'normal emms-playlist-mode-map
    "j"        #'next-line
    "k"        #'previous-line
    "h"        #'evil-backward-char
    "l"        #'evil-forward-char
    "q"        #'quit-window
    (kbd "RET") #'emms-playlist-mode-play-smart
    "d"        #'emms-playlist-mode-kill-track
    "x"        #'emms-pause
    "X"        #'emms-stop
    "+"        #'emms-volume-raise
    "-"        #'emms-volume-lower
    "r"        #'emms-random
    "s"        #'emms-shuffle)
  (setq emms-player-list '(emms-player-mpd)
        emms-info-functions '(emms-info-mpd)
        emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory "~/Music"
        ;; Disable the 1 Hz MPD poll: the timer calls
        ;; `emms-player-mpd-detect-song-change' which (via its sync /
        ;; select-song paths) does `goto-char' + playlist rewrites
        ;; inside *EMMS Playlist*, making the buffer unusable while
        ;; a track is playing.  Trade-off: EMMS's "currently playing"
        ;; highlight won't auto-advance when MPD moves to the next
        ;; track — hit `SPC z l' (or any emms command) and it
        ;; re-reads MPD state, or set up mpc/mpris elsewhere.
        emms-player-mpd-check-interval nil)
  ;; Keep the original after-started hook as a safety net for
  ;; reconnects (idempotent).
  (add-hook 'emms-player-started-hook #'emms-player-mpd-connect)

  ;; One-shot initial connect.  Syncs MPD's current queue into the
  ;; EMMS playlist so `SPC z p' has a track to start with.  Runs on
  ;; a 2 s idle timer so Emacs startup isn't held if MPD is slow or
  ;; unreachable; errors are caught so the rest of config survives.
  ;;
  ;; We deliberately do NOT call `emms-cache-set-from-mpd-all' here:
  ;; it iterates every track in MPD's library (for a big library,
  ;; thousands of messages floods the echo area and triggers heavy
  ;; redisplay).  The browser works without a primed cache — the
  ;; first time it's opened after a fresh Emacs, run
  ;; `M-x emms-cache-set-from-mpd-all' manually, or bind it to a
  ;; key (e.g. `SPC z B' below for "Browser prime").
  (run-with-idle-timer
   2 nil
   (lambda ()
     (condition-case err
         (emms-player-mpd-connect)
       (error (message "EMMS: MPD initial connect failed: %s"
                       (error-message-string err))))))

  (map! :leader
        (:prefix ("z" . "music")
         :desc "Play/pause"     "p" #'emms-pause
         :desc "Stop"           "s" #'emms-stop
         :desc "Next track"     "n" #'emms-next
         :desc "Previous track" "N" #'emms-previous
         :desc "Playlist"       "l" #'emms-playlist-mode-go
         :desc "Browser"        "b" #'emms-browser
         :desc "Browser: prime from MPD" "B" #'emms-cache-set-from-mpd-all
         :desc "Volume up"      "+" #'emms-volume-raise
         :desc "Volume down"    "-" #'emms-volume-lower
         :desc "Shuffle"        "S" #'emms-shuffle
         :desc "Now playing"    "i" #'emms-show)))

(provide '+emms)
;;; +emms.el ends here
