;;; +yeetube.el -*- lexical-binding: t; -*-
;;
;; +yeetube.el - YouTube front-end (yeetube + mpv + yt-dlp)
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
;; Search YouTube → play via mpv → optionally download via yt-dlp.
;; All keys hang off the `SPC Y' YouTube prefix; the in-buffer
;; `?' help command renders an evil-friendly cheat sheet.

;;; Code:

;;; YouTube front-end (yeetube): search + play via mpv, download via yt-dlp.
(defun zach/yeetube-show-keys ()
  "Display a help buffer listing `yeetube-mode' keybindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*yeetube-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "yeetube Keybindings\n"
              "===================\n\n"
              "Playback:\n"
              "  RET    Play video at point (mpv)\n"
              "  r      Replay last video\n"
              "  p      Toggle mpv pause\n"
              "  v      Toggle video display in mpv\n"
              "  V      Toggle --no-video flag\n"
              "  C-q    Change mpv video quality\n\n"
              "Navigation / search:\n"
              "  M-RET  New search\n"
              "  C-RET  Open video/playlist page\n"
              "  b      Browse URL in external browser\n"
              "  c      Channel videos (for channel at point)\n"
              "  L      Channel live streams\n\n"
              "Saved / clipboard:\n"
              "  s      Save video to persistent list\n"
              "  P      Play a saved video\n"
              "  C      Copy URL to kill ring\n\n"
              "Download:\n"
              "  d      Download video (yt-dlp)\n"
              "  D      Change download directory\n"
              "  a      Change audio format\n\n"
              "Misc:\n"
              "  T      Toggle torsocks routing\n"
              "  S      Sort by column at point (tabulated-list)\n"
              "  q      Quit window\n"
              "  ?      This help\n"))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*yeetube-keys*"))

(use-package! yeetube
  :defer t
  :init
  (setq yeetube-results-limit        20
        yeetube-enable-emojis        nil
        yeetube-display-thumbnails-p t)
  (map! :leader :desc "YouTube" "Y" nil)
  (map! :leader
        :desc "Search YouTube"  "Y s" #'yeetube-search
        :desc "Saved videos"    "Y l" #'yeetube-play-saved-video
        :desc "Download dir"    "Y d" #'yeetube-download-change-directory)
  :config
  (keymap-set yeetube-mode-map "?" #'zach/yeetube-show-keys)
  ;; Evil normal-state overrides — otherwise RET / letter keys just move point.
  (evil-define-key* 'normal yeetube-mode-map
    (kbd "RET")   #'yeetube-play
    (kbd "M-RET") #'yeetube-search
    (kbd "C-<return>") #'yeetube-video-or-playlist-page
    (kbd "C-q")   #'yeetube-mpv-change-video-quality
    "b"  #'yeetube-browse-url
    "c"  #'yeetube-channel-videos
    "C"  #'yeetube-copy-url
    "d"  #'yeetube-download-video
    "D"  #'yeetube-download-change-directory
    "a"  #'yeetube-download-change-audio-format
    "p"  #'yeetube-mpv-toggle-pause
    "v"  #'yeetube-mpv-toggle-video
    "V"  #'yeetube-mpv-toggle-no-video-flag
    "s"  #'yeetube-save-video
    "L"  #'yeetube-channel-streams
    "P"  #'yeetube-play-saved-video
    "r"  #'yeetube-replay
    "T"  #'yeetube-mpv-toggle-torsocks
    "q"  #'quit-window
    "?"  #'zach/yeetube-show-keys))

(provide '+yeetube)
;;; +yeetube.el ends here
