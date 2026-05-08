;;; +home-assistant.el -*- lexical-binding: t; -*-
;;
;; +home-assistant.el - Home Assistant control (purplg/hass)
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
;; Talks to Home Assistant via the REST API.  Reads server URL from
;; `$HASS_SERVER' (host, host:port, or http(s)://host[:port]) and a
;; long-lived access token from `$HASS_TOKEN'.  Both must be exported
;; in the Emacs process's env (~/.profile or the Doom launcher).
;;
;; `zach/hass-parse-server' normalizes whatever is in HASS_SERVER into
;; (host port insecure-p) so the same setup line works for `localhost',
;; `home.local:8123', or `https://home.example.com'.
;;
;; Two dashboards (`hass-dash-layouts'):
;;   - `default' — full layout grouped by area (basement, garage,
;;                 cameras, virtual switches, …).  Confirm-prompts
;;                 on freezer plugs and the server-room cutoff.
;;   - `quick'   — flat list of the most-used lamps and lights.
;;
;; Keys live under `SPC H'.

;;; Code:

;;; Home Assistant (purplg/hass)
;;; Uses the $HASS_SERVER (URL or host:port) and $HASS_TOKEN environment
;;; variables.  Make sure both are exported in the Emacs process's env
;;; (e.g. in ~/.profile or the Doom launcher).

(defun zach/hass-parse-server (server)
  "Parse SERVER into a list (HOST PORT INSECURE-P).
Accepts \"host\", \"host:port\", \"http://host[:port]\", or
\"https://host[:port]\".  When no port is specified, defaults to 443
for https and 8123 for http (HA's native port)."
  (let (scheme host port)
    (cond
     ((string-match "^\\(https?\\)://\\([^:/]+\\)\\(?::\\([0-9]+\\)\\)?/?" server)
      (setq scheme (match-string 1 server)
            host   (match-string 2 server)
            port   (match-string 3 server)))
     ((string-match "^\\([^:/]+\\):\\([0-9]+\\)$" server)
      (setq scheme "http"
            host   (match-string 1 server)
            port   (match-string 2 server)))
     (t (setq scheme "http" host server)))
    (list host
          (cond
           (port                    (string-to-number port))
           ((equal scheme "https")  443)
           (t                       8123))
          (equal scheme "http"))))

(use-package! hass
  :defer t
  :init
  (let* ((server (or (getenv "HASS_SERVER") "localhost:8123"))
         (parsed (zach/hass-parse-server server)))
    (setq hass-host     (nth 0 parsed)
          hass-port     (nth 1 parsed)
          hass-insecure (nth 2 parsed)
          hass-apikey   (lambda () (getenv "HASS_TOKEN"))))
  ;; Dashboards — generated from /api/template scrape of entities+areas.
  ;; Two layouts:
  ;;   default — everything worth controlling, grouped by room
  ;;   quick   — flat list of the most-used lamps/lights only
  (setq hass-dash-layouts
        '((default .
            ((hass-dash-group
              :title "Home"
              :format "%t\n\n%v"

              (hass-dash-group
               :title "Basement — Lamps & Fans"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.b4_b0_24_0e_09_83" :label "Lamp 01")
               (hass-dash-toggle :entity-id "switch.b4_b0_24_0e_0d_c5" :label "Lamp 02")
               (hass-dash-toggle :entity-id "switch.54_af_97_f4_25_28" :label "Lamp 03")
               (hass-dash-toggle :entity-id "switch.28_87_ba_6a_2a_ef" :label "Lamp 04")
               (hass-dash-toggle :entity-id "switch.b4_b0_24_29_80_df" :label "Fan 01")
               (hass-dash-toggle :entity-id "switch.b4_b0_24_29_c2_2c" :label "Fan 02"))

              (hass-dash-group
               :title "Basement — Lights"
               :title-face outline-2
               (hass-dash-toggle :entity-id "light.1c_61_b4_64_ea_42" :label "Can (Left)")
               (hass-dash-toggle :entity-id "light.30_de_4b_77_19_54" :label "Can (Right)"))

              (hass-dash-group
               :title "Basement — Rooms"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.6c_5a_b0_9b_38_ae" :label "Storage Room")
               (hass-dash-toggle :entity-id "switch.d8_47_32_9e_e9_15" :label "Server Room"
                                 :confirm t))

              (hass-dash-group
               :title "Basement — Media"
               :title-face outline-2
               (hass-dash-state  :entity-id "media_player.fire_tv_10_0_4_141" :label "Fire TV")
               (hass-dash-state  :entity-id "media_player.kodi_186ab93026605a815ffe66304538e245" :label "Kodi")
               (hass-dash-state  :entity-id "media_player.zach_s_tv"   :label "TV")
               (hass-dash-state  :entity-id "media_player.zach_s_tv_2" :label "TV 2"))

              (hass-dash-group
               :title "Garage — Freezers"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.e4_fa_c4_9e_5b_5c"
                                 :label "Freezer Plug Group" :confirm t)
               (hass-dash-toggle :entity-id "switch.e4_fa_c4_9e_5b_5c_e4_fa_c4_9e_5b_5c_1"
                                 :label "Freezer 1" :confirm t)
               (hass-dash-toggle :entity-id "switch.e4_fa_c4_9e_5b_5c_e4_fa_c4_9e_5b_5c_2"
                                 :label "Freezer 2" :confirm t))

              (hass-dash-group
               :title "Cameras — Driveway"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.bullet_driveway_detections_motion" :label "Motion")
               (hass-dash-toggle :entity-id "switch.bullet_driveway_privacy_mode"      :label "Privacy"))

              (hass-dash-group
               :title "Cameras — Front Yard"
               :title-face outline-2
               (hass-dash-toggle :entity-id "switch.bullet_frontyard_detections_motion" :label "Motion")
               (hass-dash-toggle :entity-id "switch.bullet_frontyard_privacy_mode"      :label "Privacy"))

              (hass-dash-group
               :title "Virtual"
               :title-face outline-2
               (hass-dash-toggle :entity-id "input_boolean.power_status" :label "Power Status")))))

          (quick .
            ((hass-dash-toggle :entity-id "switch.b4_b0_24_0e_09_83" :label "Lamp 01")
             (hass-dash-toggle :entity-id "switch.b4_b0_24_0e_0d_c5" :label "Lamp 02")
             (hass-dash-toggle :entity-id "switch.54_af_97_f4_25_28" :label "Lamp 03")
             (hass-dash-toggle :entity-id "switch.28_87_ba_6a_2a_ef" :label "Lamp 04")
             (hass-dash-toggle :entity-id "light.1c_61_b4_64_ea_42"  :label "Can (Left)")
             (hass-dash-toggle :entity-id "light.30_de_4b_77_19_54"  :label "Can (Right)")))))
  (map! :leader :desc "Home Assistant" "H" nil)
  (map! :leader
        :desc "HA dashboard"         "H d" #'hass-dash-open
        :desc "HA call service"      "H c" #'hass-call-service
        :desc "HA service+payload"   "H p" #'hass-call-service-with-payload
        :desc "HA ensure connection" "H e" #'hass-ensure))

(provide '+home-assistant)
;;; +home-assistant.el ends here
