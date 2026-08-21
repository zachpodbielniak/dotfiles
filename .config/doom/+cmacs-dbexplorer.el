;;; +cmacs-dbexplorer.el --- Saved database connections -*- lexical-binding: t; -*-

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

;; Every database on the local PostgreSQL quadlet, as saved cmacs-dbexplorer
;; connections.
;;
;; A PostgreSQL URL names one database, so the ten databases on the single
;; `postgres-lt-zach' container are ten entries rather than one.  They differ
;; only in that last path element, which is why they are built rather than
;; written out: the host, the port and the user live in one place, and the
;; other two postgres quadlets (`postgres-srv-zach', `postgres-hacbook')
;; become a second call to the same helper if they are ever wanted here.
;;
;; No passwords, and none needed.  The quadlet sets
;; POSTGRES_HOST_AUTH_METHOD=trust, so the URL alone authenticates and there
;; is nothing for auth-source to supply.  If that container is ever switched
;; to scram or md5, nothing in this file changes -- a URL that names a user
;; and a host and carries no password is completed from auth-source at
;; connect time, so one authinfo line covers all ten:
;;
;;   machine 127.0.0.1 login postgres port 5432 password SECRET
;;
;; The `require' at the end is load-bearing under Doom.  Doom regenerates its
;; own autoloads and keeps only what it scanned itself, dropping everything
;; the cmacs pdump knew -- so without it `M-x cmacs-dbexplorer' is
;; `void-function' and reads as a subsystem that was never built.

;;; Code:

(defconst +cmacs-dbexplorer-local-pg-host "127.0.0.1"
  "Host for the local PostgreSQL quadlet.

The container publishes on 127.0.0.1 and on the Tailscale address
100.72.0.41; the loopback one is correct from this machine and keeps the
connection off the network entirely.")

(defconst +cmacs-dbexplorer-local-pg-port 5432
  "Port for the local PostgreSQL quadlet.")

(defconst +cmacs-dbexplorer-local-pg-user "postgres"
  "Role used for the local PostgreSQL quadlet.")

(defun +cmacs-dbexplorer-local-pg (database &rest settings)
  "Return a saved-connection entry for DATABASE on the local quadlet.

SETTINGS is spliced into the entry's plist, so a database that wants
`:read-only' or `:auto-connect' says so at its own call site.  The
connection is named after DATABASE with underscores turned into hyphens,
because that name is the identity the whole explorer uses -- it appears in
buffer names, it is what completion offers, and it is how the MCP, D-Bus
and emacsctl surfaces address the connection."
  (cons (concat "pg-" (string-replace "_" "-" database))
        (append (list :url (format "postgresql://%s@%s:%d/%s"
                                   +cmacs-dbexplorer-local-pg-user
                                   +cmacs-dbexplorer-local-pg-host
                                   +cmacs-dbexplorer-local-pg-port
                                   database))
                settings)))

(setq cmacs-dbexplorer-connections
      (list
       ;; Read-only, both of them, because both are reachable by an agent.
       ;; The brigade db tools, MCP, D-Bus and emacsctl all address a
       ;; connection by name and inherit whatever that connection allows, so
       ;; the flag is the one place that decides it for every surface at
       ;; once.  Lift it per-session with `r' in the connections buffer when
       ;; a write is actually wanted.
       (+cmacs-dbexplorer-local-pg "agent_memories" :read-only t)
       (+cmacs-dbexplorer-local-pg "ai_chats")
       (+cmacs-dbexplorer-local-pg "cold_storage")
       (+cmacs-dbexplorer-local-pg "flashcards")
       (+cmacs-dbexplorer-local-pg "media")
       (+cmacs-dbexplorer-local-pg "notes" :read-only t)
       (+cmacs-dbexplorer-local-pg "possessions")
       (+cmacs-dbexplorer-local-pg "rag")
       (+cmacs-dbexplorer-local-pg "transcriptions")
       ;; The maintenance database.  Nothing of yours lives here; it is the
       ;; one to connect to for server-wide questions -- pg_database,
       ;; pg_stat_activity, roles, sizes -- which are not visible from
       ;; inside any single database.
       (+cmacs-dbexplorer-local-pg "postgres")))

(require 'cmacs-dbexplorer-autoloads nil t)

(provide '+cmacs-dbexplorer)
;;; +cmacs-dbexplorer.el ends here
