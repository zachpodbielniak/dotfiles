;;; +games.el -*- lexical-binding: t; -*-
;;
;; +games.el - Games launcher + built-in and third-party game bundle
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
;; A single SPC G prefix that exposes every game we have in Emacs:
;;   - 18 built-in games (tetris, snake, pong, dunnet, gomoku, 5x5,
;;     blackbox, bubbles, solitaire, hanoi, mpuz, life, doctor, zone,
;;     spook, yow, decipher, animate).
;;   - 13 third-party games (elcity, chess, 2048, malyon, nethack,
;;     sudoku, minesweeper, poker, gnugo, speed-type, autotetris,
;;     fireplace, tamagotchi).
;;
;; The launcher (SPC G G) is a single `completing-read' over every
;; registered game, grouped by category.  The most-played games have
;; direct shortcuts: SPC G t (tetris), s (snake), d (dunnet), c
;; (chess), e (elcity), 2 (2048), m (malyon), n (nethack), S (sudoku),
;; T (speed-type), f (fireplace).
;;
;; Adding a new game is one `games-register' call anywhere — pass
;; :keybind to also get a direct shortcut (you'll need to add it to
;; the `map!' block below for it to show in which-key).
;;
;; External binaries:
;;   nethack — `dnf install nethack' (or your distro's equivalent)
;;   gnugo   — `dnf install gnugo'
;;   chess   — optional gnuchess/crafty for engine play; built-in AI works without.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; -----------------------------------------------------------------
;;; Registry

(defvar games--registry nil
  "Alist of (ID . PLIST) describing registered games.
Plist keys:
  :id          unique symbol
  :name        display string
  :category    grouping label (\"Action\", \"Puzzle\", ...)
  :description one-line blurb
  :cmd         interactive command symbol to call
  :source      symbol \\=`builtin', \\=`package', or \\=`recipe'
  :requires    optional 0-arg fn called before launch (for setup/load)
  :external    optional binary name that must be on PATH
  :keybind     optional single-char string for SPC G <key> (informational —
               the direct keys live in the `map!' block below)")

(defvar games--category-order
  '("Action" "Puzzle" "Strategy" "Simulation"
    "Text Adventure" "Roguelike" "Card" "Typing"
    "Toy" "Ambience")
  "Display order for the launcher's category grouping.")

(cl-defun games-register
    (&key id name category description cmd source requires external keybind)
  "Register a game in the launcher.

Required: ID (symbol), NAME (string), CATEGORY (string), DESCRIPTION (string),
CMD (interactive command symbol).
Optional: SOURCE (\\=`builtin', \\=`package', or \\=`recipe'), REQUIRES (0-arg fn run
before launch), EXTERNAL (binary name to executable-find), KEYBIND
(informational direct-key suffix under SPC G).

Re-registering an ID overwrites the previous entry."
  (unless (and (symbolp id) id)
    (error "games-register: :id must be a non-nil symbol"))
  (unless (stringp name)
    (error "games-register: :name must be a string"))
  (unless (and (symbolp cmd) cmd)
    (error "games-register: :cmd must be a non-nil command symbol"))
  (let ((entry (list :id id :name name
                     :category (or category "Misc")
                     :description (or description "")
                     :cmd cmd
                     :source (or source 'builtin)
                     :requires requires
                     :external external
                     :keybind keybind)))
    (setf (alist-get id games--registry) entry))
  ;; Define a thin wrapper command `games-launch/<id>' so direct
  ;; keybindings (and `M-x games-launch/<id>') route through
  ;; `games--launch' — which handles autoload, external-binary
  ;; checks, and the optional `:requires' hook.  Without this,
  ;; binding `#\\='chess' directly fails because the chess package
  ;; doesn't autoload its main command.
  (let ((sym (intern (format "games-launch/%s" id))))
    (defalias sym
      `(lambda () (interactive) (games--launch ',id))
      (format "Launch the `%s' game via the games registry." id)))
  id)

(defun games--sorted-registry ()
  "Return registry entries sorted by category (using `games--category-order')
then by name."
  (let ((entries (mapcar #'cdr games--registry)))
    (sort entries
          (lambda (a b)
            (let* ((ca (or (cl-position (plist-get a :category)
                                        games--category-order
                                        :test #'string-equal)
                           999))
                   (cb (or (cl-position (plist-get b :category)
                                        games--category-order
                                        :test #'string-equal)
                           999)))
              (cond
               ((< ca cb) t)
               ((> ca cb) nil)
               (t (string< (plist-get a :name) (plist-get b :name)))))))))

;;; -----------------------------------------------------------------
;;; Launcher

(defun games--check-external (entry)
  (when-let* ((bin (plist-get entry :external)))
    (unless (executable-find bin)
      (user-error
       "%s needs `%s' on PATH — try your distro's package manager (e.g. `dnf install %s')"
       (plist-get entry :name) bin bin))))

(defun games--ensure-feature (feature)
  "`require' FEATURE; on file-missing, try the command-name variant.
Lets real load errors surface (don't swallow them) but tolerates a
package whose feature name differs from its registry id."
  (condition-case _err
      (require feature)
    (file-missing nil)))

(defun games--launch (id)
  (let* ((entry (or (alist-get id games--registry)
                    (user-error "Unknown game: %s" id)))
         (cmd   (plist-get entry :cmd))
         (src   (plist-get entry :source)))
    (games--check-external entry)
    ;; Ensure the package is loaded for non-builtin entries.  We can't
    ;; rely on autoloads alone — GNU ELPA `chess', for example, ships
    ;; an autoload that may or may not be active depending on init
    ;; order — so we explicitly `require' by id, then fall back to the
    ;; command symbol's name if the id-feature is missing.
    (when (memq src '(package recipe))
      (unless (featurep id)
        (games--ensure-feature id))
      (unless (or (commandp cmd) (eq id cmd))
        (games--ensure-feature (intern (symbol-name cmd)))))
    (when-let* ((req (plist-get entry :requires)))
      (funcall req))
    ;; Let `call-interactively' produce the native "wrong type
    ;; argument: commandp" error if the command is still unbound —
    ;; that's more informative than a generic wrapper message.
    (call-interactively cmd)))

;;;###autoload
(defun games-launcher ()
  "Pick a game from the registry and launch it.
Entries are grouped by category (Action, Puzzle, Strategy, ...).
The selected game is launched via its registered command symbol;
external binaries are checked first with a friendly error."
  (interactive)
  (let* ((entries (games--sorted-registry))
         (rows
          (mapcar
           (lambda (p)
             (cons (format "%-22s  %-16s  %s"
                           (propertize (plist-get p :name)
                                       'face 'font-lock-function-name-face)
                           (propertize (format "[%s]"
                                               (plist-get p :category))
                                       'face 'font-lock-type-face)
                           (propertize (plist-get p :description)
                                       'face 'font-lock-comment-face))
                   (plist-get p :id)))
           entries))
         (completion-extra-properties
          (list :annotation-function
                (lambda (_) "")
                :group-function
                (lambda (cand transform)
                  (if transform cand
                    (let* ((id  (cdr (assoc cand rows)))
                           (ent (alist-get id games--registry)))
                      (or (plist-get ent :category) "Misc"))))))
         (choice (completing-read "Game: " (mapcar #'car rows) nil t)))
    (games--launch (cdr (assoc choice rows)))))

;;;###autoload
(defun games-show-keys ()
  "Show direct keybindings for the SPC G games prefix."
  (interactive)
  (with-current-buffer (get-buffer-create "*games-keys*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "SPC G — games keybindings\n"
                          'face 'font-lock-function-name-face))
      (insert (propertize "=========================\n\n"
                          'face 'font-lock-comment-face))
      (insert "Launcher:\n")
      (insert "  G   Launcher (pick any registered game via completing-read)\n")
      (insert "  ?   This help buffer\n\n")
      (insert "Direct shortcuts:\n")
      (dolist (entry (games--sorted-registry))
        (when-let* ((k (plist-get entry :keybind)))
          (insert (format "  %-3s %-22s %s\n"
                          k
                          (plist-get entry :name)
                          (plist-get entry :description)))))
      (insert "\nAll other games via SPC G G.\n"))
    (goto-char (point-min))
    (special-mode))
  (pop-to-buffer "*games-keys*"))

;;; -----------------------------------------------------------------
;;; Package configuration (third-party only — built-ins are autoloaded)

;; chess: keep it self-contained and in the current frame.
;;   * `chess-default-engine' to `chess-ai' keeps everything in-process
;;     (no gnuchess/crafty engine binary prompts).
;;   * `chess-images-separate-frame' nil keeps the image display in
;;     the same Emacs frame.  Default is `(display-multi-frame-p)' →
;;     t in GUI Emacs, which spawns a new frame that gowl/Wayland
;;     puts on another workspace, leaving the visible window showing
;;     the doom dashboard.
;;   * `chess-display-popup' t still lets the board pop into a side
;;     window (set to nil to keep in current window only).
(use-package! chess
  :defer t
  :init
  (setq chess-default-engine '(chess-ai)
        chess-images-separate-frame nil))

;; elcity ships its main command as `elcity'.  No init needed.
(use-package! elcity :defer t)

;; The rest just need to be reachable when called via the registry.
(use-package! 2048-game     :defer t)
(use-package! malyon        :defer t)
(use-package! nethack       :defer t)
(use-package! sudoku        :defer t)
(use-package! minesweeper   :defer t)
(use-package! poker         :defer t)
(use-package! gnugo         :defer t)
(use-package! speed-type    :defer t)
(use-package! autotetris-mode :defer t)
(use-package! fireplace     :defer t)
(use-package! tamagotchi    :defer t)

;;; -----------------------------------------------------------------
;;; Built-in registrations (18)

(games-register :id 'tetris    :name "Tetris"
                :category "Action"
                :description "Classic falling-blocks"
                :cmd #'tetris :source 'builtin :keybind "t")

(games-register :id 'snake     :name "Snake"
                :category "Action"
                :description "Eat dots, don't bite your tail"
                :cmd #'snake :source 'builtin :keybind "s")

(games-register :id 'pong      :name "Pong"
                :category "Action"
                :description "Two-paddle pong, two-player on one keyboard"
                :cmd #'pong :source 'builtin)

(games-register :id '5x5       :name "5x5"
                :category "Puzzle"
                :description "Toggle-grid logic puzzle (has a solver)"
                :cmd #'5x5 :source 'builtin)

(games-register :id 'blackbox  :name "Blackbox"
                :category "Puzzle"
                :description "Shoot rays into a box to deduce hidden balls"
                :cmd #'blackbox :source 'builtin)

(games-register :id 'bubbles   :name "Bubbles"
                :category "Puzzle"
                :description "Same-color blob clearer (real GUI graphics)"
                :cmd #'bubbles :source 'builtin)

(games-register :id 'solitaire :name "Solitaire"
                :category "Puzzle"
                :description "Peg solitaire (jump-over kind, not Klondike)"
                :cmd #'solitaire :source 'builtin)

(games-register :id 'hanoi     :name "Hanoi"
                :category "Puzzle"
                :description "Towers of Hanoi animation"
                :cmd #'hanoi :source 'builtin)

(games-register :id 'mpuz      :name "Mpuz"
                :category "Puzzle"
                :description "Multiplication cryptarithm puzzle"
                :cmd #'mpuz :source 'builtin)

(games-register :id 'life      :name "Life"
                :category "Puzzle"
                :description "Conway's Game of Life"
                :cmd #'life :source 'builtin)

(games-register :id 'gomoku    :name "Gomoku"
                :category "Strategy"
                :description "Five-in-a-row against Emacs"
                :cmd #'gomoku :source 'builtin)

(games-register :id 'dunnet    :name "Dunnet"
                :category "Text Adventure"
                :description "Ron Schnell's built-in Zork-like (the OG)"
                :cmd #'dunnet :source 'builtin :keybind "d")

(games-register :id 'doctor    :name "Doctor"
                :category "Toy"
                :description "ELIZA — the original chatbot"
                :cmd #'doctor :source 'builtin)

(games-register :id 'zone      :name "Zone"
                :category "Toy"
                :description "Screensaver: randomly mangles your buffer"
                :cmd #'zone :source 'builtin)

(games-register :id 'spook     :name "Spook"
                :category "Toy"
                :description "Random NSA bait — adds keywords to a buffer"
                :cmd #'spook :source 'builtin)

(games-register :id 'yow       :name "Yow"
                :category "Toy"
                :description "Random Zippy the Pinhead quote"
                :cmd #'yow :source 'builtin)

(games-register :id 'decipher  :name "Decipher"
                :category "Toy"
                :description "Interactive substitution-cipher solver"
                :cmd #'decipher :source 'builtin)

(games-register :id 'animate   :name "Animate"
                :category "Toy"
                :description "Animated text demo (try `animate-birthday-present')"
                :cmd #'animate-birthday-present :source 'builtin)

;;; -----------------------------------------------------------------
;;; Third-party registrations (13)

;; elcity.el is just a meta-file that loads the simulation submodules; the
;; player-facing entry point lives in `elcity-player.el', which isn't
;; required by the meta-file.  Require it explicitly.
(games-register :id 'elcity        :name "ElCity"
                :category "Simulation"
                :description "Turn-based SimCity clone in Emacs Lisp"
                :cmd #'elcity-player-start :source 'recipe :keybind "e"
                :requires (lambda () (require 'elcity-player)))

;; The tamagotchi-on-emacs repo's main file is `tamaemacs.el', providing
;; the feature `tamaemacs' (not `tamagotchi') and the command `tamaemacs'.
;; Our `--launch' fallback requires by command symbol's name, so just
;; pointing `:cmd' at #'tamaemacs is enough.
(games-register :id 'tamagotchi    :name "Tamagotchi"
                :category "Simulation"
                :description "Virtual pet — care for it, don't let it die"
                :cmd #'tamaemacs :source 'recipe)

(games-register :id 'chess         :name "Chess"
                :category "Strategy"
                :description "Full chess; built-in AI by default"
                :cmd #'chess :source 'package :keybind "c")

(games-register :id 'gnugo         :name "GNU Go"
                :category "Strategy"
                :description "Play Go against the GNU Go engine"
                :cmd #'gnugo :source 'package :external "gnugo")

(games-register :id '2048-game     :name "2048"
                :category "Puzzle"
                :description "Tile-merger; combine to reach 2048"
                :cmd #'2048-game :source 'package :keybind "2")

(games-register :id 'sudoku        :name "Sudoku"
                :category "Puzzle"
                :description "Sudoku with downloadable puzzles"
                :cmd #'sudoku :source 'package :keybind "S")

(games-register :id 'minesweeper   :name "Minesweeper"
                :category "Puzzle"
                :description "Classic minesweeper"
                :cmd #'minesweeper :source 'package)

(games-register :id 'poker         :name "Poker"
                :category "Card"
                :description "Texas Hold'em simulator"
                :cmd #'poker :source 'package)

(games-register :id 'malyon        :name "Malyon"
                :category "Text Adventure"
                :description "Z-machine interpreter — runs Infocom z3/z5/z8 files"
                :cmd #'malyon :source 'package :keybind "m")

(games-register :id 'nethack       :name "NetHack"
                :category "Roguelike"
                :description "Real NetHack driven from an Emacs buffer"
                :cmd #'nethack :source 'package :external "nethack" :keybind "n")

(games-register :id 'speed-type    :name "Speed-Type"
                :category "Typing"
                :description "Typing trainer using Project Gutenberg prose"
                :cmd #'speed-type-text :source 'package :keybind "T")

;; `autotetris-mode' is the minor-mode toggle (useless from a non-tetris
;; buffer); `autotetris' is the real entry point — starts tetris and
;; enables auto-play in one go.
(games-register :id 'autotetris    :name "Autotetris"
                :category "Action"
                :description "Watch Emacs play tetris by itself"
                :cmd #'autotetris :source 'package
                :requires (lambda () (require 'autotetris-mode)))

(games-register :id 'fireplace     :name "Fireplace"
                :category "Ambience"
                :description "Animated ASCII fireplace (not a game, everyone installs it)"
                :cmd #'fireplace :source 'package :keybind "f")

;;; -----------------------------------------------------------------
;;; Keybindings

;; All direct keys route through `games-launch/<id>' wrappers (defined
;; by `games-register' above) so that autoload, external-binary check,
;; and the `:requires' hook all run identically to the launcher path.
(map! :leader
      (:prefix ("G" . "games")
       :desc "Launcher (pick any game)" "G" #'games-launcher
       :desc "Show direct keys"         "?" #'games-show-keys
       ;; Action
       :desc "Tetris"                   "t" #'games-launch/tetris
       :desc "Snake"                    "s" #'games-launch/snake
       ;; Text adventure
       :desc "Dunnet"                   "d" #'games-launch/dunnet
       :desc "Malyon (Z-machine)"       "m" #'games-launch/malyon
       ;; Strategy
       :desc "Chess"                    "c" #'games-launch/chess
       ;; Simulation
       :desc "ElCity"                   "e" #'games-launch/elcity
       ;; Puzzle
       :desc "2048"                     "2" #'games-launch/2048-game
       :desc "Sudoku"                   "S" #'games-launch/sudoku
       ;; Roguelike
       :desc "NetHack"                  "n" #'games-launch/nethack
       ;; Typing
       :desc "Speed-type"               "T" #'games-launch/speed-type
       ;; Ambience
       :desc "Fireplace"                "f" #'games-launch/fireplace))

(provide '+games)
;;; +games.el ends here
