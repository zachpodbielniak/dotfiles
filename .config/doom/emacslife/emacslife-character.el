;;; emacslife-character.el --- EmacsLife: character struct & creation -*- lexical-binding: t; -*-
;;
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
;; The protagonist data model.  One `cl-defstruct emacslife-character'
;; holds everything that gets serialized.  Helpers for creating a new
;; character (random or custom), modifying stats, advancing age, and
;; querying country/age laws.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)

;;; -----------------------------------------------------------------
;;; Country registry

(defvar emacslife--countries nil
  "Alist of (ID . PLIST) for each playable country.
PLIST keys:
  :name string
  :currency string  (display symbol, e.g. \"$\", \"€\")
  :currency-mult float (relative cost-of-living vs USD)
  :drinking-age int
  :driving-age int
  :marriage-age int
  :crime-tolerance float (0..1, higher = lighter sentences)
  :starting-cash int
  :has-royalty bool
  :has-monarchy bool
  :draft-age int or nil
  :first-names-male  list
  :first-names-female list
  :surnames list")

(defun emacslife-register-country (id &rest plist)
  "Register country ID with PLIST."
  (setf (alist-get id emacslife--countries) plist)
  id)

(defun emacslife-country (id)
  "Look up country ID; returns plist or signals."
  (or (alist-get id emacslife--countries)
      (error "Unknown country: %S" id)))

;;; -----------------------------------------------------------------
;;; Country definitions (12 — broad coverage, distinct flavors)

(emacslife-register-country 'usa
  :name "United States" :currency "$" :currency-mult 1.0
  :drinking-age 21 :driving-age 16 :marriage-age 18
  :crime-tolerance 0.4 :starting-cash 0 :has-royalty nil :draft-age nil
  :first-names-male '("James" "John" "Robert" "Michael" "William" "David"
                      "Richard" "Joseph" "Thomas" "Charles" "Christopher"
                      "Daniel" "Matthew" "Anthony" "Mark" "Donald" "Steven"
                      "Paul" "Andrew" "Joshua" "Kenneth" "Kevin" "Brian"
                      "George" "Edward" "Ronald" "Timothy" "Jason" "Jeffrey")
  :first-names-female '("Mary" "Patricia" "Jennifer" "Linda" "Elizabeth"
                        "Barbara" "Susan" "Jessica" "Sarah" "Karen" "Lisa"
                        "Nancy" "Betty" "Helen" "Sandra" "Donna" "Carol"
                        "Ruth" "Sharon" "Michelle" "Laura" "Emily" "Kimberly"
                        "Deborah" "Dorothy" "Amy" "Angela" "Ashley" "Brenda")
  :surnames '("Smith" "Johnson" "Williams" "Brown" "Jones" "Garcia" "Miller"
              "Davis" "Rodriguez" "Martinez" "Hernandez" "Lopez" "Gonzalez"
              "Wilson" "Anderson" "Thomas" "Taylor" "Moore" "Jackson" "Martin"
              "Lee" "Perez" "Thompson" "White" "Harris" "Sanchez" "Clark"))

(emacslife-register-country 'uk
  :name "United Kingdom" :currency "£" :currency-mult 0.8
  :drinking-age 18 :driving-age 17 :marriage-age 18
  :crime-tolerance 0.5 :starting-cash 0 :has-royalty t :draft-age nil
  :first-names-male '("Oliver" "Harry" "George" "Noah" "Jack" "Leo" "Oscar"
                      "Charlie" "Henry" "Jacob" "Alfie" "Freddie" "Theo"
                      "Archie" "Ethan" "Isaac" "Joshua" "James" "William")
  :first-names-female '("Olivia" "Amelia" "Isla" "Ava" "Mia" "Isabella"
                        "Sophia" "Grace" "Lily" "Freya" "Emily" "Ivy" "Ella"
                        "Rosie" "Evie" "Florence" "Poppy" "Charlotte")
  :surnames '("Smith" "Jones" "Taylor" "Brown" "Williams" "Wilson" "Johnson"
              "Davies" "Robinson" "Wright" "Thompson" "Evans" "Walker"
              "White" "Roberts" "Green" "Hall" "Wood" "Jackson" "Clarke"))

(emacslife-register-country 'germany
  :name "Germany" :currency "€" :currency-mult 0.85
  :drinking-age 16 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.55 :starting-cash 0 :has-royalty nil :draft-age nil
  :first-names-male '("Lukas" "Maximilian" "Felix" "Paul" "Leon" "Jonas"
                      "Finn" "Elias" "Ben" "Noah" "Luis" "Tim" "Niklas"
                      "Tobias" "Hans" "Klaus" "Dieter" "Wolfgang" "Stefan")
  :first-names-female '("Hanna" "Sophie" "Marie" "Emma" "Mia" "Anna" "Lena"
                        "Lea" "Leonie" "Greta" "Lina" "Sarah" "Laura"
                        "Katharina" "Ingrid" "Gertrud" "Heidi" "Helga")
  :surnames '("Müller" "Schmidt" "Schneider" "Fischer" "Weber" "Meyer"
              "Wagner" "Becker" "Schulz" "Hoffmann" "Schäfer" "Koch"
              "Bauer" "Richter" "Klein" "Wolf" "Schröder" "Neumann"))

(emacslife-register-country 'japan
  :name "Japan" :currency "¥" :currency-mult 0.0085
  :drinking-age 20 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.2 :starting-cash 0 :has-royalty t :draft-age nil
  :first-names-male '("Haruto" "Yuto" "Sota" "Yuki" "Hayato" "Haruki" "Ryusei"
                      "Koki" "Sora" "Itsuki" "Hiroto" "Riku" "Takeshi" "Akira")
  :first-names-female '("Yui" "Aoi" "Hina" "Yuna" "Akari" "Sakura" "Mei" "Rin"
                        "Hana" "Mio" "Yua" "Tsumugi" "Ichika" "Yume" "Saki")
  :surnames '("Sato" "Suzuki" "Takahashi" "Tanaka" "Watanabe" "Ito" "Yamamoto"
              "Nakamura" "Kobayashi" "Kato" "Yoshida" "Yamada" "Sasaki"
              "Yamaguchi" "Saito" "Matsumoto" "Inoue" "Kimura"))

(emacslife-register-country 'france
  :name "France" :currency "€" :currency-mult 0.9
  :drinking-age 18 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.5 :starting-cash 0 :has-royalty nil :draft-age nil
  :first-names-male '("Gabriel" "Léo" "Raphaël" "Louis" "Arthur" "Hugo" "Jules"
                      "Lucas" "Adam" "Maël" "Noah" "Liam" "Sacha" "Tiago"
                      "Pierre" "Jean" "François" "Antoine")
  :first-names-female '("Emma" "Jade" "Louise" "Alice" "Chloé" "Lina" "Léa"
                        "Manon" "Rose" "Anna" "Inès" "Mila" "Ambre" "Camille"
                        "Marie" "Brigitte" "Françoise" "Catherine")
  :surnames '("Martin" "Bernard" "Dubois" "Thomas" "Robert" "Richard" "Petit"
              "Durand" "Leroy" "Moreau" "Simon" "Laurent" "Lefebvre" "Michel"
              "Garcia" "David" "Bertrand" "Roux" "Vincent" "Fournier"))

(emacslife-register-country 'brazil
  :name "Brazil" :currency "R$" :currency-mult 0.20
  :drinking-age 18 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.3 :starting-cash 0 :has-royalty nil :draft-age 18
  :first-names-male '("Miguel" "Arthur" "Heitor" "Bernardo" "Théo" "Davi"
                      "Gabriel" "Pedro" "Lucas" "Matheus" "Rafael" "Enzo"
                      "João" "Carlos" "Paulo" "José")
  :first-names-female '("Helena" "Alice" "Laura" "Manuela" "Sophia" "Maitê"
                        "Liz" "Cecília" "Isabella" "Heloísa" "Beatriz" "Júlia"
                        "Maria" "Ana" "Camila" "Larissa")
  :surnames '("Silva" "Santos" "Oliveira" "Souza" "Lima" "Pereira" "Ferreira"
              "Costa" "Rodrigues" "Almeida" "Nascimento" "Carvalho" "Araújo"
              "Ribeiro" "Alves" "Gomes" "Martins" "Rocha"))

(emacslife-register-country 'india
  :name "India" :currency "₹" :currency-mult 0.012
  :drinking-age 21 :driving-age 18 :marriage-age 21
  :crime-tolerance 0.35 :starting-cash 0 :has-royalty nil :draft-age nil
  :first-names-male '("Aarav" "Vihaan" "Reyansh" "Mohammed" "Sai" "Arjun"
                      "Ayaan" "Krishna" "Ishaan" "Shaurya" "Rudra" "Vivaan"
                      "Aryan" "Kabir" "Rohan" "Raj" "Vikram")
  :first-names-female '("Saanvi" "Aanya" "Aadhya" "Aaradhya" "Ananya" "Pari"
                        "Diya" "Anvi" "Riya" "Ira" "Priya" "Kavya" "Meera"
                        "Sneha" "Pooja" "Anjali")
  :surnames '("Sharma" "Singh" "Kumar" "Gupta" "Patel" "Verma" "Yadav"
              "Mishra" "Chauhan" "Reddy" "Iyer" "Mehta" "Kapoor" "Joshi"
              "Khan" "Nair" "Pillai" "Bose"))

(emacslife-register-country 'australia
  :name "Australia" :currency "A$" :currency-mult 0.65
  :drinking-age 18 :driving-age 16 :marriage-age 18
  :crime-tolerance 0.5 :starting-cash 0 :has-royalty t :draft-age nil
  :first-names-male '("Oliver" "Jack" "Noah" "William" "Leo" "Lucas" "Thomas"
                      "Henry" "Charlie" "James" "Ethan" "Mason" "Bruce"
                      "Shane" "Wayne")
  :first-names-female '("Charlotte" "Olivia" "Mia" "Amelia" "Isla" "Ava"
                        "Grace" "Willow" "Harper" "Chloe" "Sheila" "Kylie")
  :surnames '("Smith" "Jones" "Williams" "Brown" "Wilson" "Taylor" "Johnson"
              "White" "Martin" "Anderson" "Thompson" "Walker" "Nguyen"))

(emacslife-register-country 'canada
  :name "Canada" :currency "C$" :currency-mult 0.75
  :drinking-age 19 :driving-age 16 :marriage-age 18
  :crime-tolerance 0.55 :starting-cash 0 :has-royalty t :draft-age nil
  :first-names-male '("Liam" "Noah" "Benjamin" "William" "Lucas" "Henry"
                      "Theodore" "Oliver" "Jack" "James" "Pierre")
  :first-names-female '("Olivia" "Charlotte" "Emma" "Amelia" "Ava" "Sophia"
                        "Isabella" "Mia" "Evelyn" "Harper" "Céline")
  :surnames '("Smith" "Brown" "Tremblay" "Martin" "Roy" "Wilson" "MacDonald"
              "Gagnon" "Johnson" "Taylor" "Anderson" "Lévesque" "Bouchard"))

(emacslife-register-country 'mexico
  :name "Mexico" :currency "MX$" :currency-mult 0.055
  :drinking-age 18 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.3 :starting-cash 0 :has-royalty nil :draft-age 18
  :first-names-male '("Santiago" "Mateo" "Sebastián" "Leonardo" "Matías"
                      "Diego" "Emiliano" "Daniel" "Nicolás" "Alejandro"
                      "Miguel" "Pablo" "Luis" "Jesús")
  :first-names-female '("Sofía" "Valentina" "Camila" "Isabella" "Valeria"
                        "Mariana" "Daniela" "Lucía" "Sara" "Andrea"
                        "Guadalupe" "María")
  :surnames '("García" "Martínez" "López" "Hernández" "González" "Pérez"
              "Rodríguez" "Sánchez" "Ramírez" "Torres" "Flores" "Rivera"
              "Gómez" "Díaz" "Cruz" "Reyes" "Morales"))

(emacslife-register-country 'sweden
  :name "Sweden" :currency "kr" :currency-mult 0.095
  :drinking-age 20 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.7 :starting-cash 0 :has-royalty t :draft-age 18
  :first-names-male '("Lucas" "Liam" "Hugo" "William" "Adam" "Oliver" "Oscar"
                      "Elias" "Alexander" "Axel" "Sven" "Lars" "Bjørn")
  :first-names-female '("Alice" "Maja" "Vera" "Lilly" "Olivia" "Astrid" "Wilma"
                        "Ella" "Ebba" "Alma" "Greta" "Ingrid")
  :surnames '("Andersson" "Johansson" "Karlsson" "Nilsson" "Eriksson" "Larsson"
              "Olsson" "Persson" "Svensson" "Gustafsson" "Pettersson" "Berg"))

(emacslife-register-country 'nigeria
  :name "Nigeria" :currency "₦" :currency-mult 0.0011
  :drinking-age 18 :driving-age 18 :marriage-age 18
  :crime-tolerance 0.35 :starting-cash 0 :has-royalty nil :draft-age nil
  :first-names-male '("Chinedu" "Emeka" "Ifeanyi" "Obi" "Tunde" "Kunle"
                      "Bola" "Ade" "Olumide" "Femi" "Segun" "Yusuf" "Musa")
  :first-names-female '("Adaeze" "Chioma" "Ngozi" "Amaka" "Folake" "Bukola"
                        "Yetunde" "Aisha" "Fatima" "Halima" "Funmi" "Tola")
  :surnames '("Adeyemi" "Okafor" "Eze" "Okeke" "Bello" "Mohammed" "Yusuf"
              "Adebayo" "Ogunyemi" "Obi" "Nwosu" "Abubakar"))

;;; -----------------------------------------------------------------
;;; Character struct

(cl-defstruct emacslife-character
  ;; identity (`orientation' slot retained for save-file compatibility
  ;; with v1 lives — no longer set or read)
  name surname gender orientation
  country city
  ;; time
  age birth-year alive death-cause
  ;; stats (0..100)
  happiness health smarts looks karma
  ;; money
  cash credit-score debt
  ;; education/career
  education-stage gpa degrees major
  job job-history
  fame
  ;; family/social
  parents siblings spouse children ex-spouses
  npcs                                 ; alist npc-id → emacslife-npc
  ;; assets/legal
  assets investments
  jail-record prison-time-served paroled
  pets
  ;; meta
  timeline ribbons achievements
  god-mode metadata)

;;; -----------------------------------------------------------------
;;; Character creation

(defun emacslife--random-name (country gender)
  "Pick a random first name for COUNTRY (id) and GENDER (\\='male / \\='female)."
  (let* ((c (emacslife-country country))
         (pool (if (eq gender 'female)
                   (plist-get c :first-names-female)
                 (plist-get c :first-names-male))))
    (emacslife-pick pool)))

(defun emacslife--random-surname (country)
  (emacslife-pick (plist-get (emacslife-country country) :surnames)))

(defun emacslife--random-stat (&optional skew)
  "Return a random stat 0..100, optionally SKEWed toward HIGH/LOW.
SKEW is :high, :low, or nil."
  (let ((base (random 101)))
    (cond
     ((eq skew :high) (emacslife-clamp (+ base 20)))
     ((eq skew :low)  (emacslife-clamp (- base 20)))
     (t base))))

(defun emacslife--current-year ()
  (string-to-number (format-time-string "%Y")))

(defun emacslife-create-random-character ()
  "Build a fresh newborn character with randomized everything."
  (let* ((country (or (emacslife-pick (mapcar #'car emacslife--countries))
                      'usa))
         (gender (emacslife-pick '(male female)))
         (name (emacslife--random-name country gender))
         (surname (emacslife--random-surname country))
         (year (emacslife--current-year))
         (cdata (emacslife-country country)))
    (make-emacslife-character
     :name name :surname surname :gender gender
     :country country :city "Hometown"
     :age 0 :birth-year year :alive t :death-cause nil
     :happiness (emacslife--random-stat)
     :health    (emacslife--random-stat)
     :smarts    (emacslife--random-stat)
     :looks     (emacslife--random-stat)
     :karma     50
     :cash (plist-get cdata :starting-cash)
     :credit-score 700 :debt 0
     :education-stage 'none :gpa 0.0 :degrees nil :major nil
     :job nil :job-history nil :fame 0
     :parents nil :siblings nil :spouse nil :children nil :ex-spouses nil
     :npcs nil
     :assets nil :investments nil
     :jail-record nil :prison-time-served 0 :paroled nil :pets nil
     :timeline nil :ribbons nil :achievements nil
     :god-mode emacslife-god-mode
     :metadata (list :created (emacslife-iso-date)
                     :save-version emacslife-save-format-version))))

(defun emacslife-create-custom-character ()
  "Prompt for each field and build a character (God-Mode style)."
  (let* ((countries (mapcar (lambda (c)
                              (cons (plist-get (cdr c) :name) (car c)))
                            emacslife--countries))
         (country (cdr (assoc (completing-read "Country: " countries nil t) countries)))
         (gender (intern (completing-read "Gender: "
                                          '("male" "female") nil t)))
         (name (read-string "First name: " (emacslife--random-name country gender)))
         (surname (read-string "Surname: " (emacslife--random-surname country)))
         (happiness (read-number "Happiness (0-100): " 70))
         (health    (read-number "Health (0-100): " 80))
         (smarts    (read-number "Smarts (0-100): " 60))
         (looks     (read-number "Looks (0-100): " 60))
         (year (emacslife--current-year))
         (cdata (emacslife-country country)))
    (make-emacslife-character
     :name name :surname surname :gender gender
     :country country :city "Hometown"
     :age 0 :birth-year year :alive t :death-cause nil
     :happiness (emacslife-clamp happiness) :health (emacslife-clamp health)
     :smarts (emacslife-clamp smarts) :looks (emacslife-clamp looks)
     :karma 50
     :cash (plist-get cdata :starting-cash)
     :credit-score 700 :debt 0
     :education-stage 'none :gpa 0.0 :degrees nil :major nil
     :job nil :job-history nil :fame 0
     :parents nil :siblings nil :spouse nil :children nil :ex-spouses nil
     :npcs nil
     :assets nil :investments nil
     :jail-record nil :prison-time-served 0 :paroled nil :pets nil
     :timeline nil :ribbons nil :achievements nil
     :god-mode t
     :metadata (list :created (emacslife-iso-date)
                     :save-version emacslife-save-format-version))))

;;; -----------------------------------------------------------------
;;; Stat operations

(defun emacslife-stat (char stat)
  "Read STAT from CHAR.  STAT is :happiness, :health, :smarts, :looks, :karma."
  (pcase stat
    (:happiness (emacslife-character-happiness char))
    (:health    (emacslife-character-health char))
    (:smarts    (emacslife-character-smarts char))
    (:looks     (emacslife-character-looks char))
    (:karma     (emacslife-character-karma char))
    (_ (error "Unknown stat: %S" stat))))

(defun emacslife-set-stat (char stat value)
  "Set STAT on CHAR to clamped VALUE."
  (let ((v (emacslife-clamp value)))
    (pcase stat
      (:happiness (setf (emacslife-character-happiness char) v))
      (:health    (setf (emacslife-character-health char) v))
      (:smarts    (setf (emacslife-character-smarts char) v))
      (:looks     (setf (emacslife-character-looks char) v))
      (:karma     (setf (emacslife-character-karma char) v))
      (_ (error "Unknown stat: %S" stat)))))

(defun emacslife-bump-stat (char stat delta)
  "Add DELTA to CHAR's STAT (clamped)."
  (emacslife-set-stat char stat (+ (emacslife-stat char stat) delta)))

(defun emacslife-bump-money (char delta)
  "Add DELTA to CHAR's cash (no clamp — debt is possible)."
  (setf (emacslife-character-cash char)
        (+ (emacslife-character-cash char) delta)))

(defun emacslife-apply-deltas (char deltas)
  "Apply DELTAS plist to CHAR.  Keys: :happiness :health :smarts :looks
:karma :cash :fame.  Values are integer changes."
  (when-let* ((d (plist-get deltas :happiness))) (emacslife-bump-stat char :happiness d))
  (when-let* ((d (plist-get deltas :health)))    (emacslife-bump-stat char :health d))
  (when-let* ((d (plist-get deltas :smarts)))    (emacslife-bump-stat char :smarts d))
  (when-let* ((d (plist-get deltas :looks)))     (emacslife-bump-stat char :looks d))
  (when-let* ((d (plist-get deltas :karma)))     (emacslife-bump-stat char :karma d))
  (when-let* ((d (plist-get deltas :cash)))      (emacslife-bump-money char d))
  (when-let* ((d (plist-get deltas :fame)))
    (setf (emacslife-character-fame char)
          (emacslife-clamp (+ (emacslife-character-fame char) d)))))

;;; -----------------------------------------------------------------
;;; Display helpers

(defun emacslife-character-full-name (char)
  "First + surname."
  (format "%s %s"
          (emacslife-character-name char)
          (emacslife-character-surname char)))

(defun emacslife-country-name (char)
  (plist-get (emacslife-country (emacslife-character-country char)) :name))

(defun emacslife-country-currency-symbol (char)
  (plist-get (emacslife-country (emacslife-character-country char)) :currency))

(defun emacslife-localized-money (char n)
  "Format N in CHAR's country currency (multiplier-converted)."
  (let* ((c (emacslife-country (emacslife-character-country char)))
         (mult (plist-get c :currency-mult))
         (sym  (plist-get c :currency))
         (local (round (/ n (max 0.0001 mult)))))
    (let* ((abs (abs local))
           (s (cond
               ((>= abs 1000000000) (format "%s%.2fB" sym (/ (float local) 1e9)))
               ((>= abs 1000000)    (format "%s%.2fM" sym (/ (float local) 1e6)))
               ((>= abs 1000)       (format "%s%.1fK" sym (/ (float local) 1e3)))
               (t                   (format "%s%d" sym local)))))
      s)))

;;; -----------------------------------------------------------------
;;; Timeline log helper

(defun emacslife-log (char text &optional tone)
  "Append TEXT to CHAR's timeline at CHAR's current age.
TONE is :good, :bad, :neutral (default), or :funny — used for face."
  (let ((entry (list (emacslife-character-age char)
                     :text text
                     :tone (or tone :neutral)
                     :year (+ (emacslife-character-birth-year char)
                              (emacslife-character-age char)))))
    (setf (emacslife-character-timeline char)
          (append (emacslife-character-timeline char) (list entry)))))

;;; -----------------------------------------------------------------
;;; Country/age law queries

(defun emacslife-age-of-majority (char)
  "Generally 18 in our model; some countries differ."
  (plist-get (emacslife-country (emacslife-character-country char)) :marriage-age))

(defun emacslife-can-drink-p (char)
  (>= (emacslife-character-age char)
      (plist-get (emacslife-country (emacslife-character-country char)) :drinking-age)))

(defun emacslife-can-drive-p (char)
  (>= (emacslife-character-age char)
      (plist-get (emacslife-country (emacslife-character-country char)) :driving-age)))

(defun emacslife-can-marry-p (char)
  (>= (emacslife-character-age char)
      (plist-get (emacslife-country (emacslife-character-country char)) :marriage-age)))

(provide 'emacslife-character)
;;; emacslife-character.el ends here
