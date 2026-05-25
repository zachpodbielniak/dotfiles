;;; emacslife-skills.el --- EmacsLife: skills + languages + practice -*- lexical-binding: t; -*-
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
;; Hobby/talent skills (music, writing, art, coding, cooking, sports,
;; photography, gardening, dancing) — each 0..100, leveled by
;; activities, used to gate special careers or unlock content.  Also
;; a separate languages plist for polyglot achievement chasing.
;;
;; Stored on the character via `emacslife-character-metadata' so we
;; don't need to bump the struct version (existing saves get defaults).

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-activities)

;;; -----------------------------------------------------------------
;;; Skill catalogue

(defconst emacslife-skill-names
  '((music       . "Music")
    (writing     . "Writing")
    (art         . "Art")
    (coding      . "Coding")
    (cooking     . "Cooking")
    (sports      . "Sports")
    (photography . "Photography")
    (gardening   . "Gardening")
    (dancing     . "Dancing"))
  "All skill IDs and display names.")

(defconst emacslife-language-names
  '((english     . "English")
    (spanish     . "Spanish")
    (french      . "French")
    (german      . "German")
    (mandarin    . "Mandarin")
    (japanese    . "Japanese")
    (italian     . "Italian")
    (portuguese  . "Portuguese")
    (russian     . "Russian")
    (arabic      . "Arabic")
    (hindi       . "Hindi")
    (korean      . "Korean")
    (latin       . "Latin")
    (esperanto   . "Esperanto"))
  "All language IDs and display names.")

;;; -----------------------------------------------------------------
;;; Accessors (skills + languages live in metadata)

(defun emacslife-skill (char skill)
  "Return CHAR's level in SKILL (0..100, default 0)."
  (or (plist-get (plist-get (emacslife-character-metadata char) :skills)
                 skill)
      0))

(defun emacslife-set-skill (char skill value)
  "Set CHAR's SKILL level to clamped VALUE."
  (let* ((md (or (emacslife-character-metadata char) '()))
         (skills (or (plist-get md :skills) '())))
    (setq skills (plist-put skills skill (emacslife-clamp value)))
    (setf (emacslife-character-metadata char)
          (plist-put md :skills skills))))

(defun emacslife-bump-skill (char skill delta)
  (emacslife-set-skill char skill (+ (emacslife-skill char skill) delta)))

(defun emacslife-all-skills (char)
  "Return alist (SKILL-ID . LEVEL) for all skills, including zeros."
  (mapcar (lambda (cell)
            (cons (car cell) (emacslife-skill char (car cell))))
          emacslife-skill-names))

(defun emacslife-language-level (char lang)
  "Return CHAR's proficiency in LANG (0..100), default 0.
The character's birth-country language defaults to 100."
  (let ((stored (plist-get (plist-get (emacslife-character-metadata char) :languages)
                           lang)))
    (or stored
        (let* ((country (emacslife-character-country char))
               (native (pcase country
                         ('usa 'english) ('uk 'english) ('canada 'english)
                         ('australia 'english)
                         ('germany 'german) ('france 'french)
                         ('japan 'japanese) ('brazil 'portuguese)
                         ('mexico 'spanish) ('sweden 'english) ; bilingual-ish
                         ('india 'hindi) ('nigeria 'english)
                         (_ 'english))))
          (if (eq lang native) 100 0)))))

(defun emacslife-set-language-level (char lang value)
  (let* ((md (or (emacslife-character-metadata char) '()))
         (langs (or (plist-get md :languages) '())))
    (setq langs (plist-put langs lang (emacslife-clamp value)))
    (setf (emacslife-character-metadata char)
          (plist-put md :languages langs))))

(defun emacslife-bump-language (char lang delta)
  (emacslife-set-language-level char lang
                                 (+ (emacslife-language-level char lang) delta)))

(defun emacslife-languages-spoken (char &optional threshold)
  "Return list of LANG IDs the char speaks at THRESHOLD (default 60)."
  (let ((thr (or threshold 60)))
    (cl-remove-if-not
     (lambda (lang) (>= (emacslife-language-level char lang) thr))
     (mapcar #'car emacslife-language-names))))

;;; -----------------------------------------------------------------
;;; Streak tracking — reward consecutive years of the same activity

(defun emacslife--streak-key (id) (intern (format ":streak-%s" id)))
(defun emacslife--streak-year-key (id) (intern (format ":streak-year-%s" id)))

(defun emacslife-track-streak (char id)
  "Update streak counter for activity ID; return new streak count."
  (let* ((md (or (emacslife-character-metadata char) '()))
         (year (emacslife-character-age char))
         (last (plist-get md (emacslife--streak-year-key id)))
         (streak (or (plist-get md (emacslife--streak-key id)) 0))
         (new (if (and last (= (- year last) 1)) (1+ streak) 1)))
    (setq md (plist-put md (emacslife--streak-key id) new))
    (setq md (plist-put md (emacslife--streak-year-key id) year))
    (setf (emacslife-character-metadata char) md)
    new))

(defun emacslife-streak (char id)
  "Read current streak for activity ID."
  (or (plist-get (emacslife-character-metadata char)
                 (emacslife--streak-key id))
      0))

;;; -----------------------------------------------------------------
;;; Skill-up activities

(emacslife-register-activity
 :id 'practice-music :label "Practice music" :category 'skill :min-age 6
 :cost 20 :description "Music skill +2..4"
 :handler
 (lambda (char)
   (let ((d (+ 2 (random 3))))
     (emacslife-bump-skill char 'music d)
     (let ((s (emacslife-track-streak char 'practice-music)))
       (when (and (> s 1) (= 0 (mod s 5)))
         (emacslife-bump-skill char 'music 5)
         (emacslife-log char (format "Music %d-year streak! Bonus +5 skill." s) :good)))
     (emacslife-bump-stat char :happiness 1)
     (emacslife-log char (format "Practiced music. Skill now %d."
                                  (emacslife-skill char 'music))
                    :good)
     (message "Music skill +%d." d))))

(emacslife-register-activity
 :id 'write-daily :label "Write something" :category 'skill :min-age 6
 :description "Writing skill +1..3"
 :handler
 (lambda (char)
   (let ((d (+ 1 (random 3))))
     (emacslife-bump-skill char 'writing d)
     (when (= 0 (mod (emacslife-track-streak char 'write-daily) 5))
       (emacslife-bump-skill char 'writing 5))
     (emacslife-bump-stat char :smarts 1)
     (emacslife-log char (format "Wrote daily. Writing now %d."
                                  (emacslife-skill char 'writing))
                    :good))))

(emacslife-register-activity
 :id 'take-art-class :label "Take an art class" :category 'skill :min-age 8
 :cost 50 :description "Art skill +2..4"
 :handler
 (lambda (char)
   (let ((d (+ 2 (random 3))))
     (emacslife-bump-skill char 'art d)
     (emacslife-bump-stat char :happiness 2)
     (emacslife-log char (format "Art class. Skill now %d."
                                  (emacslife-skill char 'art)) :good))))

(emacslife-register-activity
 :id 'code-github :label "Code on github" :category 'skill :min-age 10
 :description "Coding +1..3, smarts +"
 :handler
 (lambda (char)
   (let ((d (+ 1 (random 3))))
     (emacslife-bump-skill char 'coding d)
     (emacslife-bump-stat char :smarts 1)
     (emacslife-log char (format "Coded all weekend. Coding skill now %d."
                                  (emacslife-skill char 'coding)) :good))))

(emacslife-register-activity
 :id 'cooking-class :label "Cooking class" :category 'skill :min-age 10
 :cost 75 :description "Cooking +2..4"
 :handler
 (lambda (char)
   (let ((d (+ 2 (random 3))))
     (emacslife-bump-skill char 'cooking d)
     (emacslife-bump-stat char :happiness 2)
     (emacslife-log char (format "Cooking class. Skill now %d."
                                  (emacslife-skill char 'cooking)) :good))))

(emacslife-register-activity
 :id 'sports-league :label "Join sports league" :category 'skill :min-age 8
 :cost 100 :description "Sports skill +2..3, health +"
 :handler
 (lambda (char)
   (let ((d (+ 2 (random 2))))
     (emacslife-bump-skill char 'sports d)
     (emacslife-bump-stat char :health 2)
     (emacslife-log char (format "Sports league. Skill now %d."
                                  (emacslife-skill char 'sports)) :good))))

(emacslife-register-activity
 :id 'photography-walk :label "Photography walk" :category 'skill :min-age 10
 :description "Photography skill +1..3"
 :handler
 (lambda (char)
   (let ((d (+ 1 (random 3))))
     (emacslife-bump-skill char 'photography d)
     (emacslife-bump-stat char :happiness 2)
     (emacslife-log char (format "Photo walk. Skill now %d."
                                  (emacslife-skill char 'photography)) :good))))

(emacslife-register-activity
 :id 'garden :label "Tend the garden" :category 'skill :min-age 8
 :cost 30 :description "Gardening +1..3, happiness +"
 :handler
 (lambda (char)
   (let ((d (+ 1 (random 3))))
     (emacslife-bump-skill char 'gardening d)
     (emacslife-bump-stat char :happiness 2)
     (emacslife-log char (format "Gardening. Skill now %d."
                                  (emacslife-skill char 'gardening)) :good))))

(emacslife-register-activity
 :id 'dance-class :label "Dance class" :category 'skill :min-age 6
 :cost 50 :description "Dancing +2..4, looks +"
 :handler
 (lambda (char)
   (let ((d (+ 2 (random 3))))
     (emacslife-bump-skill char 'dancing d)
     (emacslife-bump-stat char :looks 1)
     (emacslife-log char (format "Dance class. Skill now %d."
                                  (emacslife-skill char 'dancing)) :good))))

(emacslife-register-activity
 :id 'learn-language :label "Take a language class" :category 'skill :min-age 8
 :cost 80 :description "Language +4..8 in chosen language"
 :handler
 (lambda (char)
   (let* ((lang-rows (mapcar (lambda (cell)
                               (cons (cdr cell) (car cell)))
                              emacslife-language-names))
          (lang (cdr (assoc (completing-read "Which language? "
                                              (mapcar #'car lang-rows) nil t)
                            lang-rows)))
          (d (+ 4 (random 5))))
     (emacslife-bump-language char lang d)
     (emacslife-bump-stat char :smarts 1)
     (emacslife-log char (format "Studied %s. Level now %d."
                                  (cdr (assq lang emacslife-language-names))
                                  (emacslife-language-level char lang))
                    :good))))

;;; -----------------------------------------------------------------
;;; Dispatch entry for the hub `Skills' tab

(defun emacslife-skills-menu ()
  "Top-level skills sub-menu."
  (interactive)
  (emacslife-pick-activity-and-run emacslife--state 'skill))

(emacslife-register-action :skills (lambda (_) (emacslife-skills-menu)))

(provide 'emacslife-skills)
;;; emacslife-skills.el ends here
