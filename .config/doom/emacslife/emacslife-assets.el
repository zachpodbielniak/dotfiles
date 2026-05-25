;;; emacslife-assets.el --- EmacsLife: bank, assets, investments, casino -*- lexical-binding: t; -*-
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
;; Banking (loans/credit/debt), purchasable assets (cars/houses with
;; mortgage and rental income), investments (stocks/crypto/RE), the
;; lottery, and casino minigames (blackjack, slots).

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)
(require 'emacslife-instruments)

;;; -----------------------------------------------------------------
;;; Asset definitions

(defconst emacslife-car-options
  '(("Used Toyota"      . 4000)
    ("Honda Civic"      . 22000)
    ("Subaru Outback"   . 32000)
    ("BMW M3"           . 75000)
    ("Tesla Model S"    . 95000)
    ("Lamborghini Urus" . 240000)
    ("Bugatti Chiron"   . 3000000)))

(defconst emacslife-house-options
  '(("Studio Apartment" . 120000)
    ("Townhouse"        . 250000)
    ("Suburban Home"    . 480000)
    ("Loft Condo"       . 700000)
    ("Beach House"      . 1500000)
    ("Mansion"          . 6500000)
    ("Private Island"   . 50000000)))

(defconst emacslife-jewelry-options
  '(("Gold Chain"   . 1200)
    ("Rolex Watch"  . 14000)
    ("Diamond Ring" . 35000)
    ("Crown Jewels" . 850000)))

;;; -----------------------------------------------------------------
;;; Bank actions

(defun emacslife-bank-menu ()
  "Banking sub-menu."
  (interactive)
  (emacslife-with-state char
    (let ((choice (completing-read "Bank: "
                                   '("Take out a loan"
                                     "Repay loan"
                                     "Declare bankruptcy"
                                     "Check credit score")
                                   nil t)))
      (pcase choice
        ("Take out a loan"
         (let* ((max-loan (max 1000 (* (emacslife-character-credit-score char) 50)))
                (amount (read-number (format "Loan amount (max $%d): " max-loan) 1000)))
           (when (and (> amount 0) (<= amount max-loan))
             (emacslife-bump-money char amount)
             (setf (emacslife-character-debt char)
                   (+ (emacslife-character-debt char) (round (* amount 1.15))))
             (emacslife-log char (format "Took out a $%d loan." amount) :neutral)
             (message "Money in."))))
        ("Repay loan"
         (let* ((debt (emacslife-character-debt char))
                (cash (emacslife-character-cash char))
                (pay (read-number (format "Repay how much? (debt $%d cash $%d): "
                                          debt cash) (min debt cash))))
           (when (and (> pay 0) (<= pay cash) (<= pay debt))
             (emacslife-bump-money char (- pay))
             (setf (emacslife-character-debt char) (- debt pay))
             (setf (emacslife-character-credit-score char)
                   (min 850 (+ (emacslife-character-credit-score char) 5)))
             (emacslife-log char (format "Repaid $%d in loans." pay) :good))))
        ("Declare bankruptcy"
         (when (yes-or-no-p "Are you sure?  Wipes debt but tanks credit. ")
           (setf (emacslife-character-debt char) 0)
           (setf (emacslife-character-credit-score char) 300)
           (emacslife-bump-stat char :happiness -10)
           (emacslife-log char "Declared bankruptcy.  Fresh start." :bad)))
        ("Check credit score"
         (message "Credit score: %d" (emacslife-character-credit-score char))))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

;;; -----------------------------------------------------------------
;;; Asset purchase

(defun emacslife-buy-asset (kind)
  "Buy an asset of KIND (\\='car \\='house \\='jewelry)."
  (interactive)
  (emacslife-with-state char
    (let* ((opts (pcase kind
                   ('car emacslife-car-options)
                   ('house emacslife-house-options)
                   ('jewelry emacslife-jewelry-options)
                   (_ (user-error "Unknown asset kind: %S" kind))))
           (rows (mapcar (lambda (cell)
                           (cons (format "%s — $%s" (car cell)
                                         (emacslife-format-money (cdr cell)))
                                 cell))
                         opts))
           (pick (cdr (assoc (completing-read (format "Buy %s: " kind)
                                              (mapcar #'car rows) nil t)
                             rows))))
      (when pick
        (let ((cost (cdr pick)))
          (when (< (emacslife-character-cash char) cost)
            (user-error "You can't afford it"))
          (emacslife-bump-money char (- cost))
          (push (list :kind kind :name (car pick)
                      :purchase-price cost
                      :purchase-year (+ (emacslife-character-birth-year char)
                                        (emacslife-character-age char))
                      :rental-income (if (eq kind 'house) 0 0))
                (emacslife-character-assets char))
          (emacslife-log char (format "Bought a %s for $%s."
                                      (car pick)
                                      (emacslife-format-money cost))
                         :good)
          (message "Acquired.")))
      (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh)))))

;;; -----------------------------------------------------------------
;;; Investments — position-based portfolio
;;;
;;; A position is a plist:
;;;   (:symbol "AAPL" :type 'stock :shares 10
;;;    :cost-basis 230.0 :purchased-year 2026)
;;;
;;; Legacy entries (pre-rewrite) had:
;;;   (:kind "Stocks" :amount 1000 :year 2026)
;;; Those still load and are treated as "lump sum legacy holdings".

(defun emacslife--position-value (char pos)
  "Current market value of a single position POS on CHAR."
  (cond
   ;; New format
   ((plist-get pos :symbol)
    (let* ((sym (plist-get pos :symbol))
           (px (emacslife--current-price char sym))
           (shares (plist-get pos :shares)))
      (round (* px shares))))
   ;; Legacy lump-sum
   ((plist-get pos :amount)
    (let* ((amt (plist-get pos :amount))
           (years (- (+ (emacslife-character-birth-year char)
                        (emacslife-character-age char))
                     (or (plist-get pos :year) 0))))
      ;; assume modest 6% drift
      (round (* amt (expt 1.06 (max 0 years))))))
   (t 0)))

(defun emacslife--position-cost (pos)
  "Original cost (basis × shares) of POS, or the legacy :amount."
  (cond
   ((plist-get pos :symbol)
    (round (* (plist-get pos :cost-basis) (plist-get pos :shares))))
   ((plist-get pos :amount) (plist-get pos :amount))
   (t 0)))

(defun emacslife-portfolio-value (char)
  "Total current market value of CHAR's portfolio."
  (apply #'+ (mapcar (lambda (p) (emacslife--position-value char p))
                     (emacslife-character-investments char))))

(defun emacslife-portfolio-cost (char)
  "Total cost basis of CHAR's portfolio."
  (apply #'+ (mapcar #'emacslife--position-cost
                     (emacslife-character-investments char))))

;;; ---------- Buy ----------

(defun emacslife--instrument-pickrows (instruments char)
  "Build completing-read rows for a list of INSTRUMENTS for CHAR.
Each row surfaces price AND yield % so the user can shop on income."
  (mapcar
   (lambda (cell)
     (let* ((sym (car cell))
            (i (cdr cell))
            (px (emacslife--current-price char sym))
            (yield (or (plist-get i :dividend-yield) 0.0))
            (yield-label
             (pcase (plist-get i :type)
               ('bond  "int")
               ('reit  "dist")
               ('etf   "dist")
               ('crypto "stake")
               (_       "div"))))
       (cons (format "%-8s  %-32s  $%-10s  %5s %.2f%%   %s"
                     sym
                     (plist-get i :name)
                     (format "%.2f" px)
                     yield-label
                     (* 100 yield)
                     (plist-get i :description))
             sym)))
   instruments))

(defun emacslife-invest-buy ()
  "Buy shares of an instrument."
  (interactive)
  (emacslife-with-state char
    (let* ((type-choice (completing-read
                         "Asset class: "
                         '("Stock" "ETF" "REIT (real estate)"
                           "Crypto" "Bond" "Rugpull (don't)")
                         nil t))
           (type (pcase type-choice
                   ("Stock" 'stock) ("ETF" 'etf)
                   ("REIT (real estate)" 'reit)
                   ("Crypto" 'crypto)
                   ("Bond" 'bond) (_ 'rugpull)))
           (insts (emacslife-instruments-by-type type))
           (_ (unless insts (user-error "No %s instruments registered" type)))
           (rows (emacslife--instrument-pickrows insts char))
           (sym (cdr (assoc (completing-read (format "Buy %s: " type-choice)
                                             (mapcar #'car rows) nil t)
                            rows)))
           (price (emacslife--current-price char sym))
           (max-shares (floor (/ (emacslife-character-cash char) price)))
           (_ (when (<= max-shares 0)
                (user-error "You can't afford even one share of %s ($%.2f)"
                            sym price)))
           (shares-str (read-string
                        (format "Shares of %s (max %d at $%.2f = $%s): "
                                sym max-shares price
                                (emacslife-format-money
                                 (* max-shares price)))
                        (number-to-string (min 10 max-shares))))
           (shares (string-to-number shares-str)))
      (when (and (> shares 0) (<= shares max-shares))
        (let ((cost (round (* shares price))))
          (emacslife-bump-money char (- cost))
          ;; Combine with existing position of same symbol (average cost basis)
          (let* ((existing (cl-find sym (emacslife-character-investments char)
                                    :test (lambda (s p)
                                            (equal s (plist-get p :symbol))))))
            (cond
             (existing
              (let* ((old-shares (plist-get existing :shares))
                     (old-basis (plist-get existing :cost-basis))
                     (new-shares (+ old-shares shares))
                     (new-basis (/ (+ (* old-shares old-basis)
                                       (* shares price))
                                    new-shares)))
                (setf (plist-get existing :shares) new-shares)
                (setf (plist-get existing :cost-basis) new-basis)))
             (t
              (push (list :symbol sym :type type :shares shares
                          :cost-basis price
                          :purchased-year (+ (emacslife-character-birth-year char)
                                              (emacslife-character-age char)))
                    (emacslife-character-investments char)))))
          (emacslife-log char (format "Bought %d %s @ $%.2f ($%s)."
                                      shares sym price
                                      (emacslife-format-money cost))
                         :neutral)
          (message "Bought %d %s." shares sym))))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; ---------- Sell ----------

(defun emacslife-invest-sell ()
  "Sell shares of a held position."
  (interactive)
  (emacslife-with-state char
    (let* ((positions
            (cl-remove-if-not (lambda (p) (plist-get p :symbol))
                              (emacslife-character-investments char)))
           (_ (unless positions
                (user-error "No tickered positions to sell")))
           (rows (mapcar
                  (lambda (p)
                    (let* ((sym (plist-get p :symbol))
                           (px (emacslife--current-price char sym))
                           (shares (plist-get p :shares))
                           (basis (plist-get p :cost-basis))
                           (pl (- (* px shares) (* basis shares)))
                           (pl-pct (* 100.0 (/ (- px basis) basis)))
                           (income (emacslife-position-annual-income char p)))
                      (cons (format "%-8s  %4d sh  @ $%-8s now $%-8s  P/L $%-10s (%s%.1f%%)  income/yr $%s"
                                    sym shares
                                    (format "%.2f" basis)
                                    (format "%.2f" px)
                                    (emacslife-format-money (round pl))
                                    (if (>= pl-pct 0) "+" "") pl-pct
                                    (emacslife-format-money income))
                            p)))
                  positions))
           (pos (cdr (assoc (completing-read "Sell which? "
                                              (mapcar #'car rows) nil t)
                            rows)))
           (sym (plist-get pos :symbol))
           (held (plist-get pos :shares))
           (sell-str (read-string (format "How many shares of %d? " held)
                                  (number-to-string held)))
           (sell (string-to-number sell-str)))
      (when (and (> sell 0) (<= sell held))
        (let* ((px (emacslife--current-price char sym))
               (proceeds (round (* sell px))))
          (emacslife-bump-money char proceeds)
          (cond
           ((= sell held)
            (setf (emacslife-character-investments char)
                  (cl-remove pos (emacslife-character-investments char))))
           (t
            (setf (plist-get pos :shares) (- held sell))))
          (emacslife-log char (format "Sold %d %s @ $%.2f ($%s)."
                                      sell sym px
                                      (emacslife-format-money proceeds))
                         :neutral)
          (message "Sold %d %s for $%s."
                   sell sym (emacslife-format-money proceeds)))))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

(defun emacslife-cash-out-investments ()
  "Sell ALL positions at current market price."
  (interactive)
  (emacslife-with-state char
    (let ((total 0))
      (dolist (pos (emacslife-character-investments char))
        (cl-incf total (emacslife--position-value char pos)))
      (emacslife-bump-money char total)
      (setf (emacslife-character-investments char) nil)
      (emacslife-log char (format "Sold entire portfolio: $%s."
                                  (emacslife-format-money total))
                     (if (> total 0) :good :neutral))
      (message "Cashed out everything for $%s."
               (emacslife-format-money total)))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; ---------- Quick view ----------

(defun emacslife-portfolio-summary ()
  "Show portfolio summary in the echo area."
  (interactive)
  (emacslife-with-state char
    (let ((cost (emacslife-portfolio-cost char))
          (value (emacslife-portfolio-value char))
          (income (emacslife-portfolio-annual-income char))
          (lifetime (emacslife--lifetime-dividends char)))
      (if (zerop cost)
          (message "No investments — buy some via 'Invest: buy'.")
        (message "Portfolio: cost $%s → value $%s (%s%.1f%% return) · annual income $%s · lifetime divs $%s"
                 (emacslife-format-money cost)
                 (emacslife-format-money value)
                 (if (>= value cost) "+" "")
                 (* 100.0 (/ (- (float value) cost) (max 1 cost)))
                 (emacslife-format-money income)
                 (emacslife-format-money lifetime))))))

;;; -----------------------------------------------------------------
;;; Lottery

(defun emacslife-buy-lottery-ticket ()
  "Buy a $5 ticket.  1-in-1000 chance of $5M jackpot."
  (interactive)
  (emacslife-with-state char
    (when (< (emacslife-character-cash char) 5)
      (user-error "Five dollars?  Really?"))
    (emacslife-bump-money char -5)
    (cond
     ((emacslife-roll 0.001)
      (let ((win 5000000))
        (emacslife-bump-money char win)
        (emacslife-bump-stat char :happiness 25)
        (emacslife-log char (format "JACKPOT — won $%s in the lottery!"
                                    (emacslife-format-money win))
                       :good)
        (push 'jackpot (emacslife-character-achievements char))
        (message "JACKPOT!")))
     ((emacslife-roll 0.05)
      (let ((win (+ 50 (random 500))))
        (emacslife-bump-money char win)
        (emacslife-log char (format "Won $%d in the lottery." win) :good)
        (message "Small win.")))
     (t (message "Better luck next year.")))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; -----------------------------------------------------------------
;;; Casino — slots

(defun emacslife-casino-slots ()
  "Pull the slot machine.  $20 per pull, occasional big wins."
  (interactive)
  (emacslife-with-state char
    (when (< (emacslife-character-cash char) 20)
      (user-error "Need at least $20"))
    (emacslife-bump-money char -20)
    (let* ((symbols ["🍒" "🍋" "🍊" "⭐" "💎" "7"])
           (s1 (aref symbols (random 6)))
           (s2 (aref symbols (random 6)))
           (s3 (aref symbols (random 6)))
           (win (cond
                 ((and (equal s1 s2) (equal s2 s3) (equal s1 "💎")) 10000)
                 ((and (equal s1 s2) (equal s2 s3) (equal s1 "7")) 5000)
                 ((and (equal s1 s2) (equal s2 s3)) 500)
                 ((or (equal s1 s2) (equal s2 s3)) 30)
                 (t 0))))
      (message "%s %s %s — %s" s1 s2 s3
               (if (> win 0)
                   (format "WIN $%d!" win)
                 "no luck"))
      (when (> win 0)
        (emacslife-bump-money char win)
        (emacslife-log char (format "Slots: %s %s %s — won $%d!"
                                    s1 s2 s3 win)
                       :good)))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; -----------------------------------------------------------------
;;; Casino — blackjack (simple)

(defun emacslife--bj-deal ()
  (1+ (random 13)))

(defun emacslife--bj-value (cards)
  (let* ((vals (mapcar (lambda (c)
                         (cond ((= c 1) 11)
                               ((>= c 11) 10)
                               (t c)))
                       cards))
         (sum (apply #'+ vals)))
    (while (and (> sum 21) (memq 11 vals))
      (let ((pos (cl-position 11 vals)))
        (setcar (nthcdr pos vals) 1))
      (setq sum (apply #'+ vals)))
    sum))

(defun emacslife-casino-blackjack ()
  "Play a single hand of blackjack."
  (interactive)
  (emacslife-with-state char
    (let* ((bet (read-number "Bet: " 100))
           (_ (when (< (emacslife-character-cash char) bet)
                (user-error "Not enough money")))
           (_ (emacslife-bump-money char (- bet)))
           (player (list (emacslife--bj-deal) (emacslife--bj-deal)))
           (dealer (list (emacslife--bj-deal) (emacslife--bj-deal))))
      ;; Hit/stand loop
      (while (and (< (emacslife--bj-value player) 21)
                  (yes-or-no-p (format "Your hand: %s (%d). Hit? "
                                       player (emacslife--bj-value player))))
        (push (emacslife--bj-deal) player))
      (while (< (emacslife--bj-value dealer) 17)
        (push (emacslife--bj-deal) dealer))
      (let ((pv (emacslife--bj-value player))
            (dv (emacslife--bj-value dealer)))
        (cond
         ((> pv 21)
          (message "Bust at %d.  Dealer wins." pv)
          (emacslife-log char (format "Blackjack: busted at %d. -$%d." pv bet) :bad))
         ((or (> dv 21) (> pv dv))
          (emacslife-bump-money char (* 2 bet))
          (message "You win $%d (you %d vs dealer %d)" bet pv dv)
          (emacslife-log char (format "Blackjack win: $%d." bet) :good))
         ((= pv dv)
          (emacslife-bump-money char bet)
          (message "Push at %d." pv))
         (t
          (message "Dealer wins (%d vs %d)." dv pv)
          (emacslife-log char (format "Blackjack loss: $%d." bet) :bad)))))
    (when (fboundp 'emacslife-ui-refresh) (emacslife-ui-refresh))))

;;; -----------------------------------------------------------------
;;; Net worth + yearly asset processing

(defun emacslife-net-worth (char)
  "Cash + asset value + portfolio market value − debt.
Uses `emacslife-portfolio-value' so position-based positions
(symbol/shares) value correctly alongside legacy lump-sum entries."
  (- (+ (emacslife-character-cash char)
        (apply #'+ (mapcar (lambda (a)
                             (or (plist-get a :purchase-price) 0))
                           (emacslife-character-assets char)))
        (emacslife-portfolio-value char))
     (emacslife-character-debt char)))

(defun emacslife-process-assets-year (char)
  "Apply rental income, asset depreciation, debt interest."
  (dolist (a (emacslife-character-assets char))
    (pcase (plist-get a :kind)
      ('house
       (let ((rent (or (plist-get a :rental-income) 0)))
         (when (> rent 0) (emacslife-bump-money char rent)))
       ;; appreciation
       (setf (plist-get a :purchase-price)
             (round (* (plist-get a :purchase-price) 1.03))))
      ('car
       (setf (plist-get a :purchase-price)
             (round (* (plist-get a :purchase-price) 0.92))))))
  ;; Debt interest
  (when (> (emacslife-character-debt char) 0)
    (setf (emacslife-character-debt char)
          (round (* (emacslife-character-debt char) 1.05))))
  ;; Living expenses scale with age and assets
  (let* ((expenses (+ 6000
                      (if (emacslife-character-spouse char) 4000 0)
                      (* 3000 (length (emacslife-character-children char))))))
    (emacslife-bump-money char (- expenses))))

;;; -----------------------------------------------------------------
;;; Assets sub-menu

(defun emacslife-assets-menu ()
  "Top-level assets menu."
  (interactive)
  (let ((choice (completing-read
                 "Assets: "
                 '("Bank"
                   "Buy a car" "Buy a house" "Buy jewelry"
                   "Invest: buy shares" "Invest: sell shares"
                   "Invest: portfolio summary" "Invest: cash out everything"
                   "Lottery ticket"
                   "Casino: blackjack" "Casino: slots")
                 nil t)))
    (pcase choice
      ("Bank" (emacslife-bank-menu))
      ("Buy a car" (emacslife-buy-asset 'car))
      ("Buy a house" (emacslife-buy-asset 'house))
      ("Buy jewelry" (emacslife-buy-asset 'jewelry))
      ("Invest: buy shares" (emacslife-invest-buy))
      ("Invest: sell shares" (emacslife-invest-sell))
      ("Invest: portfolio summary" (emacslife-portfolio-summary))
      ("Invest: cash out everything" (emacslife-cash-out-investments))
      ("Lottery ticket" (emacslife-buy-lottery-ticket))
      ("Casino: blackjack" (emacslife-casino-blackjack))
      ("Casino: slots" (emacslife-casino-slots)))))

(emacslife-register-action :assets (lambda (_form) (emacslife-assets-menu)))

(provide 'emacslife-assets)
;;; emacslife-assets.el ends here
