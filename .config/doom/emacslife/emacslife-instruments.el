;;; emacslife-instruments.el --- EmacsLife: named tickers + market sim -*- lexical-binding: t; -*-
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
;; Catalogue of investable instruments (stocks, ETFs, crypto, bonds)
;; with yearly mark-to-market simulation.  Market regimes (bull /
;; normal / bear / crash) are rolled once per year and apply a common
;; drift on top of each instrument's individual volatility.

;;; Code:

(require 'cl-lib)
(require 'emacslife-core)
(require 'emacslife-character)

;;; -----------------------------------------------------------------
;;; Instrument registry

(defvar emacslife--instruments nil
  "Alist (SYMBOL . PLIST) of investable instruments.
Plist keys:
  :symbol string      ticker, e.g. \"AAPL\"
  :name string        display name
  :type symbol        one of: stock etf crypto bond rugpull
  :sector symbol      one of: tech finance health consumer energy ...
  :initial-price float
  :drift float        annual drift mean (e.g. 0.10 = +10% expected)
  :volatility float   annual std dev (e.g. 0.20 = 20%)
  :dividend-yield float
  :description string")

(cl-defun emacslife-register-instrument
    (&key symbol name type sector initial-price drift volatility dividend-yield description)
  (setf (alist-get symbol emacslife--instruments nil nil #'string=)
        (list :symbol symbol :name name :type type :sector sector
              :initial-price initial-price :drift drift :volatility volatility
              :dividend-yield (or dividend-yield 0.0)
              :description (or description "")))
  symbol)

(defun emacslife-instrument (symbol)
  "Return the instrument plist for SYMBOL, or nil."
  (alist-get symbol emacslife--instruments nil nil #'string=))

(defun emacslife-instruments-by-type (type)
  "Return all instruments of TYPE (symbol)."
  (cl-remove-if-not (lambda (cell) (eq (plist-get (cdr cell) :type) type))
                    emacslife--instruments))

;;; -----------------------------------------------------------------
;;; Stocks — 20 named tickers across sectors

(emacslife-register-instrument :symbol "AAPL" :name "Apple Inc."
  :type 'stock :sector 'tech
  :initial-price 230.0 :drift 0.12 :volatility 0.18 :dividend-yield 0.005
  :description "The iPhone company. People still buy them.")

(emacslife-register-instrument :symbol "MSFT" :name "Microsoft Corp."
  :type 'stock :sector 'tech
  :initial-price 420.0 :drift 0.13 :volatility 0.16 :dividend-yield 0.008
  :description "Cloud, AI, and Excel — the holy trinity.")

(emacslife-register-instrument :symbol "GOOG" :name "Alphabet Inc."
  :type 'stock :sector 'tech
  :initial-price 175.0 :drift 0.12 :volatility 0.19 :dividend-yield 0.004
  :description "Search ads, YouTube, and increasingly anxious about AI.")

(emacslife-register-instrument :symbol "NVDA" :name "NVIDIA Corp."
  :type 'stock :sector 'tech
  :initial-price 130.0 :drift 0.25 :volatility 0.40 :dividend-yield 0.0
  :description "Sells the AI gold rush its shovels.")

(emacslife-register-instrument :symbol "TSLA" :name "Tesla Inc."
  :type 'stock :sector 'consumer
  :initial-price 250.0 :drift 0.08 :volatility 0.50 :dividend-yield 0.0
  :description "Cars, sometimes. Also tweets.")

(emacslife-register-instrument :symbol "META" :name "Meta Platforms"
  :type 'stock :sector 'tech
  :initial-price 580.0 :drift 0.10 :volatility 0.30 :dividend-yield 0.005
  :description "Facebook for your parents, Instagram for everyone else.")

(emacslife-register-instrument :symbol "AMZN" :name "Amazon.com"
  :type 'stock :sector 'consumer
  :initial-price 185.0 :drift 0.11 :volatility 0.22 :dividend-yield 0.0
  :description "Cardboard boxes, the cloud, and groceries.")

(emacslife-register-instrument :symbol "NFLX" :name "Netflix Inc."
  :type 'stock :sector 'consumer
  :initial-price 700.0 :drift 0.10 :volatility 0.28 :dividend-yield 0.0
  :description "Raises prices, you keep watching.")

(emacslife-register-instrument :symbol "BRK.B" :name "Berkshire Hathaway B"
  :type 'stock :sector 'finance
  :initial-price 440.0 :drift 0.09 :volatility 0.13 :dividend-yield 0.0
  :description "Warren Buffett's grocery cart of America.")

(emacslife-register-instrument :symbol "JPM" :name "JPMorgan Chase"
  :type 'stock :sector 'finance
  :initial-price 200.0 :drift 0.08 :volatility 0.18 :dividend-yield 0.022
  :description "The bank's bank.")

(emacslife-register-instrument :symbol "JNJ" :name "Johnson & Johnson"
  :type 'stock :sector 'health
  :initial-price 155.0 :drift 0.06 :volatility 0.12 :dividend-yield 0.030
  :description "Band-aids, baby powder, and lawsuits.")

(emacslife-register-instrument :symbol "V" :name "Visa Inc."
  :type 'stock :sector 'finance
  :initial-price 290.0 :drift 0.11 :volatility 0.15 :dividend-yield 0.008
  :description "Gets a cut of everything you buy.")

(emacslife-register-instrument :symbol "WMT" :name "Walmart Inc."
  :type 'stock :sector 'consumer
  :initial-price 90.0 :drift 0.07 :volatility 0.12 :dividend-yield 0.015
  :description "Sells everything cheaper.")

(emacslife-register-instrument :symbol "DIS" :name "Walt Disney Co."
  :type 'stock :sector 'consumer
  :initial-price 90.0 :drift 0.05 :volatility 0.22 :dividend-yield 0.008
  :description "Theme parks and streaming wars.")

(emacslife-register-instrument :symbol "KO" :name "Coca-Cola Co."
  :type 'stock :sector 'consumer
  :initial-price 60.0 :drift 0.05 :volatility 0.10 :dividend-yield 0.030
  :description "Brown sugar water. Reliable.")

(emacslife-register-instrument :symbol "XOM" :name "Exxon Mobil"
  :type 'stock :sector 'energy
  :initial-price 110.0 :drift 0.05 :volatility 0.25 :dividend-yield 0.035
  :description "Dinosaur juice corporation.")

(emacslife-register-instrument :symbol "PFE" :name "Pfizer Inc."
  :type 'stock :sector 'health
  :initial-price 28.0 :drift 0.04 :volatility 0.18 :dividend-yield 0.060
  :description "Big pharma, biggest dividends.")

(emacslife-register-instrument :symbol "GME" :name "GameStop Corp."
  :type 'stock :sector 'consumer
  :initial-price 25.0 :drift 0.0 :volatility 0.80 :dividend-yield 0.0
  :description "Meme stock. Buckle up.")

(emacslife-register-instrument :symbol "F" :name "Ford Motor"
  :type 'stock :sector 'consumer
  :initial-price 11.0 :drift 0.04 :volatility 0.22 :dividend-yield 0.055
  :description "Pickup trucks forever.")

(emacslife-register-instrument :symbol "BA" :name "Boeing Co."
  :type 'stock :sector 'aerospace
  :initial-price 175.0 :drift 0.04 :volatility 0.30 :dividend-yield 0.0
  :description "Planes. Sometimes they stay together.")

;;; -----------------------------------------------------------------
;;; ETFs — diversified low-cost options

(emacslife-register-instrument :symbol "SPY" :name "SPDR S&P 500 ETF"
  :type 'etf :sector 'index
  :initial-price 580.0 :drift 0.10 :volatility 0.16 :dividend-yield 0.014
  :description "The S&P 500. Boring. Effective.")

(emacslife-register-instrument :symbol "QQQ" :name "Invesco QQQ (Nasdaq-100)"
  :type 'etf :sector 'index
  :initial-price 500.0 :drift 0.12 :volatility 0.22 :dividend-yield 0.006
  :description "Nasdaq-100 — heavy on tech.")

(emacslife-register-instrument :symbol "VOO" :name "Vanguard S&P 500"
  :type 'etf :sector 'index
  :initial-price 530.0 :drift 0.10 :volatility 0.16 :dividend-yield 0.014
  :description "Cheaper SPY.")

(emacslife-register-instrument :symbol "VTI" :name "Vanguard Total Stock Market"
  :type 'etf :sector 'index
  :initial-price 290.0 :drift 0.10 :volatility 0.17 :dividend-yield 0.014
  :description "Whole U.S. stock market in one ticker.")

(emacslife-register-instrument :symbol "VXUS" :name "Vanguard Total International"
  :type 'etf :sector 'index
  :initial-price 65.0 :drift 0.07 :volatility 0.18 :dividend-yield 0.030
  :description "Everything not American.")

(emacslife-register-instrument :symbol "ARKK" :name "ARK Innovation ETF"
  :type 'etf :sector 'thematic
  :initial-price 55.0 :drift 0.08 :volatility 0.45 :dividend-yield 0.0
  :description "Cathie Wood's vision board.")

(emacslife-register-instrument :symbol "BND" :name "Vanguard Total Bond"
  :type 'etf :sector 'bonds
  :initial-price 73.0 :drift 0.03 :volatility 0.06 :dividend-yield 0.040
  :description "Bond ETF.  Slow and steady.")

;;; -----------------------------------------------------------------
;;; Crypto — the fun corner

(emacslife-register-instrument :symbol "BTC" :name "Bitcoin"
  :type 'crypto :sector 'crypto
  :initial-price 96000.0 :drift 0.20 :volatility 0.55 :dividend-yield 0.0
  :description "Digital gold. Or a tulip. Hard to tell.")

(emacslife-register-instrument :symbol "ETH" :name "Ethereum"
  :type 'crypto :sector 'crypto
  :initial-price 3500.0 :drift 0.18 :volatility 0.65 :dividend-yield 0.04
  :description "Smart contracts and gas fees.")

(emacslife-register-instrument :symbol "SOL" :name "Solana"
  :type 'crypto :sector 'crypto
  :initial-price 200.0 :drift 0.15 :volatility 0.80 :dividend-yield 0.05
  :description "Fast.  Sometimes online.")

(emacslife-register-instrument :symbol "DOGE" :name "Dogecoin"
  :type 'crypto :sector 'crypto
  :initial-price 0.40 :drift 0.05 :volatility 1.20 :dividend-yield 0.0
  :description "Much wow.  Very risk.")

(emacslife-register-instrument :symbol "SHIB" :name "Shiba Inu"
  :type 'crypto :sector 'crypto
  :initial-price 0.000025 :drift 0.0 :volatility 1.50 :dividend-yield 0.0
  :description "It's a meme of a meme.")

(emacslife-register-instrument :symbol "RUGCOIN" :name "RugCoin"
  :type 'rugpull :sector 'crypto
  :initial-price 1.00 :drift -0.50 :volatility 1.80 :dividend-yield 0.0
  :description "It's literally called RugCoin. What are you doing.")

(emacslife-register-instrument :symbol "MOONROCK" :name "MoonRock Token"
  :type 'rugpull :sector 'crypto
  :initial-price 0.10 :drift -0.30 :volatility 2.00 :dividend-yield 0.0
  :description "Promises 1000x. Delivers 0.01x.")

(emacslife-register-instrument :symbol "SCMTKN" :name "ScamToken"
  :type 'rugpull :sector 'crypto
  :initial-price 0.50 :drift -0.40 :volatility 1.50 :dividend-yield 0.0
  :description "The whitepaper is two paragraphs and a clip-art rocket.")

;;; -----------------------------------------------------------------
;;; Bonds (treated as price-stable yielding instruments)

(emacslife-register-instrument :symbol "TBILL" :name "10-Year Treasury Bond"
  :type 'bond :sector 'bonds
  :initial-price 100.0 :drift 0.0 :volatility 0.04 :dividend-yield 0.042
  :description "Uncle Sam owes you 4.2% per year.")

(emacslife-register-instrument :symbol "CORP" :name "AAA Corporate Bond"
  :type 'bond :sector 'bonds
  :initial-price 100.0 :drift 0.0 :volatility 0.06 :dividend-yield 0.055
  :description "Big stable companies, 5.5% yield.")

(emacslife-register-instrument :symbol "JUNK" :name "High-Yield 'Junk' Bond"
  :type 'bond :sector 'bonds
  :initial-price 100.0 :drift 0.0 :volatility 0.18 :dividend-yield 0.085
  :description "These companies maybe don't pay you back.")

(emacslife-register-instrument :symbol "MUNI" :name "Municipal Bond"
  :type 'bond :sector 'bonds
  :initial-price 100.0 :drift 0.0 :volatility 0.05 :dividend-yield 0.035
  :description "Tax-free yield from your city.")

;;; -----------------------------------------------------------------
;;; REITs — Real Estate Investment Trusts (high-yield distributions)
;;;
;;; By law, REITs must distribute 90%+ of taxable income to shareholders,
;;; so their yields are structurally much higher than typical equities.

(emacslife-register-instrument :symbol "VNQ" :name "Vanguard Real Estate ETF"
  :type 'reit :sector 'real-estate
  :initial-price 92.0 :drift 0.06 :volatility 0.18 :dividend-yield 0.042
  :description "Diversified U.S. REIT ETF.  Quarterly distributions.")

(emacslife-register-instrument :symbol "O" :name "Realty Income Corp."
  :type 'reit :sector 'real-estate
  :initial-price 58.0 :drift 0.04 :volatility 0.16 :dividend-yield 0.055
  :description "\"The Monthly Dividend Company.\" Pays every month.")

(emacslife-register-instrument :symbol "PLD" :name "Prologis Inc."
  :type 'reit :sector 'real-estate
  :initial-price 115.0 :drift 0.07 :volatility 0.20 :dividend-yield 0.035
  :description "Industrial warehouses — owns Amazon's logistics.")

(emacslife-register-instrument :symbol "AMT" :name "American Tower"
  :type 'reit :sector 'real-estate
  :initial-price 200.0 :drift 0.06 :volatility 0.18 :dividend-yield 0.032
  :description "Cell tower landlord. The 5G dividend.")

(emacslife-register-instrument :symbol "EQIX" :name "Equinix Inc."
  :type 'reit :sector 'real-estate
  :initial-price 850.0 :drift 0.08 :volatility 0.22 :dividend-yield 0.022
  :description "Data centers — the cloud's physical home.")

(emacslife-register-instrument :symbol "SCHH" :name "Schwab U.S. REIT ETF"
  :type 'reit :sector 'real-estate
  :initial-price 21.0 :drift 0.05 :volatility 0.17 :dividend-yield 0.038
  :description "Cheap REIT ETF.  Diversified, low-cost.")

(emacslife-register-instrument :symbol "MORT" :name "VanEck Mortgage REIT"
  :type 'reit :sector 'real-estate
  :initial-price 12.0 :drift 0.0 :volatility 0.28 :dividend-yield 0.095
  :description "Mortgage REITs — sky-high yield, scary volatility.")

;;; -----------------------------------------------------------------
;;; Market simulation
;;;
;;; Prices are stored on the CHAR in metadata under :market-prices —
;;; an alist (SYMBOL . CURRENT-PRICE).  We initialize lazily and
;;; update every year via `emacslife-process-market-year'.

(defun emacslife--market-prices (char)
  (plist-get (emacslife-character-metadata char) :market-prices))

(defun emacslife--set-market-prices (char prices)
  (let ((md (or (emacslife-character-metadata char) '())))
    (setf (emacslife-character-metadata char)
          (plist-put md :market-prices prices))))

(defun emacslife--current-price (char symbol)
  "Return current price for SYMBOL, initializing from instrument
default if not yet tracked for CHAR."
  (let ((prices (emacslife--market-prices char)))
    (or (alist-get symbol prices nil nil #'string=)
        (let ((init (plist-get (emacslife-instrument symbol) :initial-price)))
          (when init
            (setf (alist-get symbol prices nil nil #'string=) init)
            (emacslife--set-market-prices char prices)
            init)))))

(defun emacslife--market-regime ()
  "Roll the market regime for the year.
Returns a plist (:label STR :drift FLOAT :vol-mult FLOAT)."
  (let ((roll (random 100)))
    (cond
     ((< roll 5)   (list :label "MARKET CRASH"   :drift -0.40 :vol-mult 2.0))
     ((< roll 20)  (list :label "Bear market"    :drift -0.10 :vol-mult 1.3))
     ((< roll 80)  (list :label "Normal year"    :drift  0.00 :vol-mult 1.0))
     ((< roll 95)  (list :label "Bull market"    :drift  0.10 :vol-mult 0.9))
     (t            (list :label "EUPHORIC BULL"  :drift  0.25 :vol-mult 0.8)))))

(defun emacslife--gauss ()
  "Crude standard-normal-ish random (Box-Muller would be better but
this approximation is plenty for vibes-based simulation)."
  (let ((u (max 0.0001 (/ (1+ (random 9999)) 10000.0)))
        (v (max 0.0001 (/ (1+ (random 9999)) 10000.0))))
    (* (sqrt (* -2.0 (log u))) (cos (* 2.0 float-pi v)))))

(defun emacslife--lifetime-dividends (char)
  "Total dividends/distributions received over CHAR's lifetime."
  (or (plist-get (emacslife-character-metadata char) :lifetime-dividends) 0))

(defun emacslife--bump-lifetime-dividends (char delta)
  (let ((md (or (emacslife-character-metadata char) '())))
    (setf (emacslife-character-metadata char)
          (plist-put md :lifetime-dividends
                     (+ (or (plist-get md :lifetime-dividends) 0) delta)))))

(defun emacslife-position-annual-income (char pos)
  "Expected annual dividend/interest income from POS at current prices."
  (let* ((sym (plist-get pos :symbol))
         (instr (and sym (emacslife-instrument sym))))
    (if (not instr)
        0
      (round (* (or (plist-get pos :shares) 0)
                (emacslife--current-price char sym)
                (or (plist-get instr :dividend-yield) 0.0))))))

(defun emacslife-portfolio-annual-income (char)
  "Expected total annual income from all yielding positions."
  (apply #'+ (mapcar (lambda (p) (emacslife-position-annual-income char p))
                     (emacslife-character-investments char))))

(defun emacslife-process-market-year (char)
  "Advance market prices one year on CHAR.
Updates prices, pays per-position dividends/distributions/interest,
logs the market regime and a dividend breakdown."
  (let* ((regime (emacslife--market-regime))
         (regime-drift (plist-get regime :drift))
         (regime-vol-mult (plist-get regime :vol-mult))
         (prices (emacslife--market-prices char)))
    ;; ----- Update each instrument's price -----
    (dolist (cell emacslife--instruments)
      (let* ((sym (car cell))
             (i (cdr cell))
             (cur (or (alist-get sym prices nil nil #'string=)
                      (plist-get i :initial-price)))
             (drift (+ (plist-get i :drift) regime-drift))
             (vol (* (plist-get i :volatility) regime-vol-mult))
             (shock (* vol (emacslife--gauss)))
             (new (max 0.0001 (* cur (+ 1.0 drift shock)))))
        ;; Rugpulls have a small chance per year of going to zero
        (when (and (eq (plist-get i :type) 'rugpull)
                   (emacslife-roll 0.15))
          (setq new 0.0001))
        (setf (alist-get sym prices nil nil #'string=) new)))
    (emacslife--set-market-prices char prices)
    ;; ----- Pay yields -----
    (let ((div-total 0)
          (per-instr nil)     ; alist (TYPE-LABEL . amount)
          (positions (emacslife-character-investments char)))
      (dolist (pos positions)
        (let* ((sym (plist-get pos :symbol))
               (instr (and sym (emacslife-instrument sym))))
          (when instr
            (let* ((yield (or (plist-get instr :dividend-yield) 0.0))
                   (price (alist-get sym prices nil nil #'string=))
                   (shares (or (plist-get pos :shares) 0))
                   (pay (round (* shares price yield))))
              (when (> pay 0)
                (emacslife-bump-money char pay)
                (cl-incf div-total pay)
                ;; Categorize for the log: "dividends" vs "distributions"
                ;; vs "interest" depending on instrument type.
                (let* ((type (plist-get instr :type))
                       (label
                        (pcase type
                          ('bond  "interest")
                          ('reit  "distributions")
                          ('etf   "distributions")
                          ('crypto "staking rewards")
                          (_       "dividends")))
                       (existing (assoc label per-instr)))
                  (if existing
                      (setcdr existing (+ (cdr existing) pay))
                    (push (cons label pay) per-instr))))))))
      (when (> div-total 0)
        (emacslife--bump-lifetime-dividends char div-total)
        ;; Log a single line with the breakdown, e.g.
        ;; "Investment income: $4,250 (dividends $3,000, interest $1,250)"
        (let ((breakdown (mapconcat
                          (lambda (cell)
                            (format "%s %s"
                                    (car cell)
                                    (emacslife-format-money (cdr cell))))
                          per-instr ", ")))
          (emacslife-log char
                         (format "Investment income: %s (%s)."
                                 (emacslife-format-money div-total)
                                 breakdown)
                         :good))))
    ;; ----- Log market regime -----
    (emacslife-log char (format "%s." (plist-get regime :label))
                   (cond ((< regime-drift -0.05) :bad)
                         ((> regime-drift 0.05) :good)
                         (t :neutral)))))

(provide 'emacslife-instruments)
;;; emacslife-instruments.el ends here
