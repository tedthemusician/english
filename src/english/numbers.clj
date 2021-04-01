(ns english.numbers
  (:require [clojure.string :as str])
  (:gen-class))

(def numerals
  ["ZERO"
   "ONE"
   "TWO"
   "THREE"
   "FOUR"
   "FIVE"
   "SIX"
   "SEVEN"
   "EIGHT"
   "NINE"])

(def ten-through-nineteen
  ["TEN"
   "ELEVEN"
   "TWELVE"
   "THIRTEEN"
   "FOURTEEN"
   "FIFTEEN"
   "SIXTEEN"
   "SEVENTEEN"
   "EIGHTEEN"
   "NINETEEN"])

(def tens-prefixes
  [nil
   nil
   "TWENTY"
   "THIRTY"
   "FORTY"
   "FIFTY"
   "SIXTY"
   "SEVENTY"
   "EIGHTY"
   "NINETY"])

(def thousand-suffixes
  [nil
   "THOUSAND"
   "MILLION"
   "BILLION"
   "TRILLION"
   "QUADRILLION"
   "QUINTILLION"
   "SEXTILLION"
   "SEPTILLION"
   "OCTILLION"])

(def zero-code (int \0))

(defn get-digits
  "Get the digits of a number"
  [s]
  (map #(- (int %) zero-code) (seq s)))

(defn whole-number-groups
  "Get groups of whole numbers in powers of 1,000. Each group is in the order
  (ones, tens, hundreds), and the whole collection is in the order
  (hundreds, thousands, millions...)"
  [digits]
  (partition-all 3 (reverse digits)))

(defn verbalize-group
  "Verbalize a group of digits in the order (ones, tens, hundreds)"
  [digits]
  (let [[ones tens hundreds] digits]
    (cond
      ; 1-digit number
      (nil? tens)
      [(numerals ones)]

      ; 2-digit number
      (nil? hundreds)
      (cond
        ; Number between 10 and 19 inclusive
        (= 1 tens)
        (ten-through-nineteen ones)
        
        ; Multiple of 10 greater than 19
        (zero? ones)
        (tens-prefixes tens)

        ; 2-digit number greater than 19, not a multiple of 10
        :else [(tens-prefixes tens) (numerals ones)])

      ; Multiple of 100
      (and (zero? ones) (zero? tens))
      [(numerals hundreds) "HUNDRED"]

      ; 3-digit number greater than 99, not a multiple of 100
      :else (flatten [[(numerals hundreds) "HUNDRED"]
                      (verbalize-group [ones (if (zero? tens) nil tens)])]))))

(defn add-suffix-unsafe
  "Add a power-of-1000 suffix to a group if necessary"
  [index group]
  (if (zero? index)
    group
    (conj (vec group) (thousand-suffixes index))))

(defn add-suffixes-unsafe
  "Add suffixes to whole number groups of powers of 1,000"
  [groups]
  (flatten (reverse (map-indexed add-suffix-unsafe groups))))

(defn add-suffixes-safe
  "Add suffixes to whole number groups of powers of 1,000, or return nil if
  the number is more than 999,999,999,999,999,999,999,999,999,999"
  [groups]
  (if (> (count groups) 10)
    nil
    (add-suffixes-unsafe groups)))

(defn verbalize-whole-number
  "Verbalize a whole number, disregarding any character that isn't a digit"
  [s]
  (let [digits (get-digits (str/replace s #"\D" ""))
        groups (map verbalize-group (whole-number-groups digits))]
    (add-suffixes-safe groups)))

(defn verbalize-decimal-component
  "Convert a string of numerals to their word counterparts"
  [s]
  (map #(nth numerals %) (get-digits s)))

(defn verbalize
  "Verbalize both the whole and decimal parts of a number. If there is no
  decimal part, just verbalize the whole part."
  [s]
  (let [[whole decimal] (str/split s #"\.")
        whole-words (verbalize-whole-number whole)]
    (if (nil? decimal)
      whole-words
      (flatten [whole-words "POINT" (verbalize-decimal-component decimal)]))))
