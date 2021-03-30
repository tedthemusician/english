(ns english.dict
  (:require [clojure.string :as str])
  (:gen-class))

(def primary-def-re #"^[A-Z][^()\s]*  \S")

(defn parse-word
  "Get as a pair:
    - The word being pronounced
    - Its component syllables as keywords"
  [s]
  (let [[word phoneme-str] (str/split s #"  ")
        phonemes-raw (str/split phoneme-str #" ")
        phonemes (map (comp keyword str/lower-case) phonemes-raw)]
    [word phonemes]))

(defn parse-dict
  "Parse all primary definitions in the dictionary"
  [d]
  (into {} (->> d
                str/split-lines
                (filter #(re-find primary-def-re %))
                (map parse-word))))


(def original-dict (parse-dict (slurp "resources/dict.txt")))

(def numerals ["ZERO"
               "ONE"
               "TWO"
               "THREE"
               "FOUR"
               "FIVE"
               "SIX"
               "SEVEN"
               "EIGHT"
               "NINE"])

; Add the numerals 0 through 9 to the dictionary, using the corresponding
; word's pronunciation
; TODO: Don't do this once words/verbalize-numbers is fleshed out
(def dict
  (reduce-kv
    (fn [acc k v] (assoc acc k (original-dict v)))
    original-dict
    numerals))

(defn pronounce
  "Get the phonemes of a word, case-insensitive"
  [w]
  (dict (str/upper-case w)))
