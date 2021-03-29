(ns english.dict
  (:require [clojure.string :as str])
  (:gen-class))

; TODO: Add numerals to final dictionary

(def primary-def-re #"^[A-Z][^()\s]*  \S")
; (def primary-def-re #"^[A-Z]  \S")

(defn parse-word
  "Get:
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

(def dict
  (reduce-kv
    (fn [acc k v] (assoc acc k (original-dict v)))
    original-dict
    numerals))

(dict 1)
