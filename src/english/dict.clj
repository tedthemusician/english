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


(def dict (parse-dict (slurp "resources/dict.txt")))

(defn pronounce
  "Get the phonemes of a word, case-insensitive"
  [w]
  (try (get dict (str/upper-case w))
       (catch Exception e (print (str "PROBLEM: " (.getMessage e))))))
