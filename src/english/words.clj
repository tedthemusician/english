(ns english.words
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [english.dict :as dict]
            [english.numbers :as numbers])
  (:gen-class))

(def separators
  ["-" ; hyphen
   "—" ; en dash
   "–" ; em dash
   "/"
   ":"
   "\\|"
   "\\.{2,}+"
   "\""])

(def separators-re (re-pattern (str/join "|" separators)))

(def number-re #"^\d[\d,]*(\.\d+)?$")

(defn separate
  "Replace separating characters with spaces, then split the resulting words."
  [s]
  (str/split (str/replace s separators-re " ") #"\s+"))

(defn strip-non-alphanumeric
  "Strip all non-alphanumeric characters besides apostrophes from a word"
  [s]
  (if (re-find number-re s)
    (numbers/verbalize s)
    (str/replace s #"[^\w\d']" "")))

(defn just-words
  "Convert a string into words in which the only punctuation is an apostrophe"
  [s]
  (flatten (remove empty? (map strip-non-alphanumeric (separate s)))))

(defn say
  "Phonemes for an entire string"
  [s]
  (mapcat dict/pronounce (just-words s)))
