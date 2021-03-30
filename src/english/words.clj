(ns english.words
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [english.dict :as dict])
  (:gen-class))

(def separators
  ["-" ; hyphen
   "—" ; en dash
   "–" ; em dash
   "/"
   ":"
   "\\."
   "\\|"
   "\""])

(def separators-re (re-pattern (str/join "|" separators)))

(defn separate
  "Replace separating characters with spaces, then split the resulting words."
  [s]
  (str/split (str/replace s separators-re " ") #"\s+"))

(defn strip-non-alphanumeric
  "Strip all non-alphanumeric characters besides apostrophes from a word"
  [s]
  (str/replace s #"[^\w\d']" ""))

(defn verbalize-numbers
  "For now, just remove numbers"
  ; TODO: Convert numerals into words, then remove numerals from dict/dict
  [s]
  (if (number? (edn/read-string s))
    ""
    s))

(def just-word (comp verbalize-numbers strip-non-alphanumeric))

(defn just-words
  "Convert a string into words in which the only punctuation is an apostrophe"
  [s]
  (remove empty? (map just-word (separate s))))

(defn say
  "Phonemes for an entire string"
  [s]
  (mapcat dict/pronounce (just-words s)))

(def sentences
  ["Time stops for...no one... really"
   "It's not available--sorry for the inconvenience!"
   "We expect never-before-seen levels."
   "There are 525,600 minutes/year"])

