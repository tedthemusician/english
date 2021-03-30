(ns english.show
  (:require [clojure.string :as str]
            [english.clusters :as clusters])
  (:gen-class))

; TODO:

; Separate phonemes into words of one, two, or three syllables that start and
; end with legal consonant clusters, diphthongs, single consonants, or
; monophthongs

; Split words into syllabes by legal consonant clusters

; Capitalize those syllables whose vowels end with 1 or 2

; Join syllables in each word by hyphens

; Split list of words into chunks of roughly equal size

; Join words in each chunk into sentences with spaces

; Join sentences into a paragraph with " ."


