(ns english.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn map-vals
  "Map a function across the values of a hashmap"
  [f m]
  (reduce-kv (fn [acc k v] (update acc k f)) m m))

(defn load-dir-files
  [dir]
  (let [sources (next (file-seq (io/file dir)))]
    (map slurp sources)))

(def load-dir-contents (comp (partial str/join " ") load-dir-files))
