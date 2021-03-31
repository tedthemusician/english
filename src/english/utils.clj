(ns english.utils
  (:gen-class))

(defn map-vals
  "Map a function across the values of a hashmap"
  [f m]
  (reduce-kv (fn [acc k v] (update acc k f)) m m))
