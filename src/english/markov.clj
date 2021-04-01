(ns english.markov
  (:require [clojure.pprint :refer [pprint]]
            [english.utils :refer [map-vals]])
  (:gen-class))

(defn succs
  "Get every succession in the sequence, including [nothing] to the first
  element and the last element to [nothing]"
  [coll]
  (let [firsts (concat [nil] coll)
        nexts (concat coll [nil])]
    (map vector firsts nexts)))

(defn succ-freqs
  "Create a map of elements to maps of their successors and the frequencies of
  each succession"
  [coll]
  (let [succs (frequencies (succs coll))]
    (reduce-kv
      (fn [conns [elem succ] freq]
        (if (conns elem)
          (update conns elem #(assoc % succ freq))
          (assoc conns elem (sorted-map succ freq))))
      (sorted-map)
      succs)))

(defn normalize-vals
  "Convert raw counts to proportions"
  [m]
  (let [num-succs (reduce + (vals m))]
    (map-vals #(/ % num-succs) m)))

(defn normalize-freqs
  "Convert all successor frequencies to proportions"
  [conns]
  (map-vals normalize-vals conns))

(defn weighted-ranges
  "Get weighted ranges for a series of proportions such that random numbers
  between 0 and 1 will be in each of the ranges with the appropriate frequency.
  The ranges are represnted as upper limits in ascending order."
  [proportions]
  (if (= 1 (count proportions))
    [1]
    (concat (reductions
              (fn [acc curr] (+ (or acc 0) curr))
              (or (butlast proportions) []))
            [1])))

(defn range-vals
  "Get weighted ranges for an element's succesors"
  [freqs]
  (let [ranges (weighted-ranges (vals freqs))]
    (zipmap ranges (keys freqs))))

(defn range-succs
  "Give each successor a maximum for a range from its previous successors
  maximum (or 0)"
  [conns]
  (map-vals range-vals conns))

(def ranged-succs (comp range-succs normalize-freqs succ-freqs))

(defn next-elem
  "The next element in a Markov chain"
  [freqs elem]
  (let [succs (freqs elem)
        rand-num (rand)
        [[weight match] & _] (drop-while #(< (first %) rand-num) succs)]
    match))

(defn generate-next
  "Generate a Markov chain after an initial element"
  [freqs initial]
  (cons initial (lazy-seq (generate-next freqs (next-elem freqs initial)))))

(defn generate
  "Generate a sequence of n elements based on a Markov process specified by
  freqs."
  [freqs n]
  (take n (generate-next freqs (next-elem freqs nil))))

(defn imitate
  "Imitate a collection for n elements"
  [coll n]
  (generate (ranged-succs coll) n))
