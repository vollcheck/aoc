(ns y22.day03
  (:require [clojure.string :as str]
            [clojure.set :as cset]
            [core :refer [load-input]]))

(declare part-1 part-2)

(def s "vJrwpWtwJgWrhcsFMMfFFhFp")
(count s)
(count (first (split-at (/ (count s) 2) s)))

(- 27 (int \A)) ;; => -38
(- (int \A) 38)
(int \L)
(- (int \a) 96)
(- (int \z) 96)

(defn value-letter [letter]
  (if (Character/isUpperCase letter)
    (- (int letter) 38)
    (- (int letter) 96)))


(defn process-compartment [s]
  (let [len (count s)
        [f s] (split-at (/ len 2) s)
        [f s] (map set [f s])]
    (-> (cset/intersection f s)
        first
        value-letter)))

(defn part-1 [input]
  (->> (map process-compartment input)
       (reduce + 0)))

(defn proc-2 [coll]
  (->> coll
       (map set)
       (apply cset/intersection)
       first
       value-letter))

(defn part-2 [input]
  (->> input
       (partition 3)
       (map proc-2)
       (reduce + 0)))

(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input)
  (part-1 input) ;; => 8243
  (part-2 input) ;; => 2631
  )
