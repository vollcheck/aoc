(ns day01
  (:require [clojure.string :as str])))

;; Input loading, probably will use that much
(def filename "input.txt")

(defn get-input []
  (->> filename
       (slurp)
       (str/split-lines)
       (map #(Integer/parseUnsignedInt %))
       (into [])))

(def input (get-input))

;; First solution.
(defn compare-measurement [coll]
  (reduce
   (fn [cnt [x y]]
     (if (< x y) (inc cnt) cnt))
   0 (partition 2 1 input)))

;; Second solution.
(defn compare-measurement3 [coll]
  (reduce
   (fn [cnt [a b c d]]
     (if (< (+ a b c) (+ b c d))
       (inc cnt) cnt))
   0 (partition 4 1 coll)))
