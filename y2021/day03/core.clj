(ns day03
  (:require [clojure.string :as str]))

(defn load-input [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (map #(str/split % #""))
       (into [])))

(def input (load-input "input.txt"))
(def s-input (load-input "sample.txt"))

(defn transpose [m]
  (apply mapv vector m))

(defn most-frequent [coll]
  "Takes a vector and return the most popular element."
  (->> coll
       (frequencies)
       (sort-by val)
       (reverse)
       (first)
       (first)))

(defn get-gamma [coll]
  "Takes a matrix and return the most popular element in each row
and merges it into the string."
  (map most-frequent coll))

(defn get-epsilon [gamma]
  "Reverses bits."
  (map #(if (= % "1") "0" "1") gamma))

(def transposed (transpose input))
(def gamma (get-gamma transposed))
(def epsilon (get-epsilon gamma))

(defn task1 [gamma epsilon]
  (* (Long/parseLong (reduce str epsilon) 2) (Long/parseLong (reduce str gamma) 2)))

(comment
  (println (task1 gamma epsilon))
  )
