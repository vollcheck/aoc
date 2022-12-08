(ns day03
  (:require [clojure.string :as str]))


;; Loading

(defn load-input [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (map #(str/split % #""))
       (into [])))

(def input (load-input "input.txt"))
(def sample (load-input "sample.txt"))

;; Helper functions for processing.

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

(defn least-frequent [coll]
  "Takes a vector and return the most popular element."
  (->> coll
       (frequencies)
       (sort-by val)
       ((fn [[[x numx] [y numy]]]
          ;; (println x numx)
          ;; (println y numy)
          (if (= numx numy)
            "0"
            ;; here return the least number!!! todo:
            )))))

(least-frequent "0011")

;; First part (that requires refactor for sure...)

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
  (* (Long/parseLong (reduce str epsilon) 2)
     (Long/parseLong (reduce str gamma) 2)))


;; Second part

(def oxygen)
(def scrubber)

(defn mostf-on-idx [coll idx]
  "Most frequent element on specific column."
  (->> coll
       (mapv #(nth % idx))
       (most-frequent)))

(defn leastf-on-idx [coll idx]
  "Most frequent element on specific column."
  (->> coll
       (mapv #(nth % idx))
       (least-frequent)))

(defn processing [m]
  (loop [idx 0
         res m]
    (if (= idx (count (first m)))
      (Long/parseLong (reduce str (first res)) 2)
      (let [most-popular (mostf-on-idx res idx)]
        (recur (inc idx)
               (filter
                (fn [row] (= most-popular (nth row idx)))
                res))))))


(defn processing2 [m]
  (loop [idx 0
         res m]
    (if (= idx (count (first m)))
      (reduce str (first res))
      ;; (Long/parseLong (reduce str (first res)) 2)
      (let [least-popular (leastf-on-idx res idx)]
        (recur (inc idx)
               (filter
                (fn [row] (= least-popular (nth row idx)))
                res))))))

(mf-idx sample 4)
(leastf-on-idx sample 4)
(processing sample)
(processing2 sample)
