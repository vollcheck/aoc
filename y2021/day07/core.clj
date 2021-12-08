(ns day07
  (:require [clojure.string :as str]))

(defn load-input [filename]
  ;; May I should use transducer one day.
  (->> filename
       (slurp)
       (str/trim)
       ((fn [data] (str/split data #",")))
       (map #(Integer/parseUnsignedInt %))
       (into [])))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn count-fuel [coll]
  (let [m (median coll)]
    (reduce
     (fn [counter number]
       (+ counter (Math/abs (- number m))))
     0 coll)))

(defn move [end]
  (/ (* end (inc end)) 2))


(defn fuel-cost [crab position]
  (move (Math/abs (- crab position))))

(defn count-fuel2 [coll]
  (apply min
         (for [position (range 1 (inc (apply max coll)))]
           (reduce + (map #(fuel-cost % position) coll)))))

(println (count-fuel (load-input "input.txt")))
(time (println (count-fuel2 (load-input "input.txt"))))
