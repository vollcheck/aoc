(ns y22.day08
  (:require [clojure.set :as set]
            [core :refer [load-input parse-int]]
            [purity :refer [defpure]]))

(defn make-grid [in]
  (mapv #(mapv parse-int %) in))

(defn get-tree-value [grid x y]
  (-> grid
      (nth y)
      (nth x)))

(defn get-row-left [grid x y]
  (let [row (nth grid y)]
    (subvec row 0 x)))

(defn get-row-right [grid x y]
  (let [row (nth grid y)]
    (subvec row (inc x))))

(defn get-column-top [grid x y]
  (into [] (for [row (subvec grid 0 y)]
             (nth row x))))

(defn get-column-bot [grid x y]
  (into [] (for [row (subvec grid (inc y))]
             (nth row x))))

(defn larger-than? [n coll]
  (every? #(> n %) coll))

(def cross-lines-fn
  (juxt get-row-left
        get-row-right
        get-column-top
        get-column-bot))

(defn visible? [grid x y]
  (let [value (get-tree-value grid x y)
        cmp (partial larger-than? value)
        cross-lines (cross-lines-fn grid x y)]
    (some identity (map cmp cross-lines))))

(defn part-1 [in]
  (let [mtx (make-grid in)]
    (->> (for [y (range (count mtx))]
           (for [x (range (count (first mtx)))]
             (when (visible? mtx x y)
               [x y])))
         (map #(remove nil? %))
         (map #(set %))
         (apply set/union)
         (count))))


(defn directed-scenic-score [n coll]
  (min (inc (count (take-while (partial > n) coll)))
       (count coll)))

(defn scenic-score [grid x y]
  (let [value (get-tree-value grid x y)
        dss (partial directed-scenic-score value)

        [row-left row-right column-top column-bot]
        (cross-lines-fn grid x y)

        row-left (reverse row-left)
        column-top (reverse column-top)

        cross-lines [row-left row-right column-top column-bot]]
    (->> cross-lines
         (map dss)
         (reduce *))))

(defn part-2 [in]
  (let [mtx (make-grid in)]
    (->> (for [y (range (count mtx))]
           (for [x (range (count (first mtx)))]
             (scenic-score mtx x y)))
         (flatten)
         (apply max))))

(def example
  ["30373" "25512" "65332" "33549" "35390"])

;; TODO: lint it properly
(defpure part-2-pure
  {[example] 9} ;; 8
  "solves part 2 of day 8 of year 2022"
  [in]
  (let [mtx (make-grid in)]
    (->> (for [y (range (count mtx))]
           (for [x (range (count (first mtx)))]
             (scenic-score mtx x y)))
         (flatten)
         (apply max))))

(comment
  (def test-input (load-input :test)) ;; => ["30373" "25512" "65332" "33549" "35390"]
  (def input (load-input))
  (part-1 test-input) ;; => 21
  (part-1 input) ;; => 1705
  (part-2 test-input) ;; => 8
  (part-2 input) ;; => 371200
  (time (part-1 input)) ;; "Elapsed time: 128.27508 msecs"
  (time (part-2 input)) ;; "Elapsed time: 169.065716 msecs"
  )
