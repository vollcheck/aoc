(ns day09
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (mapv
   #(Integer/parseUnsignedInt %)
   (str/split line #"")))

(defn load-input [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (mapv parse-line)))

(defn create-land
  "Creates a list of vectors of points."
  [board]
  (let [h (count board)
        w (count (first board))]
    (for [y (range h)]
      (mapv #(vec [% y]) (range w)))))

(defn g [b x y]
  (get (get b y) x))

(defn tube? [board x y]
  (let [point  (g board x y)
        up     (g board x (dec y))
        down   (g board x (inc y))
        left   (g board (dec x) y)
        right  (g board (inc x) y)
        points (filterv identity [point up down left right])]
    (and (= point
            (when points
              (apply min points)))
         (< point 9))))

(defn find-neighbors [board land]
  (let [width (count (first board))
        height (count board)]
    (partition 2 (flatten (mapv
     (fn [line]
       (filter
        (fn [[x y]]
          (tube? board x y))
        line))
     land)))))

(defn get-holes [board points]
  (map
   (fn [[x y]]
     (get (get board y) x))
   points))

(defn sum-tubes [tubes]
  (reduce
   (fn [cnt coll] (+ cnt (inc coll)))
   0 tubes))

(defn solve1 [filename]
  (let [board (load-input filename)
        land (create-land board)]
    (->> land
         (find-neighbors board)
         (get-holes board)
         (sum-tubes))))

(solve1 "input.txt")
