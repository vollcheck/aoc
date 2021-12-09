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

(defn tube? [board x y]
  (let [point  (get (get board y) x)
        up     (get (get board (dec y)) x)
        down   (get (get board (inc y)) x)
        left   (get (get board y) (dec x))
        right  (get (get board y) (inc x))
        points (filterv identity [point up down left right])]
    (and (= point (apply min points)) (< point 9))))

(defn find-neighbors [board]
  (let [width (count (first board))
        height (count board)]
    (for [y (range height)]
      (for [x (range width)]
        (if (tube? board x y)
          (swap! tubes conj (get (get board y) x)))))))

(defn sum-tubes [tubes]
  (reduce
   (fn [cnt coll]
     (+ cnt (inc coll)))
   0 @tubes))

(def tubes (atom []))

(find-neighbors (load-input "input.txt"))
@tubes
(println (sum-tubes tubes))
