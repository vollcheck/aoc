(ns y21.day05
  (:require [clojure.string :as str]
            [core :refer [make-grid parse-uint transpose]]))

(defn load-input []
  (-> "src/y21/input05.txt" ;;"src/y21/input05-test.txt"
      slurp
      str/split-lines))

(defn parse-line
  [line]
  (let [[p1 p2] (str/split line #" -> ")
        [x1 y1] (map parse-uint (str/split p1 #","))
        [x2 y2] (map parse-uint (str/split p2 #","))]
    [[x1 y1] [x2 y2]]))

(defn inc-line-horizontal [grid x1 x2 y]
  (let [row (nth grid y)
        [start end] (sort [x1 x2])
        end (inc end)
        updated-row (vec (apply concat
                                [(subvec row 0 start)
                                 (mapv inc (subvec row start end))
                                 (subvec row end)]))]
    (assoc grid y updated-row)))

(defn inc-line-vertical [grid y1 y2 x]
  (for [row grid
        idx (range (count grid))
        :let [[start end] (sort [y1 y2])]]
    (if (<= start idx end)
      (update row x inc)
      row)))

(defn apply-line [grid line]
  ;; NOTE: see notes on https://clojuredocs.org/clojure.core/subvec
  (let [[[x1 y1] [x2 y2]] (parse-line line)]
    (cond
      ;; horizontal
      (= y1 y2) (inc-line-horizontal grid x1 x2 y1)

      ;; vertical
      (= x1 x2) (inc-line-vertical grid y1 y2 x1)

      ;; NOTE: horizontal and vertical lines only for now
      :else grid)))

(defn count-2s [grid]
  (count
   (map
    #(filter (partial < 1) %)
    grid)))

(defn part-1 [lines]
  (->> lines
       (reduce apply-line (make-grid 1000))
       (count-2s)
       ))

(comment
  (def lines (load-input))
  (prn (part-1 lines))

  (apply-line (make-grid 3) [[0 1] [2 1]])
  (def grid (make-grid 1000))
  (def input (load-input))
  (def line (first input))

  ;; NOTE: Should not transpose really
  ;; "Elapsed time: 0.173911 msecs"
  (time (do (make-grid 1000) 1))
  ;; "Elapsed time: 86.442644 msecs"
  (time (do (transpose (make-grid 1000)) 1))
  ;; "Elapsed time: 199.117358 msecs"
  (time (do (transpose (transpose (make-grid 1000))) 1))

  line ;; => "0,9 -> 5,9"
  (str/split line #" -> ") ;; => ["0,9" "5,9"]
  (let [[p1 p2] (str/split line #" -> ")
        [x1 y1] (map parse-uint (str/split p1 #","))
        [x2 y2] (map parse-uint (str/split p2 #","))]
    [[x1 y1] [x2 y2]])

  (vec (apply concat [[1 2] [3 4] [2 5]]))
  (let [v1 (into [] [2 5])
        v2 (into [] [5 3])]
    (into v1 v2))


  (sort [5 4])

  )
