(ns y24.day08
  (:require [core :refer [load-in]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(defn get2 [grid x y]
  (-> (nth grid y)
      (nth x)))

(defn find-all-objects
  "Looks for all objects on the grid and group them by object value."
  [grid]
  (let [max-y (count grid)
        max-x (count (first grid))]
    (->> (for [y (range max-y)
               x (range max-x)
               :let [c (get2 grid x y)]
               :when (not (= \. c))]
           [c x y])
         (reduce
          (fn [acc object]
            (update acc (first object) conj (rest object)))
          {}))))

(comment
  (find-all-objects [[\# \. \(]])
   ;; => {1 [[1 0 0]], 2 [[2 2 0]]}
  )

(defn mirrored
  "For x and y in grid find the mirrored object to middle point "
  [grid [x y] [bx by]]
  (let [maxx (count (first grid))
        maxy (count grid)
        newx (- (* 2 bx) x)
        newy (- (* 2 by) y)]
    (when (and (< -1 newx maxx)
               (< -1 newy maxy))
      [newx newy])))

(comment
  (mirrored-long ["...."
                  "...."
                  ".A.."
                  "A..."]
                 [0 3] [1 2])
  ;; => ([2 1] [3 3])
  )

(defn combo-group [grid group]
  (let [pairs (combo/combinations group 2)
        f (partial mirrored grid)]
    (reduce
     (fn [acc [p1 p2]]
       (let [r1 (f p1 p2)
             r2 (f p2 p1)]
         (cond-> acc
           r1 (conj r1)
           r2 (conj r2))))
     []
     pairs)))

(defn combo-all-groups [grid objs]
  (reduce
   (fn [acc [_sign group]]
     (apply conj acc (combo-group grid group)))
   []
   objs))

(defn part-1 [in]
  (->> (find-all-objects in)
       (combo-all-groups in)
       set
       count
       ))

(defn range-from
  "Same as `range` but with dictated start and infinite end"
  [from]
  (iterate inc' from))

(defn mirrored-long
  "For x and y in grid find the mirrored object to middle point "
  [grid [x y] [bx by]]
  (let [maxx (count (first grid))
        maxy (count grid)
        basex (- (* 2 bx) x)
        basey (- (* 2 by) y)
        stepx (- bx x)
        stepy (- by y)]
    (for [factor (range)
          :let [newx (+ basex (* factor stepx))
                newy (+ basey (* factor stepy))]
          :while (and (< -1 newx maxx)
                      (< -1 newy maxy))]
      [newx newy])))

(comment
  (def g ["...."
          "...."
          ".A.."
          "A..."])
  (->> (mirrored-long g [0 3] [1 2])
       (debug-points g))
  ["...#"
   "..#."
   ".A.."
   "A..."]
  ;; => ([2 1] [3 0])
  ;; => ([2 1] [3 2])
  ;; => ([2 1] [3 2])
  )

;; NOTE: just for debugging purposes, might be a part of core namespace
(defn assoc-in-vec-str [grid x y v]
  (let [row (into [] (nth grid y))
        newrow (apply str (assoc row x v))]
    (assoc grid y newrow)))

(defn debug-points [grid points]
  (reduce
   (fn [g [x y]]
     (if (and x y)
       (assoc-in-vec-str g x y \#)
       g))
   grid
   points))

(defn combo-all-groups-2 [grid objs]
  (let [apc (partial apply conj)]
    (reduce
     (fn [acc [_sign group]]
       (let [pairs (combo/combinations group 2)
             f (partial mirrored-long grid)
             result-pairs (reduce
                           (fn [acc [p1 p2]]
                             (let [r1 (f p1 p2)
                                   r2 (f p2 p1)]
                               (cond-> acc
                                 r1 (apc r1)
                                 r2 (apc r2))))
                           ;; put all objects from group as an initial value
                           ;; they cound as antinodes as well!
                           (into [] group)
                           pairs)]
         (apc acc result-pairs)))
     []
     objs)))

(defn part-2 [in]
  (->> (find-all-objects in)
       (combo-all-groups-2 in)
       set
       count))

(comment
  (def intest (load-in :test))
  (def in (load-in))

  (part-1 intest)
  ;; => 14
  (part-1 in)
  ;; => 381

  (part-2 intest)
  ;; => 34
  (part-2 in)
  ;; => 1184
  )
