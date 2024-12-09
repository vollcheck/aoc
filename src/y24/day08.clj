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
               (< -1 newy maxy)
               ;; put it only if it's a free spot
               #_(= \. (get2 grid newx newy)))
      [newx newy])))

(comment
  (mirrored ["...."
             "...."
             ".A.."
             "A..."]
            [0 3] [1 2])
  ;; => [2 1]
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

;; NOTE: just for debugging purposes, might be a part of core namespace
(defn assoc-in-vec-str [grid x y v]
  (let [row (into [] (nth grid y))
        newrow (apply str (assoc row x v))]
    (assoc grid y newrow)))

(defn part-1 [in]
  (->> (find-all-objects in)
       (combo-all-groups in)
       set
       count
       ))

(defn mirrored-long
  "For x and y in grid find the mirrored object to middle point "
  [grid [x y] [bx by]]
  (let [maxx (count (first grid))
        maxy (count grid)
        newx (- (* 2 bx) x)
        newy (- (* 2 by) y)]
    (when (and (< -1 newx maxx)
               (< -1 newy maxy))
      [newx newy])))

;; TODO: fix mirrored long, use `for` and `:while` check until [newx newy] fits into the grid
;; TODO: change counting antinodes to include starting antennes as well

(defn part-2 [in]
  (let [objects (find-all-objects in)
        combined (combo-all-groups in objects)]
    ))


  (->> (find-all-objects in)
       (combo-all-groups in)
       set
       count
       ))


(comment
  (def intest (load-in :test))
  (def objs (find-all-objects intest))
  (def res (set (combo-all-groups intest objs)))
  (count res)

  (def in (load-in))
  (part-1 in)

   ;; => 381
  res
  ;; => #{nil
  ;;      [7 7]
  ;;      [2 3]
  ;;      [11 0]
  ;;      [4 2]
  ;;      [10 2]
  ;;      [10 11]
  ;;      [1 5]
  ;;      [0 7]
  ;;      [3 6]
  ;;      [10 10]
  ;;      [3 1]
  ;;      [9 4]
  ;;      [6 0]}

  (reduce
   (fn [g [x y]]
     (if (and x y)
       (assoc-in-vec-str g x y \#)
       g))
   intest
   res)
  ;; => ["......#....#"
  ;;     "...#....0..."
  ;;     "....#0....#."
  ;;     "..#....0...."
  ;;     "....0....#.."
  ;;     ".#....A....."
  ;;     "...#........"
  ;;     "#......#...."
  ;;     "........A..."
  ;;     ".........A.."
  ;;     "..........#."
  ;;     "..........#."]

  )
