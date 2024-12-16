(ns y24.day15
  (:require [core :refer [in? get2 load-in]]
            [clojure.string :as str]))

(defn third [x]
  (first (nnext x)))

(defn do-move [[objects pos] move]
  ;; get all things in front of you
  (let [[x y] pos
        [axis objects-infront new-pos]
        (case move

          ;; same x, bigger y
          \v [:x
              (->> objects
                   (filter
                    (fn [[_type ox oy]]
                      (and (= x ox)
                           (< y oy))))
                   (sort-by third <))
              (update pos 1 inc)]

          ;; bigger x, same y
          \> [:y
              (->> objects
                   (filter (fn [[_type ox oy]]
                             (and (< x ox)
                                  (= y oy))))
                   (sort-by second <))
              (update pos 0 inc)]

          ;; same x, smaller y
          \^ [:x
              (->> objects
                   (filter (fn [[_type ox oy]]
                             (and (= x ox)
                                  (> y oy))))
                   (sort-by third >))
              (update pos 1 dec)]

          ;; smaller x, same y
          \< [:y
              (->> objects
                   (filter (fn [[_type ox oy]]
                             (and (> x ox)
                                  (= y oy))))
                   (sort-by second >))
              (update pos 0 dec)])

        ;; check if the new-pos' coords are in objects and if so,
        ;; if it's a wall... so really it's about check if we don't
        ;; walk into a wall
        new-pos-is-a-wall? (some
                            (fn [[c x y]]
                              (and (= \# c)
                                   (= x (first new-pos))
                                   (= y (second new-pos))))
                            objects-infront)]
    (if new-pos-is-a-wall?
      [objects pos]

      (let [df (case axis
                 :x third
                 :y second)

            ;; _ (println "pos" pos "move" move)
            ;; _ (println "objects infront" objects-infront)

            [before-wall wall-and-after]
            (split-with #(not= \# (first %)) objects-infront)


            ;; _ (println "before wall" before-wall)
            ;; _ (println "wall and after" wall-and-after)

            first-wall (first wall-and-after)
            pos-fn (case axis
                     :x second
                     :y first)


            ;; _ (println "first wall: " first-wall)

            ;; we can push if the cnt is smaller than (abs (- wall pos))
            ;; also dec because we count fields in between, without
            ;; current cell that we are standing on
            can-push? (< (count before-wall)
                         (abs (- (dec (df first-wall)) (pos-fn pos))))]

        (if (not can-push?)
          [objects pos]


          ;; now find all the boxes that we can push
          ;; `before-wall` and `wall-and-after` are already sorted :)
          ;; in the right direction meaning from your pos to the wall
          (let [boxes-to-push (->> (map-indexed vector before-wall)
                                   (filter
                                    (fn [[i o]]
                                      (= (inc i)
                                         (abs (- (pos-fn pos)
                                                 (df o)))))))

                ;; - remove all `boxes-to-push` from the objects
                ;; - increment
                ;; - put all `boxes-to-push` to the new-objects

                ;; _ (println "boxes to push:" boxes-to-push)

                [diff-key diff-fn] (case move
                                     \v [2 inc]
                                     \> [1 inc]
                                     \^ [2 dec]
                                     \< [1 dec])

                boxes-after-push (mapv
                                  (fn [[_idx box]]
                                    (println "B" box)
                                    (let [res (update box diff-key diff-fn)]
                                      (println "RES" res)
                                      res))
                                  boxes-to-push)


                ;; we should remove
                new-objects (remove #(in? % before-wall #_boxes-to-push)
                                    objects)
                new-objects (apply conj new-objects boxes-after-push)]
            [new-objects new-pos]))))))

(defn score-board [objects]
  (transduce
   (map (fn [[c x y]]
          (if (= c \O)
            (+ x (* 100 y))
            0)))
   + 0 objects))

(comment
  ;; DO NOT SPLIT INPUT
  (let [[grid moves] (str/split
                      (load-in :test :no-split)
                      #"\n\n")
        moves (str/replace moves #"\n" "")
        grid (mapv #(into [] %) (str/split-lines grid))
        maxx (count (first grid))
        maxy (count grid)

        ;; NOTE: there might be an issue that
        objects (for [y (range maxy)
                      x (range maxx)
                      :let [c (get2 grid x y)]
                      :when (#{\# \O \@} c)]
                  [c x y])
        get-position-fn (fn [o] (= (first o) \@))
        position (into [] (drop 1 (first (filter get-position-fn objects))))
        objects (remove get-position-fn objects)]
    ;; (filter (fn [[_type ox oy]]
    ;;           (and (> (first position) ox)
    ;;                (= (second position) oy)))
    ;;         objects)
    (reduce
     do-move
     [objects position]
     moves)
    )
  ;; NOTE: I PUSHED EVERY BOX OUT OF BOARD HAHAHA


   ;; => [([\# 0 0]
  ;;      [\# 1 0]
  ;;      [\# 2 0]
  ;;      [\# 3 0]
  ;;      [\# 4 0]
  ;;      [\# 5 0]
  ;;      [\# 6 0]
  ;;      [\# 7 0]
  ;;      [\# 8 0]
  ;;      [\# 9 0]
  ;;      [\# 0 1]
  ;;      [\# 9 1]
  ;;      [\# 0 2]
  ;;      [\# 9 2]
  ;;      [\# 0 3]
  ;;      [\# 9 3]
  ;;      [\# 0 4]
  ;;      [\# 9 4]
  ;;      [\# 0 5]
  ;;      [\# 2 5]
  ;;      [\# 9 5]
  ;;      [\# 0 6]
  ;;      [\# 9 6]
  ;;      [\# 0 7]
  ;;      [\# 9 7]
  ;;      [\# 0 8]
  ;;      [\# 9 8]
  ;;      [\# 0 9]
  ;;      [\# 1 9]
  ;;      [\# 2 9]
  ;;      [\# 3 9]
  ;;      [\# 4 9]
  ;;      [\# 5 9]
  ;;      [\# 6 9]
  ;;      [\# 7 9]
  ;;      [\# 8 9]
  ;;      [\# 9 9])
  ;;     [1 6]]
  )
