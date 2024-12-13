(ns y24.day12
  (:require [core :refer [get2 load-in ->queue third]]))

(defn pin
  "Turn into vec of vecs for easier lookup"
  [in]
  (mapv (partial into []) in))

(def intest1 ["AAAA"
              "BBCD"
              "BBCC"
              "EEEC"])

(def intest2 ["OOOOO"
              "OXOXO"
              "OOOOO"
              "OXOXO"
              "OOOOO"])

(def intest3 ["EEEEE"
              "EXXXX"
              "EEEEE"
              "EXXXX"
              "EEEEE"])

(def intest4 ["AAAAAA"
              "AAABBA"
              "AAABBA"
              "ABBAAA"
              "ABBAAA"
              "AAAAAA"])

(defn turn-to-indexes [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [x y]))

(defn get-next-letter [grid letter [x y]]
  (let [maxy (count grid)
        maxx (count (first grid))]
    ;; look around without diagonals
    (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
          :let [nx (+ x dx)
                ny (+ y dy)]
          :when (and (< -1 nx maxx)
                     (< -1 ny maxy)
                     (= letter (get2 grid nx ny)))]
      [nx ny])))

(defn get-region [grid letter starting-point]
  (loop [letters #{starting-point}
         q (->queue starting-point)]
    (if (empty? q)
      letters
      (if-let [result (->> (get-next-letter grid letter (peek q))
                           (remove #(letters %))
                           seq)]
        (recur (apply conj letters result)
               (apply conj (pop q) result))
        (recur letters
               (pop q))))))

(defn get-all-letters [grid]
  (reduce
   (fn [{:keys [seen groups] :as acc} point]
     (let [letter (get2 grid point)
           region (get-region grid letter point)]
       (if (seq region)
         (-> (assoc acc :seen (apply conj seen region))
             (assoc :groups (conj groups region)))
         acc)))
   {:seen #{}
    :groups #{}}
   (turn-to-indexes grid)))

(defn perimeter [grid group]
  (let [maxx (count (first grid))
        maxy (count grid)
        letter (get2 grid (first group))]
    (count
     (reduce
      (fn [acc [x y]]
        (->> (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                   :let [nx (+ x dx)
                         ny (+ y dy)]]
               (if (or (< -1 nx maxx)
                       (< -1 ny maxy)
                       (not= letter (get2 grid nx ny)))
                 [nx ny]))
             set
             (remove #(group %))
             (apply conj acc)))
      []
      group))))

(defn score [grid multiplier-fn group]
  (apply * ((juxt count (partial multiplier-fn grid)) group)))

(defn solve [multiplier-fn in]
  (let [in (pin in)]
    (->> (get-all-letters in)
         :groups
         (transduce
          (map (partial score in multiplier-fn))
          + 0))))

;; to calculate number of sides it's best to gather all points of perimeter
;; and enrich them with a side that they are "looking" to
;; and then merge them into groups if there are many points looking to
;; the same side and with same X or Y coordinate



(defn collect-small-changes

  [coords]
  (let [dir-fn (case (ffirst coords)
                 (:east :west) third
                 (:north :south) second)
        ns (sort (map dir-fn coords))]
    (->> (partition 2 1 ns)
         (reduce
            (fn [acc [a b]]
              (if (= 1 (- b a))
                (conj (pop acc) (conj (peek acc) b))
                (conj acc [b])))
            [[(first ns)]])
         count)))

(defn number-of-sides [grid group]
  (let [maxx (count (first grid))
        maxy (count grid)
        letter (get2 grid (first group))]
    (->> group
         (reduce
          (fn [acc [x y]]
            (->> (for [[face dx dy] [[:east -1 0]
                                     [:west 1 0]
                                     [:south 0 -1]
                                     [:north 0 1]]
                       :let [nx (+ x dx)
                             ny (+ y dy)]]
                   (if (or (< -1 nx maxx)
                           (< -1 ny maxy)
                           (not= letter (get2 grid nx ny)))
                     [face nx ny]))
                 set
                 (remove #(group (drop 1 %)))
                 (apply conj acc)))
          [])
         (reduce
          (fn [ret [face x y]]
            (let [field (case face
                          (:east :west) x
                          (:north :south) y)]
              (assoc ret [face field] (conj (get ret [face field] []) [face x y]))))
          {})
         (transduce
          (map (comp collect-small-changes second))
          +
          0))))

(comment

  ;; part 1
  (solve perimeter intest1)
  ;; => 140
  (solve perimeter intest2)
  ;; => 772
  (solve perimeter (load-in :test))
  ;; => 1930
  (solve perimeter (load-in))
  ;; => 1424472

  ;; part 2
  ;; we expect 80
  (solve number-of-sides intest1)
  ;; => 80

  ;; we expect 436
  (solve number-of-sides intest2)
  ;; => 268

  ;; we expect 236
  (solve number-of-sides intest3)
   ;; => 236
  ;; we expect 386
  (solve number-of-sides intest4)
   ;; => 368

  ;; we expect 1206
  (solve number-of-sides (load-in :test))
   ;; => 1206
  (solve number-of-sides (load-in))
   ;; => 870202
  )
