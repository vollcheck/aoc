(ns y24.day10
  (:require [core :refer [get2 find-cells parse-int load-in]]))

(defn parse-in [in]
  (mapv
   (fn [line] (mapv parse-int line))
   in))

(defn find-next-digits [grid v [x y]]
  (let [maxy (count grid)
        maxx (count (first grid))
        candidate (inc v)]
    ;; look around without diagonals!
    (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
          :let [nx (+ x dx)
                ny (+ y dy)]
          :when (and (< -1 nx maxx)
                     (< -1 ny maxy)
                     (= candidate (get2 grid nx ny)))]
      [nx ny])))

(defn lazy-find-paths [grid pos seen path]
  (let [value (get2 grid pos)]
    (if (= value 9)
      (list path)
      (->> (find-next-digits grid value pos)
           (remove seen)
           (mapcat (fn [next-pos]
                     (lazy-seq
                      (lazy-find-paths grid
                                       next-pos
                                       (conj seen next-pos)
                                       (conj path next-pos)))))))))

(defn find-zeros [grid]
  (find-cells grid 0))

(defn part-1 [in]
  (->> (for [zero (find-zeros in)]
         (->> (lazy-find-paths in zero #{zero} [zero])
              (group-by peek) ;; group by the position of 9's
              count))
       (reduce + 0)))

(defn part-2 [in]
  (->> (for [zero (find-zeros in)]
         (count (lazy-find-paths in zero #{zero} [zero])))
       (reduce + 0)))

(comment
  (def intest (load-in :test))
  (def in (load-in))

  (part-1 (parse-in intest))
  ;; => 36

  "Elapsed time: 62.482 msecs"
  (time (part-1 (parse-in (load-in))))


  ;; => 538

  (part-2 (parse-in intest))
  ;; => 81

  (part-2 (parse-in (load-in)))
  ;; => 1110
  )
