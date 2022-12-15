(ns y22.day13
  (:require [clojure.string :as str]
            [core :refer [load-input]]
            [purity :refer [defpure]]))

(defn cmp-vec-lexi
  "Slightly modified version from
  https://clojure.org/guides/comparators"
  [cmpf x y]
  (let [x-len (count x)
        y-len (count y)
        len (min x-len y-len)]
    (loop [i 0]
      (let [c (cmpf (x i) (y i))]
          (if (zero? c)
            (recur (inc i))
            c)))))


(defn compare' [x y]
  (let [result
        (condp = [(int? x) (int? y)]
          [true true] (compare x y)
          [true false] (compare' [x] y)
          [false true] (compare' x [y])
          (if-let [ordered?
                   (first (filter #(not (= 0 %)) (map compare' x y)))]
            ordered?
            (compare (count x) (count y))))]

    (println "comparing" x "and" y ":" result)
    result))

(def x
  [[[6 2 3 9 9] [4 1] 10 [] 3] [4 1 [8 5 5 8] [6 7 2] [4]]])
(def y
  [[6 5 0] 9 5 7 2])

(map compare' x y)
(cmp-vec-lexi compare' x y)


(defn range-from-1 []
  (iterate inc' 1))

(defn prepare-input [input]
  (->> input
       (remove empty?)
       (map read-string)
       (partition 2)))


(def in
  (str/split (load-input :test :no-split) #"\n\n"))

(first in) ;; => "[1,1,3,1,1]\n[1,1,5,1,1]"
(str/split (load-input :test :no-split) #"\n\n")

(defn parse-lists [ls]
  (mapv read-string (str/split-lines ls)))


(def inn (mapv parse-lists in))

(mapv
 (fn [[x y]] (cmp-vec-lexi compare' x y))
 inn)

(->>
 ;; TODO: prepare input is prettier
 ;; (str/split (load-input :test :no-split) #"\n\n")
 (str/split (load-input :no-split) #"\n\n") ;; test
 (mapv parse-lists)
 (mapv (fn [[x y]] (compare' x y)))
 (zipmap (range-from-1))
 (reduce (fn [agg [k v]]
           (if (= v -1)
             (+ agg k)
             agg))
         0))

(defn part-1 [in]
  (->> in
       (map (fn [[x y]] (cmp-vec-lexi compare' x y)))
       (zipmap (range-from-1))
       (reduce (fn [agg [k v]]
                 (if (= v -1)
                   (+ agg k)
                   agg))
               0)))

(defn enrich [in]
  (conj in [[2]] [[6]]))

(->> (load-input :test)
     (remove empty?)
     (map read-string)
     (enrich)
     (sort compare')
     (zipmap (range-from-1))
     ;; (keep #(when (or (= (val %) [[2]])
     ;;                  (= (val %) [[6]])
     ;;          (key %))))
     (filter (fn [[_ v]] (or (= v [[2]])
                            (= v [[6]]))))
     keys
     (apply *)
     )

(defn part-2 [input]
  (->> input
       (remove empty?)
       (map read-string)
       (enrich)
       (sort compare')
       (zipmap (range-from-1))
       (filter (fn [[_ v]] (or (= v [[2]])
                              (= v [[6]]))))
       (keys)
       (apply *)
       )
  )

(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input) ;; => 13
  (part-1 input) ;; => 5545 is wrong (too high)
  (part-2 test-input)
  (part-2 input)
  )

(comment
  (cmp-vec-lexi compare' [1,1,3,1,1] [1,1,5,1,1]) ;; => -1
  (cmp-vec-lexi compare' [[1] [2,3,4]] [[1] 4]) ;; => -1
  (cmp-vec-lexi compare' [9] [[8,7,6]]) ;; => 1 ;; NOT
  (cmp-vec-lexi compare' [[4,4],4,4] [[4,4],4,4,4]) ;; => -1
  (cmp-vec-lexi compare' [7,7,7,7] [7,7,7]) ;; => 1 ;; NOT
  (cmp-vec-lexi compare' [] [3]) ;; => -1
  (cmp-vec-lexi compare' [[[]]] [[]]) ;; => 1 ;; NOT

  (->> ["[[4,[1,[]]],[8,3,[[0,2],[5,2,6],[7,0,10,0],2,[5,7,10,2]],[[5,9],5,10,[9,7,7]]]]"
        "[[[],9],[[[3,2,6,3],[7,8]],10],[1]]"]
     (map (fn [[x y]] (cmp-vec-lexi compare' x y)))
     (zipmap (range-from-1)) ;; => {1 -1, 2 -1, 3 1, 4 -1, 5 1, 6 -1, 7 1, 8 1}
     (reduce (fn [agg [k v]]
               (if (= v -1)
                 (+ agg k)
                 agg))
             0)
     )
  )
