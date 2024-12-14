(ns y23.day14
  (:require [core :refer [load-in transpose]]))

(defn shift-group [grp]
  (if (= (first grp) \#)
    grp
    (let [freq (frequencies grp)
          o-freq (get freq \O 0)
          d-freq (get freq \. 0)]
      #_(println freq o-freq d-freq)

      #_(into [] (concat (repeat o-freq \O) (repeat d-freq \.)))
      (concat (repeat o-freq \O) (repeat d-freq \.)))))

(defn shift-line [line]
  (->> line
       (partition-by #(= % \#))
       (map shift-group) ;; TODO transducer some day
       (reduce (fn [a c] (into a c)) [])))

(defn count-o [line]
  (reduce
   (fn [a c] (if (= c \O) (inc a) a))
   0
   line))

(defn compute-load [grid]
  (->> (transpose grid) ;; TODO: refactor - do not re-transpose! you need to align the
       reverse
       (transduce
        (map-indexed (fn [idx line] (* (inc idx)
                                      (count-o line))))
        + 0)))

(defn part-1 [lines]
  (->> (transpose lines)
       (mapv shift-line)
       compute-load))

(defn do-cycle [direction grid]
  grid)

(comment
  (reduce
   (fn [a _] (inc a))
   0
   (range 1000000000))
  (take 10 (iterate inc 1)) ;; better reduce?
  )

(defn part-2 [lines]
  (->> (transpose-s lines)
       ) ;; NORTH, WEST, SOUTH, EAST
  ;; north load after 1000000000 cycles
  )

(defn transpose [m]
  (apply mapv vector m))
(defn rtranspose [m]
  (apply mapv vector (reverse m)))

(defn rtranspose2 [m]
  (into [] (reverse (apply mapv vector m))))

(rtranspose2 [[1 2]
              [3 4]])

(transpose [[1 2]
            [3 4]])
[[1 3]
 [2 4]]

(rtranspose [[1 2]
             [3 4]])
[[3 1]
 [4 2]]

(comment
  (def tlines (load-in :test))
  (def lines (load-in))

  (part-1 tlines)
   ;; => 136
  (part-1 lines)
  ;; => 106648

  (assert (= (part-1 lines) 106648))

  (part-2 tlines)

  )

(comment
  ;; --- BENCHMARK
  (require '[criterium.core :as c])

  (c/quick-bench (= 106648 (part-1 lines)))
  ;; Evaluation count : 120 in 6 samples of 20 calls.
  ;;            Execution time mean : 5.863131 ms
  ;;   Execution time std-deviation : 899.830681 µs
  ;;  Execution time lower quantile : 5.131549 ms ( 2.5%)
  ;;  Execution time upper quantile : 6.936988 ms (97.5%)
  ;;                  Overhead used : 5.786584 ns

  ;; DONE: Plan for the optimization is to operate on vectors
  (c/quick-bench (= 106648 (part-1 lines)))
  ;; Evaluation count : 90 in 6 samples of 15 calls.
  ;;            Execution time mean : 6.981446 ms
  ;;   Execution time std-deviation : 133.108777 µs
  ;;  Execution time lower quantile : 6.868677 ms ( 2.5%)
  ;;  Execution time upper quantile : 7.193875 ms (97.5%)
  ;;                  Overhead used : 5.786584 ns

  ;; NOTE: apparently, operating on vectors instead of strings is slower

  ;; TODO: will it be faster to operate on the ints instead of chars?
  )
