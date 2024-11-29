(ns y18.day11-faster
  (:require [clojure.string :as str]
            [core :refer [parallel-process parse-int]]))

;; this solution uses two optimization techniques
;; 1. https://en.wikipedia.org/wiki/Summed-area_table
;;    SAT provides sums for all cells upfront making it fast for calculating square sums afterwards
;; 2. I'm operating on the "flattened" contiguous array rather than on
;;    array of arrays.
;; 3. TBI int-array instead of vector
;; 4. do the SAT calculations on grid generation

(def ^:dynamic *serial* 7511)

(def ^:dynamic *width* 300)

(defn get-power [x y]
  (let [rack (+ x 10)]
    (-> rack
        (* y)
        (+ *serial*)
        (* rack)
        (quot 100)
        (mod 10)
        (- 5))))

;; flat array primitives
;; here's I'm developing a way of dealing with contiguous array
;; also it's worth noting that whenever I'm talking about
;; that "concatenated" array, I mean the "row-major order"
;; that means the rows are contiguous hence access would be dictated
;; by the row lenght.
;; also I'm assumming that every row has the same width!

;; IDEA: create a general function for `core` namespace for that but with signature like
;;       (create-grid width cell-fn)
;;       and for this case it would be (create-grid width (fn [x y] (get-power (inc x) (inc y))))
;;       so that it would be applicable to other use cases
(defn create-grid []
  (->> (for [y (range 0 *width*)
             x (range 0 *width*)]
         ;; note the shifted x&y - just for that particular exercise
         ;; that constraint could be removed, but don't want to do it now
         (get-power (inc x) (inc y)))
       ;; output is vector for faster access
       ;; TODO: eventually I want to do the summing here at one pass!
       (into [])))

(defn get2
  "same as `get` but for concatenated array
  (that could be an array of arrays naively),
  also assumes that every row is the same length

  works good if you do

  (def f (partial get2 grid row-length))
  (f x y)

  as you know the grid (that wouldn't change)
  and row-length (that wouldn't change as well)
  upfront anyway.
  "
  [grid row-length x y]
  (nth grid (+ (* y row-length) x)))

(defn generate-sat
  "enriches a (flattened) grid with summed area"
  [grid]
  ;; assuming that width is the same as height
  (->> (range 0 (* *width* *width*))
       (reduce
        (fn [acc i]
          (let [f (partial get2 acc *width*)
                y (quot i *width*)
                x (mod i *width*)
                ;; this cell is calculated differently
                this-cell (get2 grid *width* x y)]
            (conj acc (cond-> this-cell
                        (>= y 1) ;; top, 1 bcs we will decrement it
                        (+ (f x (dec y)))

                        (>= x 1) ;; left, 1 bcs we will decrement it
                        (+ (f (dec x) y))

                        (and (>= x 1) (>= y 1)) ;; top-left
                        (- (f (dec x) (dec y)))))))
        [])))

(defn square-power-for-n [grid x y n]
  (let [f (partial get2 grid *width*)
        top-left  (f x       y)
        top-right (f (+ x n) y)
        bot-left  (f x       (+ y n))
        bot-right (f (+ x n) (+ y n))]
    (- (+ top-left bot-right)
       top-right bot-left)))

(defn max-square-power-for-n [grid n]
  (->> (for [y (range 0 (- *width* n))
             x (range 0 (- *width* n))]
         {:x (inc x)
          :y (inc y)
          :n n
          :power (square-power-for-n grid x y n)})
       (apply max-key :power)))

(defn format-winner [{:keys [x y n power]}]
  #_(println "found a winner with" power "power points")
  (format "%d,%d,%d" x y n))

(defn find-max-square-fn [map-fn]
  (->> (range 0 *width*)
       (map-fn)
       (apply max-key :power)
       ;; it was
       ;; (sort-by :power >)
       ;; first
       format-winner))

(defn find-max-square [grid]
  (find-max-square-fn
   (fn [coll]
     (map #(max-square-power-for-n grid %) coll))))

(defn find-max-square-pmap [grid]
  (find-max-square-fn
   (fn [coll]
     (pmap #(max-square-power-for-n grid %) coll))))

(defn find-max-square-parallel [grid]
  (find-max-square-fn
   (fn [coll]
     (parallel-process #(max-square-power-for-n grid %)
                       (+ (.availableProcessors (Runtime/getRuntime)) 2)
                       coll))))

(defn find-max-square-parallel-32 [grid]
  (find-max-square-fn
   (fn [coll]
     (parallel-process #(max-square-power-for-n grid %)
                       32
                       coll))))

(comment
  (require '[clj-async-profiler.core :as prof])

  (defn part-2 [max-square-fn]
    (-> (create-grid)
        (generate-sat)
        (max-square-fn)))

  (time (part-2 find-max-square))  "Elapsed time: 10576.844 msecs"
  (time (part-2 find-max-square-pmap))  "Elapsed time: 7069.0153 msecs"
  (time (part-2 find-max-square-parallel)) "Elapsed time: 5206.8414 msecs"
  (time (part-2 find-max-square-parallel-32)) "Elapsed time: 5003.5085 msecs"

  ;; after change from `sort-by` to `max-key`

  (time (part-2 find-max-square)) "Elapsed time: 3905.9545 msecs"
  (time (part-2 find-max-square-pmap)) "Elapsed time: 1965.962 msecs"
  (time (part-2 find-max-square-parallel)) "Elapsed time: 1747.1284 msecs"
  (time (part-2 find-max-square-parallel-32)) "Elapsed time: 1784.0365 msecs"

  (prof/profile (part-2))
  #_(prof/serve-ui 8080)
  )
