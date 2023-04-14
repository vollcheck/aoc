(ns y19.day01
  (:require [core :refer [load-in]]))

(defn- compute-fuel ^Integer [^Integer mass]
  (-> (/ mass 3)
      (Math/floor)
      (- 2)
      int))

(defn recursive-compute-fuel ^Integer [^Integer mass]
  (loop [fuel (compute-fuel mass)
         sum fuel]
    (let [new-fuel (compute-fuel fuel)]
      (if (<= new-fuel 0)
        sum
        (recur new-fuel
               (+ sum new-fuel))))))

(defn sum-fuel ^Integer [input ff]
  (reduce
   (fn [acc x]
     (+ acc (ff (Integer/parseUnsignedInt x))))
   0
   input))

(comment
  (def test-input (load-in :test))
  (def input (load-in))

  ;; first part
  (sum-fuel input compute-fuel)
   ;; => 3414791

  ;; second part
  (sum-fuel input recursive-compute-fuel)
   ;; => 5119312

  (require '[criterium.core :refer [quick-bench]])
  (quick-bench (sum-fuel input compute-fuel))
  ;; Execution time mean : 7.644577 µs

  (quick-bench (sum-fuel input recursive-compute-fuel))
  ;; Execution time mean : 93.891560 µs

  (require '[clj-async-profiler.core :as prof])
  (prof/profile (sum-fuel input compute-fuel))
  (prof/profile (sum-fuel input compute-fuel))
  (prof/serve-ui 54321)
  )
