(ns y22.day06
  (:require [core :refer [load-input]]))

(defn get-distinctive-idx [^String s ^Integer shift]
  (loop [s (partition shift 1 s)
         idx shift]
    (let [f (first s)]
      (if (or (empty? f)
              (apply distinct? f))
        idx
        (recur (rest s)
               (inc idx))))))

(defn part-1 [^String s]
  (get-distinctive-idx s 4))

(defn part-2 [^String s]
  (get-distinctive-idx s 14))

(comment
  ;; OPTIMIZATION NOTE: could've using sets instead of partition

  (def test-input (first (load-input :test)))
  (def input (first (load-input)))
  (part-1 test-input)
  (part-1 input) ;; => 1920
  (time (part-1 input)) ;; => 1920
  (part-2 test-input)
  (time (part-2 input)) ;; => 2334
  )
