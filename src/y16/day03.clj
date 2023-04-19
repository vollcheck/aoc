(ns y16.day03
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(defn process-in [in]
  ;; split by tabulator,
  ;; omit first element as it's empty string
  ;; and parse string to number
  (mapv
   #(mapv parse-long (rest (str/split % #"\s+")))
   in))

(defn triangle? [v]
  (let [[s1 s2 s3] (sort v)]
    (> (+ s1 s2) s3)))

(defn transpose-by-3 [in]
  (reduce
   (fn [acc rows3]
     (into acc
           (for [x (range 3)]
             (mapv #(nth % x) rows3))))
   []
   (partition 3 in)))

(comment
  (def in (-> (load-in)
              process-in))

  ;; first part
  (count (filter triangle? in))
  ;; => 862

  (def transposed (transpose-by-3 in))
  ;; second part
  (count (filter triangle? (transpose-by-3 in)))
   ;; => 1577
  )
