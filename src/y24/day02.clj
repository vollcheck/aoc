(ns y24.day02
  (:require [clojure.string :as str]
            [core :refer [drop-at load-in]]))

(defn parse-line [line]
  (->> (re-seq #"\d+" line)
       (mapv parse-long)))

(defn safe-line? [line]
  (let [neg-tend (< (- (first line)
                       (second line))
                    0)]
    (->> line
         (partition 2 1)
         (map (fn [[x y]] (- x y)))
         (every? #(if neg-tend
                    (<= -3 % -1)
                    (<= 1 % 3))))))

(defn part-1 [in]
  (->> in
       (map (comp safe-line? parse-line))
       (filter identity)
       count))

;; NOTE: not optimized - combinatorics
(defn safe-with-removing? [line]
  (let [cnt (count line)]
    (->> (for [i (range cnt)]
           (drop-at i line))
         (some safe-line?))))

(defn part-2 [in]
  (->> in
       (map parse-line)
       (map #(or (safe-line? %)
                 (safe-with-removing? %)))
       (filter identity)
       count))

(comment
  (def intest  (load-in :test))
  (def in (load-in))
  (part-1 intest)
  ;; => 2
  (part-1 in)
  ;; => 572

  (part-2 intest)
  ;; => 4
  (part-2 in)
  ;; => 612
  )
