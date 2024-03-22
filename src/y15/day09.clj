(ns y15.day09
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [core :refer [load-in]]))

(defn parse-line [state line]
  (let [[from _ to _ dist] (str/split line #" ")]
    ;; trick: by making key the set, I can do a bidirectional search
    (assoc state #{from to} (parse-long dist))))

(defn get-graph [in]
  (reduce parse-line {} in))

(defn get-cities [in]
  (reduce
   (fn [state line]
     (let [[c1 _ c2] (str/split line #" ")]
       (into state #{c1 c2})))
   #{}
   in))

(comment
  (vec (get-cities (load-in :test)))
  ;; => ["Belfast" "London" "Dublin"]
  )

(defn calculate-trip-dist [graph trip]
  (->> (partition 2 1 trip)
       (reduce
        (fn [dist part-trip]
          (+ dist (get graph (set part-trip))))
        0)))

(defn calculate-all-trip-distances [in]
  (let [graph (get-graph in)
        cities (get-cities in)]
    (->> (vec cities)
         combo/permutations
         (map (partial calculate-trip-dist graph)))))

(defn solve [f in]
  (->> (calculate-all-trip-distances in)
       (apply f)))

(comment
  (def test-in (load-in :test))
  (def part-1-test (solve min test-in))
  (def part-2-test (solve max test-in))

  (def in (load-in))
  (def part-1 (solve min in))
  (def part-2 (solve max in))
  )
