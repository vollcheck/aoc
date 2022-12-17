(ns y18.day02
  (:require [clojure.string :as str]
            [clojure.core.match :as match]
            [clojure.math.combinatorics :as combo]
            [core :refer [load-input]]))

(defn filter-2-and-3 [state coll]
  #_{:clj-kondo/ignore [:not-empty?]}
  (match/match [(not (empty? (filter #(= 2 %) coll)))
                (not (empty? (filter #(= 3 %) coll)))]
               [true true] (-> state
                               (update :2 inc)
                               (update :3 inc))
               [true false] (update state :2 inc)
               [false true] (update state :3 inc)
               :else state))


(defn part-1 [input]
  ;; TODO (someday): candidate for a transducer
  (->> input
       (mapv (comp vals frequencies))
       (reduce filter-2-and-3 {:2 0 :3 0})
       (vals)
       (apply *)))

(defn distance [x y]
  (->> (for [i (range (count x))]
         (= (.charAt x i)
            (.charAt y i)))
       (remove true?)
       (count)))

(defn remove-different [x y]
  (str/join (for [i (range (count x))]
              (if (= (.charAt x i) (.charAt y i))
                (.charAt x i)
                ""))))

(defn part-2 [input]
  (->> (combo/combinations input 2)
       (filter (fn [[x y]] (= 1 (distance x y))))
       (first)
       (apply remove-different)))

(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input) ;; => 12
  (part-1 input) ;; => 9633
  (part-2 test-input)
  (part-2 input) ;; => "lujnogabetpmsydyfcovzixaw"
  )
