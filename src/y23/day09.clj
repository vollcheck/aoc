(ns y23.day09
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(defn parse-line [line]
  (mapv parse-long (str/split line #" ")))

(defn diffs [line]
  (->> (partition 2 1 line)
       (mapv (fn [[x y]] (- y x)))))

(defn reduce-diffs
  ([line] (reduce-diffs [line] line))
  ([acc line]
   (let [dfs (diffs line)]
     (if (every? zero? dfs)
       (transduce (map peek) + 0 acc)
       (recur (conj acc dfs) dfs)))))

(defn diffs-2 [line]
  (->> (partition 2 1 line)
       (mapv #(apply - %))))

(defn reduce-diffs-2
  ([line]
   (reduce-diffs-2 [line] line))
  ([acc line]
   (let [dfs (diffs-2 line)]
     (if (every? zero? dfs)
       (transduce (map first) - 0 acc)
       (recur (into [dfs] acc)
              dfs)))))

(defn solve [reduction-fn lines]
  (transduce
   (comp (map parse-line)
         (map reduction-fn))
   +
   0
   lines))

(defn part-1 [lines]
  (solve reduce-diffs lines))

(defn part-2 [lines]
  (solve reduce-diffs-2 lines))

(comment
  (def tlines (load-in :test))
  (def lines (load-in))

  (do
    (assert (= 114        (part-1 tlines)))
    (assert (= 1930746032 (part-1 lines)))
    (assert (= 1154       (part-2 lines)))
    (assert (= 5          (solve reduce-diffs-2 [(last tlines)]))))

  )
