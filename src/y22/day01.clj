(ns y22.day01
  (:require [clojure.string :as str]))

(defn load-input []
  (slurp "src/y22/input01.txt"))

(defn parse-group [group]
  (->> group
       str/split-lines
       (map #(Integer/parseInt %))
       (apply +)
       ))

(defn part-1 [input]
  (->> (str/split input #"\n\n")
       (map parse-group)
       (apply max)
       ))

(defn part-2 [input]
  (->> (str/split input #"\n\n")
       (map parse-group)
       (sort >)
       (take 3)
       (apply +)
       ))


(comment
  (def input (load-input))
  (part-1 input) ;; => 72017
  (part-2 input) ;; => 212520
  )
