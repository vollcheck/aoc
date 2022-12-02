(ns y22.day01
  (:require [clojure.string :as str]
            [core :refer [load-input]]))

(defn parse-group [group]
  (->> group
       str/split-lines
       (map parse-long)
       (apply +)))

(defn part-1 [input]
  (->> (str/split input #"\n\n")
       (map parse-group)
       (apply max)))

(defn part-2 [input]
  (->> (str/split input #"\n\n")
       (map parse-group)
       (sort >)
       (take 3)
       (apply +)))

(comment
  (def input (load-input))
  (part-1 input) ;; => 72017
  (part-2 input) ;; => 212520
  )
