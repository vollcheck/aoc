(ns y23.day01
  (:require [core :refer [load-in]]
            [clojure.string :as str]))

(defn parse-line-1 [line]
  (->> (re-seq #"[0-9]" line)
       ((juxt first last))
       (str/join "")
       parse-long))

(defn part-1 [lines]
  (->> lines
       (map parse-line-1)
       (reduce + 0)))

(comment
  (part-1 (load-in))
  )

(def literal
  ["one"
   "two"
   "three"
   "four"
   "five"
   "six"
   "seven"
   "eight"
   "nine"])

(def stoi
  (zipmap literal (range 1 10)))

(defn parse-line [line]
  (->>
   ;; lookahead regex as words might overlap each other
   (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|[0-9]))" line)
   (map #(second %))
   ((juxt first last))
   (map #(get stoi % %))
   (str/join "")
   parse-long))

(defn part-2 [lines]
  (->> (map parse-line lines)
       (reduce + 0)))

(comment
  (part-2 (load-in))
  )
