(ns y24.day03
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(def intest
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn parse-mul-exp
  "gets string line 'mul(11,8)' and do the multiplication of arguments"
  [exp]
  (->> (re-seq #"\d+" exp)
       (map parse-long)
       (apply *)))

(defn part-1
  "for given line, looks for all occurencies of mul(d,d)
  multiplicates those arguments and sum them althogether"
  [line]
  (->> (re-seq #"mul\(\d{1,3}\,\d{1,3}\)" line)
       (transduce (map parse-mul-exp)
                  +
                  0)))

(def intest2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn part-2 [in]
  (loop [s in
         c 0]
    (if (empty? s)
      c
      (let [[to-count rest-s] (str/split s #"don\'t\(\)" 2)
            c (+ c (sum-line to-count))]
        (if (empty? rest-s)
          c
          (recur (second (str/split rest-s #"do\(\)" 2))
                 c))))))

(comment
  (part-1 intest)
  ;; => 161
  (part-1 (load-in :no-split))
  ;; => 179834255

  (part-2 intest2)
  ;; => 48
  (part-2 (load-in :no-split))
  ;; => 80570939
  )
