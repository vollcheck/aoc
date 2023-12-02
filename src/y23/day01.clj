(ns y23.day01
  (:require [criterium.core :as c]
            [core :refer [load-in]]))

(defn parse-line-1 [line]
  (->> (re-seq #"[0-9]" line)
       #_#_(map str line) (keep parse-long) ;; 10x slower!
       ((juxt first last))
       (apply str)
       parse-long))

(defn part-1 [lines]
  (transduce
   (map parse-line-1)
   +
   0
   lines))

(comment
  (def lines (load-in))
  (assert (= (part-1 lines)
             54630))
  (c/quick-bench (part-1 lines))
  ;; Evaluation count : 1332 in 6 samples of 222 calls.
  ;;            Execution time mean : 512.928278 µs
  ;;   Execution time std-deviation : 86.674535 µs
  ;;  Execution time lower quantile : 434.797239 µs ( 2.5%)
  ;;  Execution time upper quantile : 608.785752 µs (97.5%)
  ;;                  Overhead used : 8.704769 ns
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

(def ltoi
  (zipmap literal (range 1 10)))

(defn parse-line-2 [line]
  (->>
   ;; positive lookahead regex as words might overlap each other
   (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|[0-9]))" line)
   ((juxt first last))
   (map (fn [[_ v]]
          (get ltoi v v)))
   (apply str)
   parse-long))

(defn part-2 [lines]
  (transduce
   (map parse-line-2)
   +
   0
   lines))

(comment
  (def lines (load-in))
  (assert (= (part-2 lines)
             54770))

  (re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|[0-9]))" (first lines))
  (c/quick-bench (part-2 lines))
  ;; Evaluation count : 282 in 6 samples of 47 calls.
  ;;            Execution time mean : 2.195897 ms
  ;;   Execution time std-deviation : 121.586088 µs
  ;;  Execution time lower quantile : 2.097486 ms ( 2.5%)
  ;;  Execution time upper quantile : 2.394791 ms (97.5%)
  ;;                  Overhead used : 8.704769 ns

  )
