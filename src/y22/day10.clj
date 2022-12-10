(ns y22.day10
  (:require [clojure.string :as str]
            [core :refer [load-input]]
            [purity :refer [defpure]]))

(def registers [20 60 100 140 180 220])

(defn unroll [instrs]
  (reduce
   (fn [agg instr] (if (str/starts-with? instr "addx")
                    (conj agg "noop" instr)
                    (conj agg instr)))
   [] instrs))

(defn eval-instr [agg instr]
  (let [last-instr (or (peek agg) 1)]
    (cond
      (str/starts-with? instr "noop")
      (conj agg last-instr)

      (str/starts-with? instr "addx")
      (let [[_ v] (str/split instr #" ")
            v (parse-long v)]
        (conj agg (+ last-instr v))))))

(defn compute-signal [result]
  (apply
   +
   (for [r registers]
     (* r (nth result (dec r))))))

(defn part-1 [input]
  (->> input
       (unroll)
       (reduce eval-instr [1])
       (compute-signal)))

(defn pixel-lit? [idx x]
  (if (<= (abs (- x (mod idx 40))) 1)
    "#"
    "."))

(defn print-crt [crt]
  (for [s (partition 40 crt)]
    (println (apply str s))))

(defn part-2 [input]
  (->> input
       (unroll)
       (reduce eval-instr [1])
       (map-indexed pixel-lit?)
       (print-crt)))


(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input) ;; => 13140
  (part-1 input) ;; => 12460
  (part-2 test-input)
  (part-2 input)
  )
