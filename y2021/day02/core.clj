(ns day01
  (:require [clojure.string :as str]))

(def filename "input.txt")

(defn process-instr [instr]
  ((fn [[s int]]
     (vector s (Integer/parseUnsignedInt int)))
   (str/split instr #" ")))


(defn load-input []
  (->> filename
       (slurp)
       (str/split-lines)
       (map process-instr)
       (into [])))

(def input (load-input))

(defn task1 [coll horizontal vertical]
  (if (empty? coll)
    (* horizontal vertical)
    (let [step (first coll)
          instr (first step)
          value (second step)]
      (case instr
        "forward" (task1 (rest coll) (+ horizontal value) vertical)
        "down" (task1 (rest coll) horizontal (+ vertical value))
        "up" (task1 (rest coll) horizontal (- vertical value))))))

;; Does anybody know why it doesn't work?
;; (defn task1 [coll]
;;   (reduce
;;    (fn [horizontal vertical [instr value]]
;;      (case instr
;;        "forward" (+ horizontal value)
;;        "down" (+ vertical value)
;;        "up" (- vertical value)))
;;    0 0 coll))

(defn task2 [coll aim horizontal depth]
  (if (empty? coll)
    (* horizontal depth)
    (let [step (first coll)
          instr (first step)
          value (second step)]
      (case instr
        "forward" (task2 (rest coll) aim (+ horizontal value) (+ depth (* aim value)))
        "down" (task2 (rest coll) (+ aim value) horizontal depth)
        "up" (task2 (rest coll) (- aim value) horizontal depth)))))

(comment
  (print (task1 input 0 0))
  (print (task2 input 0 0 0))
  )
