(ns y22.day02
  (:require [core :refer [load-input]]))

(defn parse-line-1 [line]
  (case line
    "A X" (+ 1 3) ;; rock draw
    "A Y" (+ 2 6) ;; paper win
    "A Z" (+ 3 0) ;; scissors loss
    "B X" (+ 1 0) ;; rock loss
    "B Y" (+ 2 3) ;; paper draw
    "B Z" (+ 3 6) ;; scissors win
    "C X" (+ 1 6) ;; rock win
    "C Y" (+ 2 0) ;; paper loss
    "C Z" (+ 3 3) ;; scissors draw
    ))

(defn parse-line-2 [line]
  (case line
    "A X" (+ 3 0) ;; lose to rock are scissors
    "A Y" (+ 1 3) ;; draw to rock is rock
    "A Z" (+ 2 6) ;; win to rock is paper
    "B X" (+ 1 0) ;; lose to paper is rock
    "B Y" (+ 2 3) ;; draw to paper is paper
    "B Z" (+ 3 6) ;; win to paper are scissors
    "C X" (+ 2 0) ;; lose to scissors is paper
    "C Y" (+ 3 3) ;; draw to scissors are scissors
    "C Z" (+ 1 6) ;; win to scissors is rock
    ))

(defn part-1 [input]
  (->> input
       (map parse-line-1)
       (reduce +)))

(defn part-2 [input]
  (->> input
       (map parse-line-2)
       (reduce +)))

(comment
  (def input (load-input))
  (part-1 (load-input :test)) ;; => 15
  (part-1 input) ;; => 14827
  (part-2 (load-input :test)) ;; => 12
  (part-2 input) ;; => 13889
  )
