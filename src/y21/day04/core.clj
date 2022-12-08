(ns day04
  (:require [clojure.string :as str]))


;; Processing part.

(defn load-input [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (into [])))

(def sample (load-input "sample.txt"))

(defn get-numbers [coll]
  (->>
   (-> coll
       (first)
       (str/split #","))
   (map #(Integer/parseUnsignedInt %))
   (into [])))


(defn clean-boards [coll]
  (->> coll
       (rest)
       (filter #(not (empty? %)))
       (map #(str/split % #"\s+"))))

(defn parse-row [row]
  (->> row
       (filter #(not (str/blank? %)))
       (map #(Integer/parseUnsignedInt %))
       (into [])))

(defn parse-boards [boards]
  (->> boards
       (clean-boards)
       (map parse-row)
       (partition 5)))

;; Logical part.

(defn draw-numbers
  ([numbers]
   (draw-numbers numbers 5))
  ([numbers counter]
    (take counter numbers)))

(defn in? [coll number]
  "Check if collection contains number."
  (some #(= number %) coll))

(defn match-series [series picked-numbers]
  (every? #(in? picked-numbers %) series))

(defn sum-unpicked [board picked]
  "Sums all numbers that hasn't been picked on the board."
  (->> board
       (filter #(not (in? picked %)))
       (reduce +)))

(defn drawing [boards numbers]
  (loop [n 5]
    (if ()
      :here-will-be-the-solution
      (recur (+ n (inc n))))))

(def random-numbers (get-numbers sample)) ;; collection that we will draw numbers from
(def boards (parse-boards sample))

(def first-nums (draw-numbers random-numbers 18))

(comment
  (map (fn [board]
         (or (map #(match-series % first-nums) board)
             (mapv #(match-series % first-nums) board)
             ))
       boards)
  (drawing boards first-nums)
  first-nums
  (first (nth boards 2))
  (match-series (first (nth boards 2)) first-nums)
  (clojure.pprint/pprint draw-numbers)
  (clojure.pprint/pprint boards)
  )
