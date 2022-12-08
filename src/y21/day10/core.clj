(ns day10
  (:require [clojure.string :as str]))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn load-input
  ([] (load-input "input.txt"))
  ([filename]
   (->> filename
        (slurp)
        (str/split-lines)
        (mapv #(str/split % #"")))))

(def pairs {"{" "}"
            "[" "]"
            "(" ")"
            "<" ">"})

(def points {")" 3
             "]" 57
             "}" 1197
             ">" 25137})

(defn find-pairs [line]
  (loop [opened []
         pos 0]
    (if (= pos (count line))
      nil ;; <- should I do something here?
      (let [elem (nth line pos)]
        (if (in? (keys pairs) elem) ;; check if element is an opening
          (recur (conj opened elem)
                 (inc pos))
          (if (= elem (get pairs (last opened))) ;; check if pairs is matched
            (recur (pop opened) ;; man... that drop/drop-last was changing structure to list
                   (inc pos))
            (get points elem)))))))

(defn sum-errors [input]
  (reduce
   (fn [cnt line]
     (+ cnt (or (find-pairs line) 0)))
   0 input))

;; Solution to the first part:
(println (sum-errors (load-input)))

;; Second part:
(def points2 {")" 1
              "]" 2
              "}" 3
              ">" 4})

(defn find-unclosed [line]
  (loop [opened []
         pos 0]
    (if (= pos (count line))
      (into [] (reverse (map #(get pairs %) opened)))
      (let [elem (nth line pos)]
        (if (in? (keys pairs) elem) ;; check if element is an opening
          (recur (conj opened elem)
                 (inc pos))
          (if (= elem (get pairs (last opened))) ;; check if pairs is matched
            (recur (pop opened) ;; man... that drop/drop-last was changing structure to list
                   (inc pos))
            nil)))))) ;; discard corrupted line

(defn sum-line [line]
  (reduce
   (fn [cnt closing]
     (+ (* 5 cnt) (get points2 closing)))
   0 (find-unclosed line)))

(defn pick-middle [input]
  (let [results (sort (filter #(> % 0) (map sum-line input)))
        middle (int (/ (count results) 2))]
    (nth results middle)))

;; Solution to the second part
(println (pick-middle (load-input)))
