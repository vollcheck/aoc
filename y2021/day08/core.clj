(ns day08
  (:require [clojure.string :as str]))

(defn load-input [filename]
  (mapv
   #(str/split % #" \| ")
   (str/split-lines (slurp filename))))

(defn sec [coll]
  "Assert that coll is a vector."
  (nth coll 1))

(defn in?
  [coll elm]
  (some #(= elm %) coll))

(def wanted-digits [2 4 3 7]) ;; lenghts of 1, 4, 7 and 8 in seven-segment system

(defn count-digits [digits]
  (->> digits
       (mapv sec)
       (mapv (fn [line]
               (filter
                #(in? wanted-digits (count %))
                (str/split line #" "))))
       (flatten)
       (count)))

;; First part.
(println (count-digits (load-input "input.txt")))

;; Second part.

;; first loop - get 1, 4, 7 and 8
(defn extract-base [digits]
  (loop [coll digits
         mapping {}]
    (if (empty? coll)
      mapping
      (let [number (first coll)]
        (case (count number)
          2 (recur (rest coll) (assoc mapping 1 (set number)))
          3 (recur (rest coll) (assoc mapping 7 (set number)))
          4 (recur (rest coll) (assoc mapping 4 (set number)))
          7 (recur (rest coll) (assoc mapping 8 (set number)))
          (recur (rest coll) mapping))))))

(defn sim [element mapping index]
  (count (clojure.set/intersection element (get mapping index))))

;; 2, 3 or 5
(defn extract-fives [digits base-mapping]
  (let [fives (->> digits
                   (filter #(= 5 (count %)))
                   (mapv #(set %)))]
    (loop [f fives
           mapping base-mapping]
      (if (empty? f)
        mapping
        (cond
          (and (= 3 (sim (first f) mapping 4)) (= 2 (sim (first f) mapping 1)))
          (recur (rest f) (assoc mapping 3 (first f)))
          (= 3 (sim (first f) mapping 4))
          (recur (rest f) (assoc mapping 5 (first f)))
          :else (recur (rest f) (assoc mapping 2 (first f))))))))

;; 0, 6 and 9
(defn extract-sixes [digits base-mapping]
  (let [sixes (->> digits
                   (filter #(= 6 (count %)))
                   (mapv #(set %)))]
    (loop [s sixes
           mapping base-mapping]
      (if (empty? s)
        mapping
        (cond
          (= 1 (sim (first s) mapping 1))
          (recur (rest s) (assoc mapping 6 (first s)))
          (= 4 (sim (first s) mapping 4))
          (recur (rest s) (assoc mapping 9 (first s)))
          :else (recur (rest s) (assoc mapping 0 (first s))))))))

(defn get-mapping [row]
  (let [digits (first row)]
    (->> digits
         (extract-base)
         (extract-fives digits)
         (extract-sixes digits)
         (into (sorted-map))
         (clojure.set/map-invert))))

(defn read-row [mapping numbers]
  (->> numbers
       (second)
       (mapv #(set %))
       (map #(get mapping %))
       (apply str)
       (Integer/parseUnsignedInt)))


(println (read-row (get-mapping fr) (second fr)))
(println (read-row (get-mapping fr) fr))

;; Second part solution.
(defn sum-digits [rows]
  (reduce
   #(+ %1 (read-row (get-mapping %2) %2))
   0 rows))

(def input (map
            (fn [row]
              (mapv #(str/split % #" ") row))
            (load-input "input.txt")))

(println (sum-digits input))

;; rules
;;
;; based on the length:
;; length -> number
;; 1 -> 2
;; 3 -> 7
;; 4 -> 4
;; 8 -> 7
;;
;; based on deduction:
;; length 5: [2, 3, 5]
;; 2-> contains ...
;; 3-> contains the two same letters as 1.
;; 5-> contains three same letters as 4.
;; length 6: [0, 6, 9]
;; 0-> contains same digits as 1 and 7 but not as 4
;; 6-> contains only one digit same as 1
;; 9-> contains all the same letters as 4
