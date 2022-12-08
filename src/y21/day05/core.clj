(ns day05
  (:require [clojure.string :as str]))


;; Loading input - preparing list of vectors contains points.

(defn load-lines [filename]
  (->> filename
       slurp
       str/split-lines
       (map (fn [line] ;; line procesing
              (->> (str/split line #" -> ")
                   (map #(str/split % #","))
                   (map (fn [v]
                          (->> v
                               (map #(Integer/parseUnsignedInt %))
                               (into []))))
                   (into []))))
       (into [])
       ))

(defn create-board
  "Create vector of vectors that represents the board."
  [w h]
  (->> (replicate w 0)
       (into [])
       (repeat h)
       (into [])))

(defn transpose [m]
  (apply mapv vector m))

(defn assoc-all
  [v ks]
  (reduce #(assoc %1 %2 (inc (nth %1 %2))) v ks))

(defn update-row [board [[x1 y1] [x2 y2]]]
  (let [row (nth board y1)]
    (assoc board y1
           (assoc-all row
                      (if (< x1 x2)
                        (range x1 (inc x2))
                        (range (inc x2) x1))))))

(defn update-col [board [xs ys]]
  (transpose (update-row (transpose board) [xs (into [] (reverse ys))])))

(defn process-instr [board [[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (update-col board [[x1 y1] [x2 y2]])
    (= y1 y2) (update-row board [[x1 y1] [x2 y2]])
    :else board))

(defn run-instrs [board instrs]
  (loop [b board i instrs]
    (if (empty? i)
      b
      (recur (process-instr b (first i)) (rest i)))))


(def sample (load-lines "sample.txt"))
(def s-board (create-board 10 10))

(clojure.pprint/pprint s-board)
(clojure.pprint/pprint (update-row s-board [[1 1] [3 1]]))
(clojure.pprint/pprint (update-col s-board [[1 1] [1 3]]))
(clojure.pprint/pprint (process-instr s-board [[9 4] [3 4]]))
(clojure.pprint/pprint (run-instrs s-board sample))
