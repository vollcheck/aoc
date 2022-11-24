(ns y15.day06
  (:require [clojure.string :as str]))

(defn load-input []
  (str/split-lines (slurp "src/y15/input06.txt")))

(defn make-grid
  ([n]
   (repeat n (vec (repeat n 0))))
  ([n value]
   (repeat n (vec (repeat n value)))))

(defn parse-line
  [line]
  (let [[end _ start instr _] (reverse (str/split line #" "))
        [sx sy] (map parse-long (str/split start #","))
        [ex ey] (map parse-long (str/split end #","))
        instr (keyword instr)]
    [instr sx sy ex ey]))

(defn action [instr]
  (case instr
    :on (constantly 1)
    :off (constantly 0)
    :toggle #(if (zero? %) 1 0)))

(defn action-2 [instr]
  (case instr
    :on (partial inc)
    :off #(max 0 (dec %)) ;; decrease, max to 0
    :toggle #(+ 2 %)))

(defn apply-instr
  [grid instr sx sy ex ey]
  (map-indexed
   (fn [idx row]
     (if (and (>= idx sy) (<= idx ey))
       (into [] (concat (subvec row 0 sx)
                        (map (action instr) (subvec row sx (inc ex)))
                        (subvec row (inc ex))))
       row))
   grid))

(defn apply-instr-2
  [grid instr sx sy ex ey]
  (map-indexed
   (fn [idx row]
     (if (and (>= idx sy) (<= idx ey))
       (into [] (concat (subvec row 0 sx)
                        (map (action-2 instr) (subvec row sx (inc ex)))
                        (subvec row (inc ex))))
       row))
   grid))

(defn count-turned-on
  [grid]
  (reduce
   (fn [agg row]
     (+ agg (reduce + row)))
   0
   grid))

(defn part-1 [grid instrs]
  (count-turned-on
   (reduce
    (fn [agg [instr sx sy ex ey]]
      (apply-instr agg instr sx sy ex ey))
    grid
    instrs)))

(defn part-2 [grid instrs]
  (count-turned-on
   (reduce
    (fn [agg [instr sx sy ex ey]]
      (apply-instr-2 agg instr sx sy ex ey))
    grid
    instrs)))

(comment
  (def grid (make-grid 1000 0))
  (def input (load-input))
  (def instrs (map parse-line input))
  (part-1 grid instrs) ;; => 543903
  (part-2 grid instrs) ;; => 14687245
  )
