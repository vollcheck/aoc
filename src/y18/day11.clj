(ns y18.day11
  (:require [clojure.string :as str]
            [core :refer [parse-int]]))

(def ^:dynamic *serial* 7511)

(def width 300)
(def height 300)

;; !!!
;; NOTE: they started with index 1, not 0!
;; !!!

(defn get-power [x y]
  (let [rack (+ x 10)]
    (-> rack
        (* y)
        (+ *serial*)
        (* rack)
        (mod 1000) ;; get hundreds
        (quot 100) ;; get hundreds
        (- 5))))

(defn generate-grid []
  (->> (for [y (range 1 (inc height))]
         (into [] (for [x (range 1 (inc width))]
                    (get-power x y))))
       (into [])))

(defn square-power
  "sum all values of 3x3 square given the top-left X and Y"
  [grid x y]
  (->> (for [yy (range y (+ y 3))
             xx (range x (+ x 3))]
         (-> grid
             (nth yy)
             (nth xx)))
       (apply +)))

(defn get-square-powers [grid]
  (for [column (range 0 (- height 2))
        row (range 0 (- width 2))]
    {:x (inc row) ;; because we are starting from 1!
     :y (inc column) ;; because we are starting from 1!
     :power (square-power grid row column)}))

(defn part-1
  ([]
   (part-1 *serial*))
  ([serial]
   (binding [*serial* serial]
     (->> (generate-grid)
          (get-square-powers)
          (sort-by :power >)
          first))))

;; part-2

(defn get-square-powers-for-n [grid n]
  (for [column (range 0 (- height (dec n)))
        row (range 0 (- width (dec n)))]
    {:x (inc row) ;; because we are starting from 1!
     :y (inc column) ;; because we are starting from 1!
     :power (square-power grid row column)}))

(comment
  (part-1 18)
  ;; => {:x 33, :y 45, :power 29}

  (part-1 42)
  ;; => {:x 21, :y 61, :power 30}

  (part-1 7511)
  ;; => {:x 21, :y 22, :power 34}

  (def grid (generate-grid))
  (count grid) ;; => 300
  (count (first grid)) ;; => 300

  (def sq-powers (get-square-powers grid))

  (= (count sq-powers) (* 298 298))
  ;; => true

  (count sq-powers)
  ;; => 298
  (count (first sq-powers))
  ;; => 298
  (sort-by identity > [2 4 1 5])

  (-> grid first count )
  ((juxt first last) (range 0 298))

  (count square-powers)
  ;; => 299
  (count (first square-powers))
  ;; => 288
  (take 3 (sort-by :power square-powers))

  (require '[clojure.test :refer [deftest are]])

  (deftest get-power-test
    (are [serial coords expected]
        (binding [*serial* serial]
          (= (get-power coords) expected))
      8  [3 5]     4
      57 [122 79]  -5
      39 [217 196] 0
      71 [101 153] 4))


  )
