(ns y18.day11
  (:require [clojure.string :as str]
            [core :refer [parallel-process parse-int]]))


;; this is one of the slowest days - find an option to make it faster!

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
        ;; ive changed that, so if it doesn't work, then that's the place
        ;; (mod 1000)
        ;; (quot 100)
        (quot 100)
        (mod 10)
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

(defn square-power-for-n
  "sum all values of 3x3 square given the top-left X and Y"
  [grid x y n]
  (->> (for [yy (range y (+ y n))
             xx (range x (+ x n))]
         (-> grid
             (nth yy)
             (nth xx)))
       (apply +)))

(defn get-square-powers-for-n [grid n]
  (for [column (range 0 (- height (dec n)))
        row (range 0 (- width (dec n)))]
    {:x (inc row) ;; because we are starting from 1 so the actual x is shifted!
     :y (inc column) ;; because we are starting from 1 so the actual y is shifted!
     :n n
     :power (square-power-for-n grid row column n)}))

(defn max-power [coll]
  (first (sort-by :power > coll)))

(defn part-2-pmap
  ([]
   (part-2 *serial*))
  ([serial]
   (binding [*serial* serial]
     (let [grid (generate-grid)

           ;; NOTE: that is heuristic, I saw that result raises till 15 and then decreases
           ;;       so I put 20 there for fast interations
           ;;       it turned out to be a good thing
           ;;       but I wonder if I can build an optimization around this
           ;;       maybe by putting some "watcher" that would tell if the tendency
           ;;       in window of 4 elements in raising or not
           ;;       if not, stop the calculations and find the local max
           {:keys [x y n power]}
           (->> (range 1 #_height 20)
                (pmap  ;; that isnt' right, it blocks whole bunch of threads until everyone
                       ;; finishes.
                 (fn [n]
                   (println "looking for max power of" n)
                   (let [result (max-power (get-square-powers-for-n grid n))]
                     (println "max power of" n "is" (:power result))
                     result)))
                max-power)]
       (println "best power:" power)
       (format "%s,%s,%s" x y n)))))

(defn part-2-es
  "same as part-2 above, but uses ExecutorService"
  ([] (part-2 *serial*))
  ([serial]
   (binding [*serial* serial]
     (let [grid (generate-grid)

           ;; NOTE: that is heuristic, I saw that result raises till 15 and then decreases
           ;;       so I put 20 there for fast interations
           ;;       it turned out to be a good thing
           ;;       but I wonder if I can build an optimization around this
           ;;       maybe by putting some "watcher" that would tell if the tendency
           ;;       in window of 4 elements in raising or not
           ;;       if not, stop the calculations and find the local max
           {:keys [x y n power]}
           (->> (range 1 #_height 20)
                (parallel-process
                 (fn [n]
                   (println "looking for max power of" n)
                   (let [result (max-power (get-square-powers-for-n grid n))]
                     (println "max power of" n "is" (:power result))
                     result))
                 6)
                max-power)]
       (println "best power:" power)
       (format "%s,%s,%s" x y n)))))

(comment
  (time (part-2-es))
  (def result (part-2))
  result
  ;; => "235,288,13"
  ;; => {:x 235, :y 288, :n 13, :power 147}


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
