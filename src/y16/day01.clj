(ns y16.day01
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(defn make-step [{:keys [x y face] :as position} step]
  (let [direction (first step)
        value (parse-long (subs step 1))]
    (case [face direction]
      [:north \L] (assoc position :x (- x value) :face :west)
      [:north \R] (assoc position :x (+ x value) :face :east)

      [:east  \L] (assoc position :y (+ y value) :face :north)
      [:east  \R] (assoc position :y (- y value) :face :south)

      [:south \L] (assoc position :x (+ x value) :face :east)
      [:south \R] (assoc position :x (- x value) :face :west)

      [:west  \L] (assoc position :y (- y value) :face :south)
      [:west  \R] (assoc position :y (+ y value) :face :north))))

(defn parse-input [lines]
  (str/split lines #", "))

(defn part-1 [input]
  (let [{:keys [x y]} (reduce make-step
                              {:x 0 :y 0 :face :north}
                              (parse-input input))]
    (+ x y)))

(defn make-step-2 [{:keys [x y face visited] :as position} step]
  (let [direction (first step)
        value (parse-long (subs step 1))
        [new-position changed]
        (case [face direction]
          [:north \L] [(assoc position :x (- x value) :face :west)  :x]
          [:north \R] [(assoc position :x (+ x value) :face :east)  :x]
          [:east  \L] [(assoc position :y (+ y value) :face :north) :y]
          [:east  \R] [(assoc position :y (- y value) :face :south) :y]
          [:south \L] [(assoc position :x (+ x value) :face :east)  :x]
          [:south \R] [(assoc position :x (- x value) :face :west)  :x]
          [:west  \L] [(assoc position :y (- y value) :face :south) :y]
          [:west  \R] [(assoc position :y (+ y value) :face :north) :y])

        newx (:x new-position)
        newy (:y new-position)
        steps-made (if (= changed :x)
                     (for [i (range (inc x) (inc newx))] [i y])
                     (for [i (range (inc y) (inc newy))] [x i]))
        match (not-empty (filter #(contains? visited %) steps-made))]
    (if match
      (reduced (first match))
      (update new-position :visited (partial apply conj visited) steps-made))))

(defn part-2 [input]
  (let [[x y] (reduce make-step-2
                      {:x 0 :y 0 :face :north :visited #{[0 0]}}
                      (parse-input input))]
    (+ (abs x) (abs y))))

(comment
  (def lines (str/trimr (load-in :no-split)))

  (part-1 lines)
   ;; => 226

  (part-2 "R8, R4, R4, R8")
  (part-2 lines)
   ;; => 79
  )

(comment
  (require '[criterium.core :as c])
  (c/quick-bench (part-1 lines))
  ;; Evaluation count : 11094 in 6 samples of 1849 calls.
  ;;            Execution time mean : 61.944484 µs
  ;;   Execution time std-deviation : 10.816185 µs
  ;;  Execution time lower quantile : 53.682600 µs ( 2.5%)
  ;;  Execution time upper quantile : 75.808250 µs (97.5%)
  ;;                  Overhead used : 5.786584 ns

  (c/quick-bench (part-1 lines))
   ;;           Execution time mean : 60.921657 µs
   ;;  Execution time std-deviation : 9.100106 µs
   ;; Execution time lower quantile : 53.338958 µs ( 2.5%)
   ;; Execution time upper quantile : 73.301887 µs (97.5%)
   ;;                 Overhead used : 5.786584 ns

  )
