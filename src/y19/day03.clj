(ns y19.day03
  (:require [clojure.string :as str]
            [clojure.set :refer [intersection]]
            [core :refer [load-input] :as core]))

(defn parse-step [{:keys [x y pos]} step]
  (let [dir (first step)
        n (parse-long (subs step 1))]
    (case dir
      \R {:x (+ x n)
          :y y
          :pos (into pos
                     (for [i (range 1 (inc n))]
                       [(+ x i) y]))}
      \L {:x (- x n)
          :y y
          :pos (into pos
                     (for [i (range 1 (inc n))]
                       [(- x i) y]))}
      \U {:x x
          :y (+ y n)
          :pos (into pos
                     (for [i (range 1 (inc n))]
                       [x (+ y i)]))}
      \D {:x x
          :y (- y n)
          :pos (into pos
                     (for [i (range 1 (inc n))]
                       [x (- y i)]))})))

(def initial {:x 0 :y 0 :pos []})

(defn nth-wire [n in]
  (reduce
   parse-step
   initial
   (str/split (get in n) #",")))

(defn part-1 [input]
  (let [pos1 (set (:pos (nth-wire 0 input)))
        pos2 (set (:pos (nth-wire 1 input)))]
    (->> (intersection pos1 pos2)
         (mapv #(mapv abs %))
         (mapv #(apply + %))
         (apply min))))

(defn indexed-intersection [s1 s2]
  #_{:clj-kondo/ignore [:unused-binding]}
  (reduce-kv (fn [result item idx]
               (if (contains? s2 item)
	         result
                 (dissoc result item)))
	     s1 s1))

(defn part-2 [input]
  (let [pos1 (zipmap (:pos (nth-wire 0 input)) (core/range-1))
        pos2 (zipmap (:pos (nth-wire 1 input)) (core/range-1))]
    (->> (merge-with +
                     (indexed-intersection pos1 pos2)
                     (indexed-intersection pos2 pos1))
         (vals)
         (apply min))))

(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input) ;; => 192
  (part-1 input) ;; => 232
  (part-2 test-input) ;; => 610
  (part-2 input) ;; => 6084
  )
