(ns y23.day02
  (:require [core :refer [load-in]]
            [clojure.string :as str]))

(defn split-space [line]
  (str/split line #" "))

(defn not-exceeds? [{:keys [green blue red]
                     :or {green 0 blue 0 red 0}}]
  (and (<= red 12)
       (<= green 13)
       (<= blue 14)))

(defn parse-draw [draw]
  (->> (str/split draw #", ")
       (reduce (fn [acc cube]
                 (let [[number color] (split-space cube)]
                   (assoc acc (keyword color) (parse-long number))))
               {})
       not-exceeds?))

(defn parse-game [game]
  (let [[game-id draws] (str/split game #": ")
        good? (->> (str/split draws #"; ")
                   (map parse-draw)
                   (every? true?))]
    (when good?
      (-> (split-space game-id)
          second
          parse-long
          (or 0)))))

(defn part-1 [lines]
  (reduce
   #(+ %1 (or (parse-game %2) 0))
   0
   lines))

(defn parse-draw-2 [draw]
  (->> (str/split draw #", ")
       (reduce
        (fn [acc cube]
          (let [[number color] (split-space cube)]
            (assoc acc (keyword color) (parse-long number))))
        {})))

(defn max-color [color draw]
  (->> (keep color draw)
       (apply max)))

(def max-colors-fns
  (juxt (partial max-color :red)
        (partial max-color :green)
        (partial max-color :blue)))

(defn parse-game-2 [game]
  (let [[_ draws] (str/split game #": ")]
    (->> (str/split draws #"; ")
         (mapv parse-draw-2)
         (max-colors-fns)
         (apply *))))

(defn part-2 [lines]
  (transduce
   (map parse-game-2)
   +
   0
   lines))

(comment
  (def lines (load-in))

  (part-1 lines)
  ;; => 1734

  (part-2 lines)
  ;; => 70387
  )
