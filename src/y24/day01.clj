(ns y24.day01
  (:require [clojure.string :as str]
            [core :refer [parse-int load-in]]))

(defn parse-pair [line]
  (->> (str/split line #"   ")
       (mapv parse-int)))

(defn part-1 [in]
  (let [lines (map parse-pair in)
        xs (sort (map first lines))
        ys (sort (map second lines))]
    (->> (map (comp abs -) xs ys)
         (apply +))))

(defn part-2 [in]
  (let [lines (map parse-pair in)
        xs (map first lines)
        ys (map second lines)
        freq (frequencies ys)]
    ;; (println freq)
    ;; (println xs)
    (->> (map #(* % (get freq % 0)) xs)
         (apply +))))

(comment
  (part-2 (load-in))
   ;; => 18650129
  (part-2 (load-in :test))

  (part-1 (load-in))
   ;; => 2904518
  (part-1 (load-in :test))
   ;; => 11
  )
(apply + )
