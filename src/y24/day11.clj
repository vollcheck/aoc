(ns y24.day11
  (:require [clojure.string :as str]
            [criterium.core :refer [quick-bench]]
            [core :refer [parse-numbers load-in]]))

(defn update-stones [stones-map]
  (->> stones-map
       (map
        (fn [[k v]]
          (if (= k 0)
            {1 v}

            (let [s (str k)
                  len (.length ^String s)
                  half (/ len 2)]
              (if (even? len)
                (let [power (Math/pow 10 half)
                      fi (int (quot k power))
                      se (int (rem k power))]
                  (merge-with + {fi v} {se v}))

                  {(* 2024 k) v})))))
       (apply merge-with +)))

(defn solve [times in]
  (->> (parse-numbers in)
       frequencies
       (iterate update-stones)
       (take (inc times))
       (last)
       (transduce (map second) +' 0)))

(comment
  (def intest "125 17")
  (def in "9759 0 256219 60 1175776 113 6 92833")

  (solve 25 intest)
  ;; => 55312
  (solve 25 in)
  ;; => 186996

  (solve 75 in)
  ;; => 221683913164898
  )
