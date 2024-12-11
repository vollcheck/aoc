(ns y24.day11
  (:require [core :refer [parse-numbers load-in]]))

(defn update-stones [stones-map]
  (reduce
   (fn [acc [k v]]
     (let [f (fnil (partial + v) 0)]
       (if (= k 0)
         (update acc 1 f)

         (let [s (str k)
               len (.length ^String s)
               half (/ len 2)]
           (if (even? len)
             (let [power (Math/pow 10 half)
                   fi (int (quot k power))
                   se (int (rem k power))]
               (-> (update acc fi f)
                   (update fi f)))

             (update acc (* 2024 k) f))))))
   {}
   stones-map))

(defn solve [times in]
  (->> (parse-numbers in)
       frequencies
       (iterate update-stones)
       (take (inc times))
       last
       (transduce (map second) + 0)))

(comment
  (def intest (load-in :test :no-split))
  (def in (load-in :no-split))

  (solve 25 intest)
  ;; => 55312
  (solve 25 in)
  ;; => 186996

  (solve 75 in)
  ;; => 221683913164898
  )


(comment
  (require '[criterium.core :refer [quick-bench]])
  (quick-bench (solve 75 in))
   ;;           Execution time mean : 448.867523 µs
   ;;  Execution time std-deviation : 29.129477 µs
   ;; Execution time lower quantile : 432.083285 µs ( 2.5%)
   ;; Execution time upper quantile : 498.456758 µs (97.5%)
   ;;                 Overhead used : 9.478232 ns

  )
