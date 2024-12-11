(ns y24.day11
  (:require [core :refer [parse-numbers load-in]]))

(defn blink [stones]
  (reduce
   (fn [acc stone]
     (if (= 0 stone)
       (conj acc 1)

       ;; if stone is even
       (let [s (str stone)
             len (.length ^String s)
             half (/ len 2)
             power (Math/pow 10 half)]
         (if (even? len)
           (conj acc
                 (int (quot stone power))
                 (int (rem stone power)))

           ;; otherwise
           (conj acc (* stone 2024))))))
   []
   stones))

(defn part-1 [times in]
  (->> (parse-numbers in)
       (iterate blink)
       (take (inc times))
       last
       count))

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

(defn part-2 [times in]
  (->> (parse-numbers in)
       frequencies
       (iterate update-stones)
       (take (inc times))
       (last)
       (transduce (map second) +' 0)))

(comment
  (def blinks 25)
  (def intest "125 17")
  (def in "9759 0 256219 60 1175776 113 6 92833")
  (def in2 "9759 0 256219 60 1175776 113 6 92833")

  (part-1 25 intest)
  ;; => 55312
  (part-1 25 in)
  ;; => 186996

  (part-2 75 in)
  ;; => 221683913164898
  )
