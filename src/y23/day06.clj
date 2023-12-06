(ns y23.day06
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(defn parse-input [lines]
  (->> (str/split-lines lines)
       (map
        (comp (partial map parse-long)
              (partial re-seq #"\d+")))
       (apply interleave)
       (partition 2)))

(defn count-ways [time distance]
  (->> (range time)
       (filter (fn [x]
                 (let [my-distance (* (- time x) x)]
                   (< distance my-distance))))
       count))

(defn part-1 [in]
  (reduce
   (fn [acc [time distance]]
     (* acc (count-ways time distance)))
   1
   in))

(defn parse-input-2 [in]
  (mapv
   (comp parse-long
         str/join
         #(re-seq #"\d+" %))
   (str/split-lines in)))

(defn part-2 [[time distance]]
  (count-ways time distance))

(comment
  (part-1 (parse-input (load-in :no-split)))
   ;; => 4811940

  (part-2 (parse-input-2 (load-in :no-split)))
   ;; => 30077773
  )
