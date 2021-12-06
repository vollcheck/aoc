(ns day06
  (:require [clojure.string :as str]))

(defn load-input [filename]
  (->> (-> filename
           (slurp)
           (str/trim)
           (str/split #","))
       (map #(Integer/parseUnsignedInt %))
       (into [])))

(defn run-day [fishes]
  (reduce
   (fn [c f]
     (if (= f 0)
       (apply conj c [6 8])
       (conj c (dec f))))
   [] fishes))

(defn run-days [start-fishes
                ^Integer days]
  (loop [fs start-fishes
         cnt 0]
    (if (= cnt days)
      (count fs)
      (recur (run-day fs) (inc cnt)))))

(def fishes (load-input "input.txt"))

(comment
  (println (str "First part: " (run-days fishes 80)))
  (println (str "Second part: " (run-days fishes 256)))
  )
