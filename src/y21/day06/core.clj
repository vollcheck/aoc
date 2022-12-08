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

(defn run-day2 [fishes]
  (frequencies fishes))

;; (run-day2 small-fishes)

(defn run-days [start-fishes
                ^Integer days]
  (loop [fs start-fishes
         cnt 0]
    (if (= cnt days)
      (count fs)
      (recur (run-day fs) (inc cnt)))))

(def fishes (load-input "input.txt"))
(def small-fishes (load-input "sample.txt"))

(comment
  (println (str "First part: " (run-days fishes 80)))
  (println (str "Second part: " (run-days fishes 256)))
  )

;; So rather than maintaining a list of each individual fish you can use 'frequencies' on the initial list, then each step is just updating the frequencies (and adding new fish when appropriate) .

;; My results return immediately that way.

;; I probably need to clean my code up a bit since I wrote it in the middle of the night but that method will be much more efficient.
