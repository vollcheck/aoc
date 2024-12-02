(ns y24.day02
  (:require [clojure.string :as str]
            [core :refer [load-in]]))


(defn parse-line [line]
  (let [difs (->> (re-seq #"\d+" line)
                  (map parse-long)
                  (partition 2 1) ;; transducer
                  (map (fn [[x y]] (- x y))))]
    (cond
      (and (every? #(> 0 %) difs)
           (every? #(<= -3 % -1) difs))
      true

      (and (every? #(< 0 %) difs)
           (every? #(<= 1 % 3) difs))
      true

      :else false)
    #_difs))

(->> (load-in :test)
     ;; first
     ;; parse-line
     (map parse-line)
     (filter identity)
     count)
 ;; => 2
 ;; => (1 2 2 1)

(->> (load-in)
     (map parse-line)
     (filter identity)
     count)
 ;; => 572


(defn parse-line-2 [line]
  (let [difs (->> (re-seq #"\d+" line)
                  (map parse-long)
                  (partition 2 1) ;; transducer
                  (map (fn [[x y]] (- x y))))
        cd (count difs)
        cm (count (filter #(> 0 %) difs))
        cp (count (filter #(< 0 %) difs))
        res (cond
              ;; if there's more minuses
              (> cm cp)
              ;; is there
              (>= 1
                  (- cd
                     (count (filter #(<= -3 % -1) difs))))

              (< cm cp)
              ;; is there
              (>= 1
                  (- cd
                     (count (filter #(<= 1 % 3) difs))))

              :else false)]
    (println difs cd cm cp res)
    res
    ))

(->> (nth (load-in :test) 1)
     parse-line-2
     )

(->> (load-in :test)
     (map parse-line-2)
     (filter identity)
     count)
 ;; => 988
