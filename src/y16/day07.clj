(ns y16.day07
  (:require [clojure.string :as str]))

(defn load-input
  ([]
   (load-input "input07.txt"))
  ([filename]
   (if (= :test filename)
     (str/split-lines (slurp "src/y16/input07-test.txt"))
     (str/split-lines (slurp (str "src/y16/" filename))))))

(defn valid-abba? [s]
  (->> (partition 4 1 s)
       (map (fn [[a1 b1 b2 a2]]
              (and (not= a1 b1)
                   (= a1 a2)
                   (= b1 b2))))
       (some true?)))

(defn valid-tls? [tls]
  (let [[s1 hypernet s2] (str/split tls #"[\[\]]")] ;; FIXME: there could be many hypernet sequences
    (and (or (valid-abba? s1)
             (valid-abba? s2))
         (not (valid-abba? hypernet)))))

(defn part-1 [input]
  (->> input
       (filter valid-tls?)
       count))

(defn part-2 [input]
  ())

(comment
  (part-1 (load-input)) ;; => 84
  (part-2 (load-input))
  )
