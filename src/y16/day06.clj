(ns y16.day06
  (:require [clojure.string :as str]))

;; NOTE: THIS IS FREAKING EASY DAY, CLOJURE ROCKS

(defn load-input
  ([]
   (load-input "input06.txt"))
  ([filename]
   (if (= :test filename)
     (str/split-lines (slurp "src/y16/input06-test.txt"))
     (str/split-lines (slurp (str "src/y16/" filename))))))

(defn transpose [m]
  (apply mapv vector m))

(->> (load-input :test)
     (transpose)
     (map (comp ffirst reverse (partial sort-by val) frequencies)) ;; haha, love clojure
     (apply str)
     )

(defn part-1 []
  (->> (load-input)
       (transpose)
       (map (comp ffirst
                  reverse
                  (partial sort-by val)
                  frequencies)) ;; haha, love clojure
       (apply str)))

(defn part-2 []
  (->> (load-input)
       (transpose)
       (map (comp ffirst
                  (partial sort-by val)
                  frequencies)) ;; haha, love clojure
       (apply str)))

(comment
  (part-1) ;; => "gyvwpxaz"
  (part-2) ;; => "jucfoary"
  )
