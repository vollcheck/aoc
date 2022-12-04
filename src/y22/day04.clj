(ns y22.day04
  (:require [clojure.string :as str]
            [core :refer [load-input]]))

(defn sub? [line]
  (let [[f s] (str/split line #",")
        [fstart fend] (str/split f #"-")
        [sstart send] (str/split s #"-")
        [fstart fend sstart send] (map parse-long [fstart fend sstart send])]
    (or
     (and (<= sstart fstart)
          (>= send fend))
     (and (>= sstart fstart)
          (<= send fend)))))

(defn overlap? [line]
  (let [[f s] (str/split line #",")
        [fstart fend] (str/split f #"-")
        [sstart send] (str/split s #"-")
        [fstart fend sstart send] (map parse-long [fstart fend sstart send])]
    (or (<= sstart fstart send)
        (<= sstart fend send)
        (<= fstart sstart fend)
        (<= fstart send fend))))


(defn part-1 [input]
  (->> input
       (filter sub?)
       count
       ))

(defn part-2 [input]
  (->> input
       (filter overlap?)
       count
       ))

(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input) ;; => 2
  (part-1 input) ;; => 571
  (part-2 test-input) ;; => 4
  (part-2 input) ;; => 917
  )
