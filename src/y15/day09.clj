(ns y15.day09
  (:require [clojure.string :as str]))

;; TODO: UNFINISHED

(def dists
"London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")

(defn load-input []
  )

(defn parse-line [graph line]
  (let [line (str/split line #" ")
        [from _ to _ dist] line
        from (keyword from)
        to (keyword to)
        dist (Integer/parseUnsignedInt dist)
        existing (or (get graph from) {})]
    (assoc graph from (assoc existing to dist))))

(defn make-dists-graph
  [lines]
  (reduce parse-line {} lines))



(comment
  (def graph (make-dists-graph (str/split-lines dists)))

  (parse-line {} "London to Dublin = 464")
  (def lines (->> dists
                  (str/split-lines)
                  (map #(str/split % #" "))))
  lines
  )
