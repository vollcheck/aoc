(ns y22.day04
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(defn- parse-line [^String line]
  (mapv parse-long (str/split line #"[,-]")))

(defn sub? [^String line]
  (let [[fstart fend sstart send] (parse-line line)]
    (or
     (and (<= sstart fstart)
          (>= send fend))
     (and (>= sstart fstart)
          (<= send fend)))))

(defn overlap? [^String line]
  (let [[fstart fend sstart send] (parse-line line)]
    (or (<= sstart fstart send)
        (<= fstart sstart fend))))

(defn solve [input ff]
  (count (filter ff input)))

(comment
  (def test-input (load-in :test))
  (def input (load-in))

  ;; first part
  (solve input sub?)
   ;; => 571

  ;; second part
  (solve input overlap?)
   ;; => 917

  (require '[criterium.core :as criterium])
  (criterium/quick-bench (solve input sub?))
  (criterium/quick-bench (solve input overlap?))

  (require '[clj-async-profiler.core :as prof])
  (prof/profile (solve input sub?))
  (prof/profile (solve input overlap?))
  (prof/serve-ui 54321)
  )
