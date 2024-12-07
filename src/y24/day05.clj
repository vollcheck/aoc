(ns y24.day05
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

;; TODO: candidate for `core` namespace
(defn get-digits [s]
  (map parse-long (re-seq #"\d+" s)))

;; TODO: candidate for `core` namespace
(defn div [x y]
  (int (/ x y)))

;; TODO: candidate for `core` namespace
(defn departition [pairs]
  (into [(ffirst pairs)]
        (map second)
        pairs))

(defn parse-coll [coll]
  (->> (str/split-lines coll)
       (map get-digits)))

;; NOTE: that is bruteforced, it would be better to prepare a graph of dependencies upfront
(defn ordered-pair? [deps [x y]]
  (->> deps
       (filter #(= x (first %)))
       (some #(= y (second %)))))

(defn ordered-line? [deps line]
  (every? (partial ordered-pair? deps) (partition 2 1 line)))


(defn get-middle [line]
  (let [middle (div (count line) 2)]
    (nth line middle)))

(defn part-1 [in]
  (let [[deps ordering] (map parse-coll (str/split in #"\n\n"))]
    (transduce
     (comp (filter (partial ordered-line? deps))
           (map get-middle))
     + 0
     ordering)))

(defn make-ordered [deps line]
  (loop [line line
         res []]
    (if (= 1 (count line))
      (let [output (conj res (first line))]
        (if (ordered-line? deps output)
          output
          (make-ordered deps output)))
      (let [h (first line)
            t (second line)]
        (if (ordered-pair? deps [h t])
          (recur (rest line)
                 (conj res h))
          (recur (into [h] (drop 2 line)) ;; append to the beginning
                 (conj res t)))))))

(defn part-2 [in]
  (let [[deps ordering] (map parse-coll (str/split in #"\n\n"))]
    (transduce
     (comp (remove (partial ordered-line? deps))
           (map (partial make-ordered deps))
           (map get-middle))
     + 0
     ordering)))

(comment
  (def intest (load-in :test :no-split))
  (def in (load-in :no-split))
  (part-1 intest)
  (part-1 in)
  ;; => 6267

  (part-2 intest)
 ;; => 123
  (part-2 in)
 ;; => 5184

  )
