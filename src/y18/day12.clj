(ns y18.day12
  (:require [clojure.string :as str]
            [core :refer [load-in]]))

(defn parse-in [in]
  {:state (-> in first
              (str/split #": ")
              second
              put-empty-deque)
   :rules (->> (drop 2 in)
               (map #(str/split % #" => "))
               (into {}))})

(defn departition [coll]
  ;; NOTE: it work only if the step is smaller than the collection count
  (loop [c coll
         acc []]
    (if (= 1 (count c))
      (apply conj acc (first c))
      (recur (rest c)
             (conj acc (ffirst c))))))

(defn extend-state-with-dots [state]
  ;; is that right?
  (let [h (not (str/starts-with? state ".."))
        l (not (str/ends-with? state ".."))]
    (cond
      (and h l) (str ".." state "..")
      h (str ".." state)
      l (str ".." state ".."))))

(defn run-gen [rules state]
  ;; also make sure that every output from this function is surrounded with two '.' dots on each side
  (->> (str "......." state ".......")
       (partition 5 1)
       (map (fn [pot]
              (let [pot (apply str pot)]
                (get rules pot \.)))) ;; to change, my output will contain all possible combinations
       (apply str)
       #_extend-state-with-dots
       ))

(comment
  (def in (load-in :test))
  (def x (parse-in in))
  (def rules (:rules x))
  (def state (:state x))

  (->> (iterate (partial run-gen rules) state)
       (take 21)
       last
       (filter #(= % \#))
       count)
   ;; => "...##...##...##..#..#..#..#.."

  )
