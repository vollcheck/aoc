(ns y23.day04
  (:require [core :refer [load-in]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn ->splitted-set [coll]
  (-> (str/trim coll)
      (str/split #"\s+")
      set))

(defn nums->power [nums]
  (->> (str/split nums #"\| ")
       (map ->splitted-set)
       (apply set/intersection)
       count))

(defn parse-game [line]
  (let [[_game numbers] (str/split line #": ")
        power (nums->power numbers)]
    (case power
      0 0
      1 1
      (Math/pow 2 (dec power)))))

(defn part-1 [lines]
  (int (transduce
        (map parse-game)
        +
        0
        lines)))

(defn parse-game-2 [line]
  (let [[card numbers] (str/split line #": ")
        card-id (parse-long (re-find #"\d+" card))
        power (nums->power numbers)]
    {card-id {:amount 1
              :numbers (vec (range (inc card-id) (+ (inc card-id) power)))}}))

(defn update-amount [acc amount numbers]
  (reduce
   (fn [acc number]
     (update-in acc [number :amount] + amount))
   acc
   numbers))

(defn part-2 [lines]
  (let [parsed-games (into {} (map parse-game-2) lines)]
    (->> (loop [acc parsed-games ;; NOTE: refactor it using double-reduce
                idx 1]
           (if-not (contains? acc idx)
             acc
             (let [{:keys [amount numbers]} (get acc idx)]
               (recur (update-amount acc amount numbers)
                      (inc idx)))))
         (transduce
          (map (comp :amount second))
          +
          0))))

(comment
  (def tlines (load-in :test))
  (def lines (load-in))
  (time (part-1 lines))
  ;; => 21919

  (time (part-2 tlines))
  ;; "Elapsed time: 1.254541 msecs"
  ;; => 30
  (time (part-2 lines))
  ;;"Elapsed time: 7.913264 msecs"
  ;; => 9881048
  )
