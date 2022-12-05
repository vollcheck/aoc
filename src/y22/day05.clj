(ns y22.day05
  (:require [clojure.string :as str]
            [core :refer [load-input positions split-by-space transpose]]))

(defn parse-crates [crates]
  (let [crates (str/split-lines crates)
        containers (butlast crates)
        indexes (last crates)
        indexes (positions #(Character/isDigit %) indexes)
        indexes-count (range 1 (inc (count indexes)))
        conts (for [cont containers]
                (for [idx indexes]
                  (nth cont idx \space)))
        proc-cont (fn [cont] (filterv #(Character/isUpperCase %) (reverse cont)))]
    (->> conts
         transpose
         (map proc-cont)
         (zipmap indexes-count))))

(defn parse-lines [lines]
  (map (fn [line]
         (let [[_ amount _ from _ to] (split-by-space line)
               [amount from to] (map parse-long [amount from to])]
           {:amount amount :from from :to to}))
       (str/split-lines lines)))

(defn move [state from to]
  (let [from-container (get state from)
        to-container (get state to)
        element (last from-container)
        from-after (pop from-container)
        to-after (conj to-container element)]
    (-> state
        (assoc from from-after)
        (assoc to to-after))))

(defn eval-op [state {:keys [amount from to]}]
  (loop [i amount
         state state]
    (if (<= i 0)
      state
      (recur (dec i)
             (move state from to)))))


(defn eval-op-2 [state {:keys [amount from to]}]
  (let [from-container (get state from)
        to-container (get state to)
        elements (->> from-container
                      (take-last amount)
                      (into []))
        from-after (->> from-container
                        (drop-last amount)
                        (into []))
        to-after (into to-container elements)]
    (-> state
        (assoc from from-after)
        (assoc to to-after))))

(defn part-1 [input]
  (let [[crates ops] (str/split input #"\n\n")
        crates (parse-crates crates)
        ops (parse-lines ops)]
    (->> ops
         (reduce eval-op crates)
         (sort-by key)
         (map (comp last second))
         (apply str)
         )))

(defn part-2 [input]
  (let [[crates ops] (str/split input #"\n\n")
        crates (parse-crates crates)
        ops (parse-lines ops)]
    (->> ops
         (reduce eval-op-2 crates)
         (sort-by key)
         (map (comp last second))
         (apply str)
         )))

(comment
  (def test-input (load-input :test :no-split))
  (def input (load-input :no-split))
  (part-1 test-input) ;; => "CMZ"
  (part-1 input) ;; => "MQTPGLLDN"
  (part-2 test-input) ;; => "MCD"
  (part-2 input) ;; => "LVZPSTTCZ"
  )
