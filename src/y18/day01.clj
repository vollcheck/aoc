(ns y18.day01
  (:require [clojure.string :as str]
            [core :refer [load-input in?]]
            [purity :refer [defpure]]))

(let [input (load-input)]
  (-> input
      count))

(transduce
 (map parse-long)
 +
 0
 (load-input))

(defn part-1 [input]
  (transduce
   (map parse-long)
   +
   0
   input))


(get #{1 2 4} 1)


(defn part-2 [input]
  (reduce
   (fn [{:keys [v freqs]} n]
     (let [new-v (+ v (parse-long n))]
       (if (contains? freqs new-v)
         (reduced new-v)
         {:v new-v :freqs (conj freqs new-v)})))
   {:v 0 :freqs #{0}}
   (cycle input)))

(comment
  (def test-input (load-input :test))
  (def input (load-input))
  (part-1 test-input)
  (part-1 input) ;; => 525
  (part-2 test-input)
  (part-2 input) ;; => 75749
  )
