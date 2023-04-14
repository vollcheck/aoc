(ns y19.day02
  (:require [core :refer [load-in]]
            [clojure.string :as str]))

(defn prepare-input [input]
  (mapv parse-long (str/split input #",")))

(defn manual-shift [input f s]
  (assoc input
         1 f
         2 s))

(defn process [input]
  (loop [idx 0
         prog input]
    (if (= (nth prog idx) 99) ;; instruction of 99 stops the program
      (subvec prog 0 3) ;; result of the program is value at 0 idx
      (let [[opcode arg1 arg2 dest] (subvec prog idx (+ idx 4))
            v1 (nth prog arg1)
            v2 (nth prog arg2)
            result (case opcode
                     1 (+ v1 v2)
                     2 (* v1 v2))]
          (recur
             (+ idx 4)
             (assoc prog dest result))))))

(defn first-part [input]
  (-> input
      (manual-shift 12 2)
      process
      first))

(defn second-part [input]
  ;; TODO: for sure we need to break from loop if the condition is met
  ;;       doseq might be an option though it returns nil
  (-> (for [x (range 100)]
        (for [y (range 100)
              :let [[result noun verb] (process (manual-shift input x y))]
              :when (= result 19690720)]
          (+ (* 100 noun) verb)))
      flatten
      first))

(comment
  (def input (prepare-input (load-in :no-split)))

  ;; first part
  (first-part input)
  ;; => 4576384

  ;; second part
  (second-part input)
   ;; => 5398

  (require '[criterium.core :refer [quick-bench]])
  (quick-bench (first-part input))
  ;; Execution time mean : 7.163985 µs
  (quick-bench (second-part input))
  ;; Execution time mean : 35.431940 ms
  )
