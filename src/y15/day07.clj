(ns y15.day07
  (:require [clojure.string :as str]))

;; UNFINISHED

(def -input
  "123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i")

(defn load-input
  ([] (load-input "input07.txt"))
  ([filename] (str/split-lines (slurp (str "src/y15/" filename)))))

(defn parse-line [state line]
  (let [data (str/split line #" ")
        _ (prn state)
        typ (count data)]
    (case typ
      ;; standard assignment
      3 (let [[value _ to-reg] data
              value (Integer/parseInt value)
              _ (when-not (int? value)
                  (throw (AssertionError.
                          (str "Value in register is not a number! " value))))]
          (assoc state to-reg value))

      ;; bitwise complementation
      4 (let [[not-gate from-reg _ to-reg] data
              _ (when-not (= not-gate "NOT")
                  (throw (AssertionError. "I thought 4-worded instruction is NOT.")))
              value (bit-and-not (get state from-reg) 16rFFFF)]
          (assoc state to-reg value))

      ;; other bitwise gates
      5 (let [[from-reg gate from-reg-or-value _ to-reg] data
              value (get state from-reg)]
          (assoc state
                 to-reg
                 (case gate
                   "AND" (bit-and value (get state from-reg-or-value))
                   "OR" (bit-or value (get state from-reg-or-value))
                   "LSHIFT" (bit-shift-left value (Integer/parseInt from-reg-or-value))
                   "RSHIFT" (bit-shift-right value (Integer/parseInt from-reg-or-value))))))))

(defn part-1 [lines]
  (get (reduce parse-line {} lines) "a"))

(comment
  (def s "v\xfb\"lgs\"kvjfywmut\x9cr")
  (str/escape "v\xfb\"lgs\"kvjfywmut\x9cr")
  (println (str/escape s char-escape-string))


  (part-1 (load-input "input07.txt"))
  (load-input "test-input07.txt")
  (def raw-lines ["123 -> x"
                  "456 -> y"
                  "x AND y -> d"
                  "x OR y -> e"
                  "x LSHIFT 2 -> f"
                  "y RSHIFT 2 -> g"
                  "NOT x -> h"
                  "NOT y -> i"])
  (reduce parse-line {} raw-lines)
  (. clojure.lang.Numbers unsignedShiftRight x n)
  (. clojure.lang.Numbers unsignedShiftLeft 123 2)
  (bit-shift-left 123 2)
  (bit-and-not 16rFFFF 456) ;; => 65079
  (bit-and-not 16rFFFF 123) ;; => 65412
  (bit-not 2r100)
  (str "2r" (Integer/toBinaryString 123)) ;; => "2r1111011"
  (str "2r" (Long/toBinaryString 123)) ;; => "2r1111011" ;; => "2r1111011"
  (bit-not "1111011")
  (.not (biginteger 123))
  (.not (long 123))
  (. (int 123) ~)
  (bit-shift-left 123 2)
  (def input (load-input "test-input07.txt"))
  (part-1 input)
  )
