(ns y24.day07
  (:require [core :refer [load-in]]))

(defn repeated-combo [items n]
  (if (zero? n)
    '(())
    (for [item items
          subcomb (repeated-combo items (dec n))]
      (cons item subcomb))))

;; TODO: how about use lazy list instead of defering all symbols?
(def signs (partial repeated-combo ['+ '*]))

(defn redu [nums ops]
  (loop [res (first nums)
         nums (rest nums)
         ops ops]
    (if (empty? nums)
      res
      (let [num (first nums)
            op (first ops)]
        (recur ((eval op) res num)
               (rest nums)
               (rest ops))))))

(defn pass? [signs line]
  (let [[expected & numbers]
        (map parse-long (re-seq #"\d+" line))
        cnt (dec (count numbers))
        combi (signs cnt)
        results (map #(redu numbers %) combi)]
    ;; (prn expected)
    ;; (prn numbers)
    ;; (prn combi)
    ;; (prn results)
    (when (some #(= expected %) results)
      expected)))

(defn solve [signs in]
  (transduce
   (keep (partial pass? signs))
   + 0 in))

(def part-1 (partial solve signs))

(defn cat-nums [x y]
  (parse-long (str x y)))

(def signs2 (partial repeated-combo ['+ '* 'cat-nums]))


(def part-2 (partial solve signs2))

(comment
  (def intest (load-in :test))
  (def in (load-in))
  (pass? (first intest))

  (part-1 intest)
  ;; => 3749
  "Elapsed time: 3906.580899 msecs"
  (time (part-1 in))
   ;; => 12839601725877

  (part-2 intest)
  ;; => 11387
  "Elapsed time: 206512.486194 msecs"
  (time (part-2 in))
   ;; => 149956401519484


  )
