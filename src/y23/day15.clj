(ns y23.day15
  (:require [clojure.string :as str]
            [core :refer [load-in drop-at]]))

(defn parse-in [lines]
  (-> (str/trim lines)
      (str/split #"\,")))

(defn algo [s]
  (reduce
   (fn [acc c]
     (rem (* 17 (+ acc (int c))) 256))
   0
   s))

(defn part-1 [lines]
  (transduce (map algo) + 0 lines))

(defn compute-power [boxes]
  (->> boxes

       ))

(defn part-2 [lines]
  (->> lines
       (reduce
        (fn [boxes line]
          (let [[label op focal] (str/split line #"(?=[\-\=])|(?<=[\-\=])")
                idx (algo label)
                box (get boxes idx [])
                indexed-box (zipmap (range) box)
                lens-idx (some
                          (fn [[nidx [nlabel _nfocal]]]
                            (when (= nlabel label)
                              nidx))
                          indexed-box)
                focal (when focal (parse-long focal))]
            (if (= op "=")
              (if lens-idx
                (assoc-in boxes [idx lens-idx 1] focal)
                (assoc boxes idx (conj box [label focal])))
              (if lens-idx
                (assoc boxes idx (drop-at lens-idx box))
                boxes))))
        {})
       (remove (fn [[_ coll]] (empty? coll)))
       (reduce-kv
        (fn [acc box-idx lenses]
          (->> lenses
               (map-indexed (fn [idx [_ focal]] (* (inc box-idx)
                                                  (inc idx)
                                                  focal)))
               (reduce + 0)
               (+ acc)))
        0)))

(comment
  (def tlines (parse-in "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"))
  (def lines (parse-in (load-in :no-split)))

  (part-1 tlines)
  ;; => 1320
  (part-1 lines)
   ;; => 508552

  (part-2 tlines)
   ;; => 145

  (part-2 lines)
   ;; => 265462
  )
