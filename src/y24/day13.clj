(ns y24.day13
  (:require [core :refer [parse-numbers load-in]]
            [clojure.string :as str]))

(defn parse-group [group]
  (let [[[bax bay] [bbx bby] [px py]] (map parse-numbers group)]
    [bax bay bbx bby px py]))

(defn find-solution [prize-enrich [bax bay bbx bby px py]]
  (let [px (+ px prize-enrich)
        py (+ py prize-enrich)
        d (- (* bax bby) (* bay bbx))
        a (/ (- (* px bby) (* bbx py)) d)
        b (/ (- (* bax py) (* px bay)) d)]
    (if (and (int? a) (int? b))
      (+ (* 3 a) b)
      0)))

(defn solve [prize-enrich in]
  (transduce (map (comp (partial find-solution prize-enrich)
                        parse-group
                        str/split-lines))
             + 0
             (str/split in #"\n\n")))


(comment
  ;; part 1
  (solve 0 (load-in :no-split))
  ;; => 38839

  (def adder 10000000000000)
  (solve adder (load-in :no-split))
   ;; => 75200131617108
  )
