(ns y24.day04
  (:require [core :refer [load-in]]))

(defn get2 [grid x y]
  (-> (nth grid y)
      (nth x)))

(defn find-xes [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= \X (get2 grid x y))]
    [x y]))

(defn count-xmas-diagonals [grid [x y]]
  (let [max-y (count grid)
        max-x (count (first grid))]
    (->> (for [nx [-1 0 1]
               ny [-1 0 1]]
           (->> (range 1 4)
                (keep (fn [n]
                        (let [xx (+ x (* nx n))
                              yy (+ y (* ny n))]
                          (when (and (< -1 xx max-x)
                                     (< -1 yy max-y))
                            (get2 grid xx yy)))))
                (apply str)
                (= "MAS")))
         (filter identity)
         count)))

(comment
  (let [g (load-in)
        xes (find-xes g)
        f (partial count-xmas-diagonals g)]
    (transduce (map f) + 0 xes))

  (count-xmas-diagonals testing (find-xes testing))
  ;; => 8
  (let [g (load-in :test)
        xes (find-xes g)
        f (partial count-xmas-diagonals g)]
    (transduce (map f) + 0 xes))
  ;; => 18


   ;; => 2434


  )

(defn find-as [grid]
  (for [y (range (count grid))
        x (range (count (first grid)))
        :when (= \A (get2 grid x y))]
    [x y]))


(defn count-mas-diagonals [grid [x y]]
  (let [max-y (count grid)
        max-x (count (first grid))]
    (when (and (< 0 x (dec max-x))
               (< 0 y (dec max-y)))
      (let [f (partial get2 grid)
            topleft  (f (dec x) (dec y))
            topright (f (inc x) (dec y))
            botleft  (f (dec x) (inc y))
            botright (f (inc x) (inc y))
            p1 (set [topleft botright])
            p2 (set [topright botleft])]
        (= #{\M \S} p1 p2)))))

(comment
  (part-2 intest3)
  (part-2 (load-in))
  ;; => 1835

  )


(defn part-2 [in]
  (->> (find-as in)
       (map (partial count-mas-diagonals in))
       (filter identity)
       count))

(comment
  (part-2 ["SSM"
           "SAS"
           "SSM"])


  (part-2 intest3)
   ;; => 21, it should be 9!

  (part-2 intest2)
  ;; => 9
  (part-2 (load-in))
  ;; => 3131

  ;;             0123456
  (def testing ["S..S..S" ;; 0
                ".A.A.A." ;; 1
                "..MMM.." ;; 2
                "SAMXMAS" ;; 3
                "..MMM.." ;; 4
                ".A.A.A." ;; 5
                "S..S..S"]); 6
  )

(comment
  (find-xes testing)
  (part-1 testing)
  )
 ;; => (()
;;     ([\. [2 0]] [\. [1 0]] [\S [0 0]])
;;     ([\. [2 1]] [\. [1 2]] [\S [0 3]])
;;     ()
;;     ([\. [3 0]] [\. [3 0]] [\. [3 0]])
;;     ([\. [3 1]] [\M [3 2]] [\X [3 3]])
;;     ()
;;     ([\. [4 0]] [\. [5 0]] [\S [6 0]])
;;     ([\. [4 1]] [\. [5 2]] [\S [6 3]]))



(defn part-1 [in]
  (transduce (map (partial create-diagonals intest))
             +
             0
             (find-xes in)))




(comment

  (->> (mapcat (partial find-next intest) xes #_[(first xes)])
       (mapcat (fn [fromx] (reduce #(into %1 %2) #{} fromx)))
       )

  (def intest (load-in :test))
  (def xes (find-xes intest))

  (count intest)
  ;; => 10
  (count (first intest))
   ;; => 10
  (map #(count-xmas-diagonals intest %) xes)
  (find-next intest [\X [[5 0]]])
   ;; => ([\M [[5 0] [5 1]]] [\M [[5 0] [6 0]]])

  )
