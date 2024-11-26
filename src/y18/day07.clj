(ns y18.day07
  (:require [clojure.set :as set]
            [core :refer [load-in]]))

(def line-rex
  #"Step (\w) must be finished before step (\w) can begin.")

(defn parse-line [line]
  (let [[_ a b] (re-matches line-rex line)]
    ;; it means that B is dependent on A
    (mapv keyword [a b])))

(defn find-starting-points [data]
  (let [ks (set (mapv first data))
        vs (set (mapcat second data))]
    (set/difference ks vs)))

(defn to-graph [data]
  (reduce
   (fn [acc [from to]]
     (if (contains? acc from)
       (update acc from conj to)
       (assoc acc from [to])))
   {}
   data))

(def sort-fn (comp first name))

(defn vcontains?
  "checks whether a seq contains an element k"
  [coll k]
  (some #(= % k) coll))

(defn prereqs
  "collects all keys that k is within it's dependents
  in other word get all keys where k is within vs"
  [graph k]
  (->> graph
       (filter #(vcontains? (second %) k))
       (map first)
       (into [])))

(defn subvec?
  "make sure all elements from subv are present in v"
  [v subv]
  (every? (partial vcontains? v) subv))

(defn done-prereqs? [graph done k]
  (let [pre (prereqs graph k)]
    (subvec? done pre)))

(defn vc-disj
  "remove item k from coll"
  [k coll]
  (remove #(= % k) coll))

(defn walk-graph [graph]
  (loop [done []
         todos (find-starting-points graph)]
    (if (empty? todos)
      (apply str (map name done))
      (let [head (->> todos
                      (filter (partial done-prereqs? graph done))
                      (sort-by sort-fn)
                      first)
            rest-todos (vc-disj head todos)
            deps (get graph head)]
        (if (nil? head)
          "somethings wrong!" ;; not needed really
          (recur (conj done head)
                 ;; set?
                 (set (apply conj rest-todos deps))))))))

;; precompute is faster?
(def char-times
  (->> (for [c (range 61 87)
             :let [letter (keyword (str (char (+ c 4))))]]
         [letter c])
       (into {})))


;; TODO: put that in the std lib
(defn update-vals [m f]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(def initial-workers
  (for [x (range 1 6)]
    {:id x
     :t 0
     :k nil}))

(defn dec-to-zero [x]
  (max (dec x) 0))

(defn walk-graph-2 [graph]
  (loop [workers initial-workers
         time 0
         done []
         todos (find-starting-points graph)]
    (if (empty? todos) ;; add max time to left and current time
      (->> (map :t workers)
           (apply max)
           (+ time))

      (let [free-workers (filter #(= 0 (:t %)) workers)
            cnt (count free-workers)

            done-from-workers (map :k free-workers)
            new-done (apply conj done done-from-workers)

            deps-from-done (set (mapcat #(get graph %) done-from-workers))

            ready-todos (->> todos
                             (filter (partial done-prereqs? graph done))
                             (sort-by sort-fn))

            todos-to-pick (take cnt ready-todos)

            todos-left    (set (apply conj
                                      (drop cnt ready-todos)
                                      deps-from-done))

            new-workers (map
                         (fn [{:keys [id t k]} ch]
                           (let [ch (keyword (str ch))]
                             {:id id
                              t (get char-times ch)
                              k ch}))
                         free-workers
                         todos-to-pick)
            ;; TODO
            ;; problem here is with keeping the count of workers!!!
            new-workers (->>
                         workers
                         (remove #(= 0 (:t %)))
                         (merge new-workers))
            _ (clojure.pprint/pprint new-workers)
            new-workers (map (fn [w] (update w :t dec-to-zero)) new-workers)]
        (recur new-workers
               inc
               new-done
               todos-left)))))

(map
 (fn [{:keys [id t k]} ch]
   (let [ch (keyword (str ch))]
     {:id id
      t (get char-times ch)
      k ch}))
 initial-workers
 [\A \B \C]
 )


(defn part-1 [input]
  (->> input
       (mapv parse-line)
       to-graph
       walk-graph))

(defn part-2 [input]
  (->> input
       (mapv parse-line)
       to-graph
       walk-graph-2))


(comment


  (def test-data
    (->> (load-in :test)
         (mapv parse-line)))
  ;; => [[:C :A] [:C :F] [:A :B] [:A :D] [:B :E] [:D :E] [:F :E]]


  (part-1 (load-in :test))
  ;; => "CABDFE"

  (part-2 (load-in :test))


  (time-for \A)
  (time-for \Z)
  (int \A)

  (part-1 (load-in))
   ;; => "BETUFNVADWGPLRJOHMXKZQCISY"
  ;; => "BEUVADNTFWGPLRJOHMXKZQCISY"
  ;; versus
  (= "BETUFNVADWGPLRJOHMXKZQCISY" ;; python solution
     "BETUFNVADWGPLRJOHMXKZQCISY")

  (def l (load-in))
  (def gg (->> (mapv parse-line l)
               data2graph))
  (count gg)

  (count (set (mapcat conj (mapv second gg))))
  (count (find-starting-points gg))
  (->> (find-starting-points gg)
       (sort-by sort-fn)
       first)

  (into [] )
  (walk-graph gg)
  ;; => "LMIROAFWQPDBJZTCEGYXHVUSNK"

  (find-starting-point gg)
  ;; => Execution error (IllegalArgumentException) at y18.day07/data2graph (REPL:39).
  ;;    Don't know how to create ISeq from: clojure.lang.Keyword

  ;; TESTS
  (to-graph test-data)
  ;; => {:C [:A :F], :A [:B :D], :B [:E], :D [:E], :F [:E]}

  (def graph (to-graph test-data))
  graph
  ;; => {:C [:A :F], :A [:B :D], :B [:E], :D [:E], :F [:E]}

  (sort-by sort-fn [:c :b :a])
  ;; => (:a :b :c)

  (vcontains? [:B :D :A] :B) ;; true
  (vcontains? [:B :D :A] :C) ;; nil

  (prereqs graph :E)
  ;; => [:B :D :F]

  (subvec? [:B :D :F :A] [:B :D :F]) ;; true
  (subvec? [:B :D] [:B :D :F]) ;; false


  (walk-graph graph)
  ;; => "CABDFE"



  )
