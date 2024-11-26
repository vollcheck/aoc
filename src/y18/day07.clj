(ns y18.day07
  (:require [clojure.set :as set]
            [core :refer [disjv load-in split-by subvec? vcontains?]]))

(def ^:dynamic *debug* false)

(def line-rex
  #"Step (\w) must be finished before step (\w) can begin.")

(defn parse-line [line]
  (let [[_ a b] (re-matches line-rex line)]
    ;; it means that B is dependent on A
    [(keyword a) (keyword b)]))

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

(def sort-alphabetically (comp first name))

(defn prereqs
  "collects all keys that k is within it's dependents
  in other word get all keys where k is within vs"
  [graph k]
  (into []
        (comp
         (filter #(vcontains? (second %) k))
         (map first))
        graph))

(defn done-prereqs?
  "checks whether k has all dependencies in done collection so that means all of k's prerequisites are done"
  [graph done k]
  (let [pre (prereqs graph k)]
    (subvec? done pre)))

(defn walk-graph [graph]
  (loop [done []
         todos (find-starting-points graph)]
    (if (empty? todos)
      (apply str (map name done))
      (let [head (->> todos
                      (filter (partial done-prereqs? graph done))
                      (sort-by sort-alphabetically)
                      first)
            rest-todos (disjv head todos)
            deps (get graph head)]
        (if (nil? head)
          "somethings wrong!" ;; not needed really
          (recur (conj done head)
                 ;; set?
                 (set (apply conj rest-todos deps))))))))

(def char-times
  (->> (for [c (range 61 87)
             ;; magic number 4: we need to start from 61 which is \A in the code
             ;; \A int is 65, so 61+4=65
             :let [letter (keyword (str (char (+ c 4))))]]
         [letter c])
       (into {})))

(def initial-workers
  (->> (for [x (range 1 6)]
         {:id x
          :t 0
          :k nil})
       (into [])))

(defn dec-to-zero
  "decrement value, but stop on 0"
  [x]
  (max (dec x) 0))

(defn walk-graph-2 [graph]
  (loop [workers initial-workers
         time 0
         done []
         todos (find-starting-points graph)]
    (if (and (empty? todos)
             (every? #(nil? (:k %)) workers))
      (->> (map :t workers)
           (apply max)
           (+ time)
           dec) ;; because we had no more work to do so no need to inc at the end

      ;; TODO: optimization idea - if there's no more todos, then you can short-cicuit on busy workers by just subtracting
      ;; the max time of all busy workers

      (let [[free-workers busy-workers] (split-by #(= 0 (:t %)) workers)
            cnt (count free-workers)]

        ;; every worker busy, we can short-circuit on smaller value
        (if (= cnt 0)
          (let [min-t (apply min (map :t workers))
                new-workers (map (fn [w] (update w :t #(- % min-t))) workers)]
            (recur new-workers
                   (+ time min-t)
                   done
                   todos))

          (let [letters-done (keep :k free-workers)

                ;; put nil as a key for all free workers
                free-workers (map #(update % :k (constantly nil)) free-workers)

                ;; with addition of letters done
                all-done (set (apply conj done letters-done))

                ;; with addition of letters done dep
                all-todos (->> (mapcat #(get graph %) letters-done)
                               (apply conj todos)
                               set)

                picked-todos (->> all-todos
                                  (filter (partial done-prereqs? graph all-done))
                                  (sort-by sort-alphabetically)
                                  (take cnt))

                all-todos (remove #(vcontains? picked-todos %) all-todos)

                picked-todos-count (count picked-todos)
                [workers-to-engage workers-unemployed] (split-at picked-todos-count free-workers)

                newly-engaged-workers (map
                                       (fn [{:keys [id]} ch]
                                         {:id id
                                          :t (get char-times ch)
                                          :k ch})
                                       workers-to-engage
                                       picked-todos)

                new-workers (->> (concat busy-workers newly-engaged-workers workers-unemployed)
                                 (map (fn [w] (update w :t dec-to-zero))))]
            (if *debug*
              (println "new workers" new-workers))
            (recur new-workers
                   (inc time)
                   all-done
                   all-todos)))))))

(defn solve [input solve-fn]
  (->> input
       (mapv parse-line)
       to-graph
       solve-fn))

(defn part-1 [input]
  (solve input walk-graph))

(defn part-2 [input]
  (solve input walk-graph-2))

(comment
  (def test-data
    (->> (load-in :test)
         (mapv parse-line)))
  ;; => [[:C :A] [:C :F] [:A :B] [:A :D] [:B :E] [:D :E] [:F :E]]


  (part-1 (load-in :test))
  ;; => "CABDFE"

  (part-2 (load-in :test))
  ;; => 254


  (part-1 (load-in))
  ;; => "BETUFNVADWGPLRJOHMXKZQCISY"

  (part-2 (load-in))
  ;; => 848

  (def graph (to-graph test-data))
  graph
  ;; => {:C [:A :F], :A [:B :D], :B [:E], :D [:E], :F [:E]}

  (sort-by sort-alphabetically [:c :b :a])
  ;; => (:a :b :c)

  (prereqs graph :E)
  ;; => [:B :D :F]
  )
