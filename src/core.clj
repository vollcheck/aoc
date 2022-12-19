(ns core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-int
  [x]
  (if (char? x)
    (Character/digit x 10)
    (Integer/parseInt x)))

(defn parse-uint
  [x]
  (Integer/parseUnsignedInt x))

(defn split-by-space [s]
  (str/split s #" "))

(defn in?
  [e coll]
  (some #(= e %) coll))

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.
  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))

;; TODO: make it working
(defn scanf [re s]
  (let [list-splitters (remove empty? (str/split (.pattern re) #"\\\w\+"))
        list-words (re-seq #"\\w\+" (.pattern re))
        list-re (map re-pattern (map #(.concat %1 %2) list-words list-splitters))
        list-matches (map #(re-find % s) list-re)]
    (map #(if (nil? %1) ""
              (first (str/split %1 %2)))
         list-matches (map re-pattern list-splitters))))

(def split-by
  ^{:doc
    "Usage:
  (split-by pos? [-1 -2 4 5 3 -9]) ;; => [(4 5 3) (-1 -2 -9)]"}
  (juxt filter remove))

(defn v-index-of
  "Like `clojure.string/index-of` but applicable to the general collection"
  [x coll]
  (let [idx? (fn [i a]
               (when (= x a)
                 i))]
    (first (keep-indexed idx? coll))))

(defn v-indexes-of
  ""
  [x coll]
  (let [idx? (fn [i a]
               (when (= x a)
                 i))]
    (keep-indexed idx? coll)))

(defn find-idx [f values]
  (->> values
       (map-indexed vector)
       (apply f second)
       (first)))

(def max-val-idx (partial find-idx max-key))
(def min-val-idx (partial find-idx min-key))

(defn load-input
  "Return data for given day using namespace that this fn
  is being called from.

  Possible options:
  - :test - if you want to load test data
  - :no-split - return only slurped stream, do not split by new line
  - :debug - to see passed options
  "
  ([] (load-input []))
  ([& opts]
   (when (in? :debug opts) (prn opts))
   (let [[y d] (str/split (str *ns*) #".day")

         filename (if (in? :test opts)
                    "src/%s/input%s-test.txt"
                    "src/%s/input%s.txt")
         full-filename (format filename y d)]
     (try
       (if (in? :no-split opts)
         (slurp full-filename)
         (str/split-lines (slurp full-filename)))
       (catch java.io.FileNotFoundException _
         (println "no test file available.\n"
                  "are you sure you want to load test data?")
         :not-ok)))))

(defn gcd [x y]
  (cond
    (= x y) x
    (> x y) (gcd (- x y) y)
    :default (gcd x (- y x))))

(defn make-grid
  "Create two-dimension board with same-length side.

  n: int - length of the side
  value: any - value which to fullfil every cell in the grid,
         default to 0

  Returns: `n` vectors of `n` vectors of `value`

  Examples:
  `(make-grid 3)` ;; => `[[0 0 0] [0 0 0] [0 0 0]]`
  `(make-grid 2 1)` ;; => `[[1 1] [1 1]]`

  Notes:
  - constructor that uses lazy sequences would be faster,
    although grid is aimed to be subbed and looked up many times,
    so hopefully at the end of the day vectors will be beneficial.
  "
  ([n]
   (vec (repeat n (vec (repeat n 0)))))
  ([n value]
   (vec (repeat n (vec (repeat n value))))))

(defn nth-grid [grid row column]
  (-> grid
      (nth row)
      (nth column)))

(defn transpose [m]
  (apply mapv vector m))

(defn new-day! [year day]
  (let [fname (format "src/y%d/day%02d.clj" year day)
        iname (format "src/y%d/input%02d.txt" year day)
        tname (format "src/y%d/input%02d-test.txt" year day)
        template-content (slurp (io/resource "template.txt"))]
    (spit fname (format template-content year day))
    (spit iname "")
    (spit tname ""))
  :ok)

(def ^:private inf (Long/MAX_VALUE))

(defn neighbors
  "Returns n's neighbors, optionally filtered if unvisited"
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbors g n) uv)))

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [graph costs curr unvisited]
  (let [curr-cost (costs curr)]
    (reduce
      (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
      costs
      (neighbors graph curr unvisited))))

(defn dijkstra
  "Returns a mapping of nodes to minimum cost from source using Dijkstra algorithm.
  Graph is a mapping of nodes to map of neighboring nodes and associated cost.
  Optionally, specify :target node to return only the min price for target"
  ([graph source] (dijkstra graph source nil))
  ([graph source target]
   (let [graph-map (zipmap (keys graph) (repeat inf))]
     (loop [costs (assoc graph-map source 0)
            curr source
            unvisited (disj (apply hash-set (keys graph)) source)]
       (if (or (empty? unvisited) (= inf (costs curr)))
         costs
         (let [costs' (update-costs graph costs curr unvisited)
               curr' (first (sort-by costs' unvisited))]
           (println curr)
           (if (= target curr)
             (costs' target)
             (recur costs'
                    curr'
                    (disj unvisited curr')))))))))

(defn dfs
  "Traverses a graph in Depth First Search (DFS)"
  [graph v]
  (loop [stack   (vector v) ;; Use a stack to store nodes we need to explore
         visited []]        ;; A vector to store the sequence of visited nodes
    (if (empty? stack)      ;; Base case - return visited nodes if the stack is empty
      visited
      (let [v           (peek stack)
            neighbors   (get graph v)
            not-visited (remove #(in? % visited) neighbors)
            new-stack   (into (pop stack) not-visited)]
        (if (in? v visited)
          (recur new-stack visited)
          (recur new-stack (conj visited v)))))))

(defn bfs
  "Traverses a graph in Breadth First Search (BFS)."
  [graph v]
  (loop [;; Use a queue to store the nodes we need to explore
         queue   (conj clojure.lang.PersistentQueue/EMPTY v)

         ;; A vector to store the sequence of visited nodes
         visited []]

    ;; Base case - return visited nodes if the queue is empty
    (if (empty? queue) visited
        (let [v           (peek queue)
              neighbors   (get graph v)
              not-visited (remove #(in? % visited) neighbors)
              new-queue   (apply conj (pop queue) not-visited)]
          (if (in? v visited)
            (recur new-queue visited)
            (recur new-queue (conj visited v)))))))

(defn remove-ns-from-keys
  "Probably not particularly useful in AoC, but it's a good core function."
  [m]
  (reduce-kv (fn [agg k v]
               (assoc agg (-> k name keyword) v))
             {}
             m))

(defn range-1
  "Range from 1 to infinity"
  []
  (iterate inc' 1))

(defn diagonal [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))
