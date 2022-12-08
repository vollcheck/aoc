(ns core_test
  (:require [clojure.test :refer [deftest is testing]]
            [core :as c]))


(deftest split-by-space-test
  (is (= (c/split-by-space "just for fun")
         ["just" "for" "fun"]))
  (is (= (c/split-by-space "")
         [""])))

(deftest in?-test
  (let [coll [1 2 3 4]]
    (is (c/in? 1 coll))
    (is (not (c/in? 5 coll)))))

(deftest indexed-test
  )

(deftest positions-test
  )

(deftest split-by-test
  )

(deftest v-index-of-test
  )

(deftest v-indexes-of-test
  )

(deftest max-val-idx-test
  (is (= (c/max-val-idx [1 2 4 0 5])
         4)))

(deftest min-val-idx-test
  (is (= (c/min-val-idx [1 2 4 0 5])
         3)))

(deftest make-grid-test
  (is (= (c/make-grid 3)
         [[0 0 0] [0 0 0] [0 0 0]]))
  (is (= (c/make-grid 2 1)
         [[1 1] [1 1]])))

(deftest nth-in-grid
  )

(deftest transpose-test
  (is (= (c/transpose [[1 2 3] [4 5 6]])
         [[1 4] [2 5] [3 6]])))

(deftest dijkstra-test
  (let [demo-graph {:red    {:green 10, :blue   5, :orange 8},
                    :green  {:red 10,   :blue   3},
                    :blue   {:green 3,  :red    5, :purple 7},
                    :purple {:blue 7,   :orange 2},
                    :orange {:purple 2, :red    2}}]
    (is (= (c/dijkstra demo-graph :red)
           {:green 8, :blue 5, :purple 10, :red 0, :orange 8}))
    (is (= (c/dijkstra demo-graph :red :orange)
           8))))

(def graph {:A [:B :C]
            :B [:A :X]
            :X [:B :Y]
            :Y [:X]
            :C [:A :D]
            :D [:C :E :F]
            :E [:D :G]
            :F [:D :G]
            :G [:E :F]})

(deftest dfs-and-bfs-test
  (let [graph {:A [:B :C]
               :B [:A :X]
               :X [:B :Y]
               :Y [:X]
               :C [:A :D]
               :D [:C :E :F]
               :E [:D :G]
               :F [:D :G]
               :G [:E :F]}]
    (testing "dfs"
      (is (= (c/dfs graph :A)
             [:A :C :D :F :G :E :B :X :Y])))
    (testing "bfs"
      (is (= (c/bfs graph :A)
             [:A :B :C :X :D :Y :E :F :G])))))
