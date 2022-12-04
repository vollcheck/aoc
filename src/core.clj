(ns core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; -------------
;; MISCELLANEOUS
;; -------------

(defn parse-int
  [x]
  (Integer/parseInt x))

(defn parse-uint
  [x]
  (Integer/parseUnsignedInt x))

;; -----------
;; COLLECTIONS
;; -----------

(defn in?
  [e coll]
  (some #(= e %) coll))

;; ---
;; I/O
;; ---

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

;; ----
;; GRID
;; ----

(defn make-grid
  "Create two-dimension board with same-length side.

  n: int - length of the side
  value: any - value which to fullfil every cell in the grid,
         default to 0

  Returns: `n` vectors of `n` vectors of `value`

  Examples:
  `(make-grid 3)` ;; => `([0 0 0] [0 0 0] [0 0 0])`
  `(make-grid 2 1)` ;; => `([1 1] [1 1])`

  Notes:
  - constructor that uses lazy sequences would be faster,
    although grid is aimed to be subbed and looked up many times,
    so hopefully at the end of the day vectors will be beneficial.
  "
  ([n]
   (vec (repeat n (vec (repeat n 0)))))
  ([n value]
   (vec (repeat n (vec (repeat n value))))))

(defn nth-in-grid [grid row column]
  (nth (nth grid row) column))

(defn transpose [m]
  (apply mapv vector m))

;; ----
;; META
;; ----

(defn new-day! [year day]
  (let [fname (format "src/y%d/day%02d.clj" year day)
        iname (format "src/y%d/input%02d.txt" year day)
        tname (format "src/y%d/input%02d-test.txt" year day)
        template-content (slurp (io/resource "template.txt"))]
    (spit fname (format template-content year day))
    (spit iname "")
    (spit tname ""))
  :ok)
