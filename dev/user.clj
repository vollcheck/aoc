(ns user
  (:require [clojure.string :refer [split]]))

(defn split-line
  "Split string by new lines."
  [s]
  (split s #"\n"))

(defn parse-int
  [x]
  (Integer/parseInt x))

(defn parse-uint
  [x]
  (Integer/parseUnsignedInt x))
