(ns user)

(defn parse-int
  [x]
  (Integer/parseInt x))

(defn parse-uint
  [x]
  (Integer/parseUnsignedInt x))

(defn in?
  [e coll]
  ((set coll) e))

(defn transpose [m]
  (apply mapv vector m))

(comment
  (in? :a [:a :b :c]) ;; => :a
  (in? :d [:a :b :c]) ;; => nil

  (in? "lea" ["amelia" "lea"]) ;; => "lea"
  (in? "jack" ["amelia" "lea"]) ;; => nil
  )
