(ns y24.day06
  (:require [core :refer [split-by load-in]]))

;; x position
;; y position
;; "direction" - face position

;; if run on \#, turn left

(defn get2 [grid x y]
  (-> (nth grid y)
      (nth x)))

(defn find-objects [grid]
  (let [h (count grid)
        w (count (first grid))
        [walls starting-point]
        (->> (for [y (range h)
                   x (range w)
                   :let [c (get2 grid x y)]
                   :when (contains? #{\# \^} c)]
               [c x y])
             (split-by #(= (first %) \#)))]
    {:walls walls
     :starting-point (first starting-point)}))

(defn turn-right [{:keys [face] :as pos}]
  (let [new-face (case face
                   :north :east
                   :east :south
                   :south :west
                   :west :north)]
    (assoc pos :face new-face)))

(defn new-move [pos]
  (case (:face pos)
    :north (update pos :y dec)
    :east  (update pos :x inc)
    :south (update pos :y inc)
    :west  (update pos :x dec)))

(defn beyond? [grid {:keys [x y]}]
  (let [max-y (count grid)
        max-x (count (first grid))]
    (or (not (< -1 x max-x))
        (not (< -1 y max-y)))))

(defn move [grid [sx sy]]
  (loop [pos {:x sx :y sy :face :north}
         steps #{[sx sy]}]
    (let [next-move (new-move pos)
          nx (:x next-move)
          ny (:y next-move)]
      (cond
        (beyond? grid next-move)
        (count steps)

        (= \# (get2 grid nx ny))
        (recur (turn-right pos)
               steps)

        :else
        (recur next-move
               (conj steps [nx ny]))))))



(comment
  (count intest)
  (beyond? intest {:x 0 :y 10})
  )

(defn part-1 [in]
  (let [{:keys [#_walls starting-point]} (find-objects in)
        [_ x y] starting-point]
    (move in [x y])))

(comment
  (find-objects intest)
  ;; => {:walls
  ;;     ([\# 4 0]
  ;;      [\# 9 1]
  ;;      [\# 2 3]
  ;;      [\# 7 4]
  ;;      [\# 1 6]
  ;;      [\# 8 7]
  ;;      [\# 0 8]
  ;;      [\# 6 9]),
  ;;     :starting-point [\^ 4 6]}
  )
;; good obstacle is when:
;; - you have it on your right
;; - (immediately) in front there is no other obstacle
;; - in some time (not necessarily immediately) leads to looped steps

(defn obs-to-the-right? [walls {:keys [x y face] :as pos}]
  (case face
    ;; y is the same and x is smaller than wall
    :north (some (fn [[wx wy]]
                   (and (< x wx)
                        (= y wy)))
                 walls)

    ;; x is the same and y is smaller than wall-y
    :east  (some (fn [[wx wy]]
                   (and (= x wx)
                        (< y wy)))
                 walls)
    :south (some (fn [[wx wy]]
                   (and (> x wx)
                        (= y wy)))
                 walls)
    :west  (some (fn [[wx wy]]
                   (and (= x wx)
                        (> y wy)))
                 walls)))

(defn obs-not-in-front? [walls {:keys [x y]}]
  (not (contains? (set walls) [x y])))

(defn good-obstacle-place? [walls pos]
  (and (obs-not-in-front? walls (new-move pos))
       (obs-to-the-right? walls pos)))

(comment
  (def wls (->> (find-objects intest)
              :walls
              (mapv #(into [] (rest %)))))

  (good-obstacle-place? wls {:face :north :x 5 :y 1})

  (obs-to-the-right? wls {:face :north :x 4 :y 1})
  (obs-to-the-right? wls {:face :east  :x 7 :y 2})
  (obs-to-the-right? wls {:face :south :x 8 :y 0})
  (obs-to-the-right? wls {:face :west  :x 4 :y 2})
  )

(defn update-grid [grid x y]
  (assoc grid y
         (apply str (assoc (into [] (nth grid y))
                           x \#))))

(defn gonna-loop? [grid steps pos]
  (loop [pos pos
         steps steps]
    (let [next-move (new-move pos)
          nx (:x next-move)
          ny (:y next-move)]
      (cond
        (beyond? grid next-move)
        false

        (contains? steps next-move)
        true

        (= \# (get2 grid nx ny))
        (recur (turn-right pos)
               steps)

        :else
        (recur next-move
               (conj steps next-move))))))


(defn show-path-1st-part [grid [sx sy]]
  (loop [pos {:x sx :y sy :face :north}
         steps #{[sx sy]}]
    (let [next-move (new-move pos)
          nx (:x next-move)
          ny (:y next-move)]
      (cond
        (beyond? grid next-move)
        #_(count steps)
        steps

        (= \# (get2 grid nx ny))
        (recur (turn-right pos)
               steps)

        :else
        (recur next-move
               (conj steps [nx ny]))))))

(defn move2 [grid walls [sx sy]]
  (loop [pos {:x sx :y sy :face :north}
         steps #{pos}
         obstacles #{}]
    (let [next-move (new-move pos)
          nx (:x next-move)
          ny (:y next-move)]
      (cond
        (beyond? grid next-move)
        ;; we've checked all the possible places for obstacles basically
        #_(count obstacles)
        obstacles

        (= \# (get2 grid nx ny))
        (recur (turn-right pos)
               steps
               obstacles)

        ;; when it would be a good place to put an obstacle
        ;; - check if putting a put it inside
        (and (good-obstacle-place? walls pos)
             (gonna-loop? #_grid
                          (update-grid grid nx ny)
                          (conj steps next-move)
                          (turn-right pos))
             (not= \# (get2 grid
                            (:x (turn-right pos))
                            (:y (turn-right pos)))))

        (recur next-move
               (conj steps next-move)
               (conj obstacles [nx ny]))

        :else
        (recur next-move
               (conj steps next-move)
               obstacles)))))

(comment
  ;; testing
  (-> (let [{:keys [walls starting-point]} (find-objects intest)
            walls (mapv #(into [] (rest %)) walls)
            [_ x y] starting-point]
        (move2 intest walls [x y]))
      )
  ;; => #{[7 7] [6 7] [1 8] [7 9] [3 6] [3 8]}
  ;; => 6
  ;; => #{[6 7] [1 8] [7 9] [3 6] [3 8]}
  ;; => #{[6 7] [1 8] [7 9] [3 6] [3 8]}

  (let [input (load-in)
        {:keys [walls starting-point]} (find-objects input)
        ;; just drop the \# from every wall-esque vector
        walls (mapv #(into [] (rest %)) walls)
        [_ x y] starting-point
        put-obstacles (move2 input walls [x y])]
    (count
     (clojure.set/intersection
      put-obstacles
      (show-path-1st-part input [x y]))
    ))
   ;; => 1847
   ;; => 4826

  ;;    null
  ;; => 1847
   ;; => 1848
   ;; => 1848
   ;; => 1848
  )


(defn part-2 [in]
  (let [{:keys [walls starting-point]} (find-objects in)
        ;; just drop the \# from every wall-esque vector
        walls (mapv #(into [] (rest %)) walls)
        [_ x y] starting-point]
    (move2 in walls [x y])))

(comment
  (def intest (load-in :test))
  (def in (load-in))
  (part-1 intest)
  (part-1 in)
  ;; => 4826

  (part-2 intest)
  (part-2 in)
  ;; should be
  ;; => 1721
  )
