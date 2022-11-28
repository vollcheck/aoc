(ns y16.day04
  (:require [clojure.string :as str]))

(defn load-input
  ([]
   (load-input "input04.txt"))
  ([filename]
   (str/split-lines (slurp (str "src/y16/" filename)))))

(defn by-count-then-alphabetically [[c1 v1] [c2 v2]]
  ;; NOTE: `a` is less than `b`
  (let [vv (compare v2 v1)] ;; NOTE: reversed!
    (if (not= vv 0)
      vv
      (compare c1 c2))))

(defn parse-room [room]
  (let [[sector-and-commons & letters] (reverse (str/split room #"-"))
        sorted-letters (->> letters
                            (apply str)
                            (frequencies)
                            (sort by-count-then-alphabetically))
        [sector commons] (str/split sector-and-commons #"[\[\]]")
        sector (Integer/parseUnsignedInt sector)]
    {:letters sorted-letters
     :sector sector
     :commons commons}))

(defn parse-room-2
  "Perserve the hyphens"
  [room]
  (let [sentence (-> room
                     (str/replace #"-\d*\[.*\]" "")
                     (str/replace #"-" " "))
        [sector-and-commons & letters] (reverse (str/split room #"-"))
        sorted-letters (->> letters
                            (apply str)
                            (frequencies)
                            (sort by-count-then-alphabetically))
        [sector commons] (str/split sector-and-commons #"[\[\]]")
        sector (Integer/parseUnsignedInt sector)]
    {:sentence sentence
     :letters sorted-letters
     :sector sector
     :commons commons}))

(defn room? [{:keys [letters commons]}]
  (= commons (->> letters
                  (take 5)
                  (map first)
                  (apply str))))

(defn shift-char [^Integer i ^Character c]
  ;; TODO refactor this ugly monster\x
  (if (= c \space)
    c
    (let [as-int (int c)
          shift (rem i 26)
          shifted (+ as-int shift)
          new (rem shifted 122)
          new (if (< new 97)
                (+ new 96)
                new)
          ;; _ (prn "old: " c ", new: " (char new))
          ]
      (char new))))

(defn shift-sentence [{:keys [sentence sector] :as room}]
  (->> sentence
       (reduce (fn [agg letter] (str agg (shift-char sector letter))) "")
       (assoc room :shifted-sentence)))

(defn part-1 [rooms]
  (->> rooms
       (map parse-room)
       (filter room?)
       (map :sector)
       (reduce +)))

(defn part-2 [rooms]
  (->> rooms
       (map parse-room-2)
       (filter room?)
       (map shift-sentence)
       (filter #(str/includes? (:shifted-sentence %) "north"))
       first
       :sector))

(comment
  (def q
    "aczupnetwp-dnlgpyrpc-sfye-dstaatyr-561[patyc]
jsehsyafy-vqw-ljsafafy-866[nymla]
tyepcyletzylw-ncjzrpytn-prr-opawzjxpye-743[cnrdl]")
  (part-2 (load-input))

  (str/includes? "north-room" "north") ;; => true
  (def r1 (parse-room-2 "qzmt-zixmtkozy-ivhz-343[abcde]"))
  (shift-char 343 \q)
  (shift-sentence r1)
  (shift-sentence {:sentence "qzmt" :sector 343})

  (:sentence (parse-room-2 "qzmt-zixmtkozy-ivhz-343[abcde]")) ;; => "qzmt zixmtkozy ivhz"
  (map #(shift-char % 343) "qzmt zixmtkozy ivhz")


  (shift-sentence {:sentence "qzmt zixmtkozy ivhz"
                   :sector 343})

  (->> (str/split "qzmt zixmtkozy ivhz" #" ")
       (map #(map (partial shift-char 343) %)))

  (def f (partial shift-char 343))
  (str/escape (apply str (map f "qzmt zixmtkozy ivhz")) {\space " "})

  (def input (load-input))
  (:sector (parse-room (first input)))
  (part-1 input) ;; => 278221
  (part-2 input)
  (class \q) ;; => java.lang.Character
  (type \q) ;; => java.lang.Character

  (shift-char \q 343) ;; => \v
  (-> \a int inc char) ;; => \b
  (-> \a int) ;; => 97
  (-> \z int) ;; => 122
  (-> \q int (quot 343))
  (let [c (-> \q int)
        move (rem 343 26)
        new (char (+ c move))
        ]
    new)

  (def parsed (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))
  parsed
  ;; => {:letters ([\a 5] [\b 3] [\x 1] [\y 1] [\z 1]),
  ;;     :sector 123,
  ;;     :commons "abxyz"}

  (room? parsed)

  (def rooms
    "aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]")

  )
