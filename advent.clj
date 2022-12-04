(ns advent
  (:require [clojure.string :as str]))

;; Tag 1

(def input1 (str/split (slurp "input1.txt") #"\n"))

(def data1 (:vv (reduce (fn [a s]
                          (if (= s "")
                            (assoc a :vv (conj (:vv a) (:v a)) :v [])
                            (assoc a :v (conj (:v a) (read-string s)))))
                        {:vv []
                         :v []}
                        input1)))

(println data1)

(def sums (map #(apply + %) data1))

(def result11 (apply max sums))

(def result12 (apply + (take 3 (reverse (sort sums)))))

;; Tag2

(def input2 (str/split (slurp "input2.txt") #"\n"))

(defn calc-score
  [round]
   (case round
     "A X" 4
     "A Y" 8
     "A Z" 3
     "B X" 1
     "B Y" 5
     "B Z" 9
     "C X" 7
     "C Y" 2
     "C Z" 6))



(def result21 (apply + (map calc-score input2)))

(defn calc-score2
  [round]
  (case round
    "A X" 3
    "A Y" 4
    "A Z" 8
    "B X" 1
    "B Y" 5
    "B Z" 9
    "C X" 2
    "C Y" 6
    "C Z" 7))

(def result22 (apply + (map calc-score2 input2)))

;; Tag3

(def input3 (str/split (slurp "input3.txt") #"\n"))

(defn split-compartments [s]
  (let [l (int (/ (count s) 2))]
    [(take l s)
     (drop l s)]))

(def data3 (map split-compartments input3))

(defn priority [c]
  (let [i (int c)]
    (if (< i 97)
      (- i 38)
      (- i 96)
      )))

(defn prio-of-common-item [[c1 c2]]
  (let [s (into #{} c1)
        common-item (first (filter #(contains? s %) c2))]
    (priority common-item)))

(def result31 (apply + (map prio-of-common-item data3)))

(def elf-groups (partition 3 input3))

(defn group-badge [s]
  (let [elf1 (first s)
        elf2 (into #{} (second s))
        elf3 (into #{} (last s))]
    (first (filter (fn [c]
                     (and (contains? elf2 c)
                          (contains? elf3 c)))
                   elf1))))

(def result32 (->> elf-groups
                  (map group-badge)
                  (map priority)
                  (apply +)))

;; Tag4

(def input4 (str/split (slurp "input4.txt") #"\n"))

input4

(defn parse [s]
  (let [t (str/split s #",")
        range1 (str/split (first t) #"-")
        range2 (str/split (second t) #"-")]
    [(mapv read-string range1) (mapv read-string range2)]))

(def pairs (map parse input4))

(defn pair-includes?
  [[[l1 h1] [l2 h2]]]
  (cond
    (and (<= l1 l2) (>= h1 h2)) true
    (and (<= l2 l1) (>= h2 h1)) true
    :else false))

(def result41 (count (filter pair-includes? pairs)))

(defn pair-overlaps?
  [[[l1 h1] [l2 h2]]]
  (cond
    (< h1 l2) false
    (< h2 l1) false
    :else true))

(def result42 (count (filter pair-overlaps? pairs)))

861
