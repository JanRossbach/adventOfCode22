(ns advent
  (:require [clojure.string :as str]
            [data.deque :as dq]))

(defn read-lines [s]
  (str/split (slurp s) #"\n"))

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

;; Tag 5

(def init-crates {1 '(:J :F :C :N :D :B :W)
                  2 '(:T :S :L :Q :V :Z :P)
                  3 '(:T :J :G :B :Z :P)
                  4 '(:C :H :B :Z :J :L :T :D)
                  5 '(:S :J :B :V :G)
                  6 '(:Q :S :P)
                  7 '(:N :P :M :L :F :D :V :B)
                  8 '(:R :L :D :B :F :M :S :P)
                  9 '(:R :T :D :V)})

(def input5 (str/split (slurp "input5.txt") #"\n"))

(def commands (drop 10 input5))

(defn parse-command [c]
  (let [[_ num from to] (first (re-seq #"move (\d*) from (\d*) to (\d*)" c))]
    {:num (read-string num)
     :from (read-string from)
     :to (read-string to)}))

(def data (map parse-command commands))

(defn exec [crates {:keys [num from to]}]
  (let [old-from (get crates from)
        old-to (get crates to)
        new-to (concat (reverse (take num old-from)) old-to)
        new-from (drop num old-from)
        new-crates (assoc crates from new-from to new-to)]
    new-crates))


(def result51 (reduce exec init-crates data))

(apply str (map (fn [n] (name (first (get result51 n)))) (range 1 10))) ;; Convert into Output String

;; Part 2

(defn exec2 [crates {:keys [num from to]}]
  (let [old-from (get crates from)
        old-to (get crates to)
        new-to (concat (take num old-from) old-to)
        new-from (drop num old-from)
        new-crates (assoc crates from new-from to new-to)]
    new-crates))

(def result52 (reduce exec2 init-crates data))

(apply str (map (fn [n] (name (first (get result52 n)))) (range 1 10))) ;; Convert into Output String

;; Tag 6


(def input6 (slurp "input6.txt"))

(defn solve6 [s n]
  (loop [q (apply dq/deque (take n s))
         i n
         cs (drop n s)]
    (assert (= n (count q)))
    (if (= n (count (distinct q)))
      i
      (recur (-> q (dq/add-first (first cs)) (dq/remove-last))
             (inc i)
             (rest cs)))))

(def result61 (solve6 input6 4))
(def result62 (solve6 input6 14))
