(ns advent
  (:require [clojure.string :as str]
            [data.deque :as dq]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.selection :refer [sel irange end]]))

(def read-lines (comp str/split-lines slurp))

;; Tag 1

(def input1 (str/split (slurp "input1.txt") #"\n"))

(def data1 (:vv (reduce (fn [a s]
                          (if (= s "")
                            (assoc a :vv (conj (:vv a) (:v a)) :v [])
                            (assoc a :v (conj (:v a) (read-string s)))))
                        {:vv []
                         :v []}
                        input1)))

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

;; Tag 7

(def input7 (read-lines "input7.txt"))

(defn command? [s]
  (= \$ (first s)))

(defn create-dir
  [lines]
  (reduce (fn [tree line]
            (cond
              (re-matches #"dir (.+)" line) (let [[_ name] (re-matches #"dir (.+)" line)] (assoc tree name {}))
              (re-matches #"(\d+) (.+)" line) (let [[_ size name] (re-matches #"(\d+) (.+)" line)] (assoc tree :files (conj (:files tree) {:filename name :size (read-string size)})))
              :else (throw (Exception. (str "Unrecognized input |" line "| encountered!")))))
          {:files []}
          lines))

(defn read-file-tree
  [ls]
  (loop [path []
         tree {:files []}
         output-list ls]
    (if (empty? output-list)
      tree
      (let [line (first output-list)]
        (cond
          (re-matches #"\$ cd .." line) (recur (pop path) tree (rest output-list))
          (re-matches #"\$ cd /" line) (recur [] tree (rest output-list))
          (re-matches #"\$ cd (.+)" line) (recur (conj path (second (re-matches #"\$ cd (.+)" line))) tree (rest output-list))
          (re-matches #"\$ ls" line) (recur path
                                            (if (empty? path)
                                              (create-dir (take-while #(not (command? %)) (rest output-list)))
                                              (assoc-in tree path (create-dir (take-while #(not (command? %)) (rest output-list)))))
                                            (drop-while #(not (command? %)) (rest output-list))))))))

(def file-tree (read-file-tree input7))

(def dirs (atom []))

(defn total-dir-size
  [dir]
  (let [dir-size (reduce
                  (fn [a e]
                    (if (vector? e)
                      (+ a (apply + (map :size e)))
                      (+ a (total-dir-size e))))
                  0
                  (vals dir))]
    (if (< dir-size 100000)
      (do (swap! dirs conj dir-size) dir-size) ;; Save the size to calc sum later
      dir-size)))

(total-dir-size file-tree)

(def result71 (apply + @dirs))

(def dirs2 (atom []))

(defn total-dir-size2
  [dir]
  (let [dir-size (reduce
                  (fn [a e]
                    (if (vector? e)
                      (+ a (apply + (map :size e)))
                      (+ a (total-dir-size2 e))))
                  0
                  (vals dir))]
    (if true
      (do (swap! dirs2 conj dir-size) dir-size) ;; Save the size to calc sum later
      dir-size)))

(def current-free-space (- 70000000 50216456))

(def free-space-needed (- 30000000 current-free-space))

(total-dir-size2 file-tree)

(def result72 (first (sort (filter (fn [e] (< free-space-needed e)) @dirs2))))

;; Tag 8

(def input8 (read-lines "input8.txt"))

(defn numstr->numvec [numstr]
  (mapv (fn [c] (read-string (str c))) numstr))

(def grid (m/matrix (mapv numstr->numvec input8)))

(defn visible-to-the
  [tree-line h]
  (not (some (fn [e] (<= h e)) tree-line)))

(defn visible?
  "Checks if the tree in the grid at row i and column j is visible. If yes, return 1, otherwise 0."
  [grid i j]
  (if (or (= i 0) (= j 0))
    1
    (let [height (get (get grid i) j)
          west-tree-line (flatten (sel grid (irange i i) (irange 0 (dec j))))
          north-tree-line (flatten (sel grid (irange 0 (dec i)) (irange j j)))
          south-tree-line (flatten (sel grid (irange (inc i) end) (irange j j)))
          east-tree-line (flatten (sel grid (irange i i) (irange (inc j) end)))]
      (cond
        (visible-to-the north-tree-line height) 1
        (visible-to-the east-tree-line height) 1
        (visible-to-the west-tree-line height) 1
        (visible-to-the south-tree-line height) 1
        :else 0))))

(def visible-trees (for [i (range (count grid))
                         j (range (count (first grid)))]
                     (visible? grid i j)))



(def result81 (apply + (flatten visible-trees)))

(defn calc-tree-score
  [tree-line height]
  (let [c (count (take-while (fn [t] (< t height)) tree-line))]
    (if (not= c (count tree-line))
      (inc c)
      c)))

(defn scenic-score
  [grid i j]
  (if (or (= i 0) (= j 0))
    0
    (let [height (get (get grid i) j)
          west-tree-line (reverse (flatten (sel grid (irange i i) (irange 0 (dec j)))))
          north-tree-line (reverse (flatten (sel grid (irange 0 (dec i)) (irange j j))))
          south-tree-line (flatten (sel grid (irange (inc i) end) (irange j j)))
          east-tree-line (flatten (sel grid (irange i i) (irange (inc j) end)))
          west-score (calc-tree-score west-tree-line height)
          east-score (calc-tree-score east-tree-line height)
          north-score (calc-tree-score north-tree-line height)
          south-score (calc-tree-score south-tree-line height)]
      (* west-score east-score north-score south-score))))

(def scores (for [i (range (count grid))
                  j (range (count (first grid)))]
              (scenic-score grid i j)))

(def result82 (apply max (flatten scores)))

(read-string "`(hello 1 2 3)")

;; Tag 9

(def input9 (read-lines "input9.txt"))

(defn parse-instruction [s] [(first s) (read-string (apply str (drop 2 s)))])

(def instructions (map parse-instruction input9))
(def instructions-long (mapcat (fn [[c n]] (repeat n c)) instructions))

(defn touching?
  [[HX HY] [TX TY]]
  (let [dX (Math/abs (- HX TX))
        dY (Math/abs (- HY TY))]
    (and (< dX 2)
         (< dY 2))))

(defn move-head [[X Y] direction]
  (case direction
    \R [(inc X) Y]
    \L [(dec X) Y]
    \U [X (inc Y)]
    \D [X (dec Y)]))

(defn catch-up-tail
  [NH H T]
  (if (touching? NH T)
    T
    H))

(defn move [[H T] direction]
  (let [NH (move-head H direction)
        NT (catch-up-tail NH H T)]
      [NH NT]))

(def result91 (->> instructions-long
                   (reductions move [[0 0] [0 0]])
                   (map second)
                   set
                   count))

(defn signum [x]
  (if (pos? x) 1 -1))

(defn catch-up [H T]
  (let [[hx hy] H
        [tx ty] T]
    (cond
      (touching? H T) T
      (or (= hx tx) (= hy ty)) [(+ tx (quot (- hx tx) 2)) (+ ty (quot (- hy ty) 2))]
      :else [(+ tx (signum (- hx tx))) (+ ty (signum (- hy ty)))])))

(defn move2 [[H Ts] direction]
  (let [NH (move-head H direction)
        NTs (loop [h NH
                   ts Ts
                   a []]
              (if (empty? ts)
                a
                (let [nt (catch-up h (first ts))]
                  (recur
                   nt
                   (rest ts)
                   (conj a nt)))))]
    [NH NTs]))

(def result92 (->> instructions-long
                   (reductions move2 [[0 0] (repeat 9 [0 0])])
                   (map second)
                   (map last)
                   set
                   count))

;; Tag 10

(def input10 (read-lines "input10.txt"))

(defn solve10
  [instructions]
  (loop [cycles [1]
         is instructions]
    (if (empty? is)
      cycles
      (if (= (first is) "noop")
        (recur (conj cycles (last cycles)) (rest is))
        (let [V (read-string (second (re-matches #"addx (-*\d+)" (first is))))]
          (recur (conj (conj cycles (last cycles)) (+ (last cycles) V))
                 (rest is)))))))

(defn signal-strength
  [cycles n]
  (* n (nth cycles (dec n))))

(def cycles (solve10 input10))

(def result101 (apply + (map (partial signal-strength cycles) (range 20 260 40))))

(defn visualize
  [grid]
  (str/join "\n" (map (partial apply str) grid)))

(defn draw
  [cycles]
  (loop [grid-position 0
         current-row []
         grid []
         cs cycles]
    (if (or (empty? cs)
            (= 6 (count grid)))
      (visualize grid)
      (let [X (first cs)
            draw-char (if (<= (dec X) (mod grid-position 40) (inc X))
                        \#
                        \.)
            [new-row new-grid] (if (= 40 (count current-row))
                                 [[draw-char] (conj grid current-row)]
                                 [(conj current-row draw-char)   grid])]
        (recur
         (inc grid-position)
         new-row
         new-grid
         (rest cs))))))

#_(println (draw cycles))

;; Tag 11

(def input11 (map (comp (partial map str/trim) str/split-lines str/trim) (drop 1 (str/split (slurp "input11.txt") #"Monkey"))))

(defn parse-monkey
  [monkey-data-list]
  (let [name (read-string (str (first (first monkey-data-list))))
        starting-items (mapv read-string (str/split (second (re-matches #"Starting items: (.*)" (nth monkey-data-list 1))) #","))
        [_ operator op-value] (re-matches #"Operation: new = old (.) (.*)" (nth monkey-data-list 2))
        div-val (read-string (second (re-matches #"Test: divisible by (\d+)" (nth monkey-data-list 3))))
        true-target (read-string (second (re-matches #"If true: throw to monkey (\d+)" (nth monkey-data-list 4))))
        false-target (read-string (second (re-matches #"If false: throw to monkey (\d+)" (nth monkey-data-list 5))))]
    {:name name
     :items starting-items
     :test (fn [e] (= 0 (mod e div-val)))
     :div-val div-val
     :operation (case operator
                  "*" (fn [x] (* x (if (= op-value "old")
                                     x (read-string op-value))))
                  "+" (fn [x] (+ x (if (= op-value "old")
                                     x (read-string op-value)))))
     :true-target true-target
     :false-target false-target
     :inspected-item-count 0}))

(def initial-monkeys (mapv parse-monkey input11))

(defn inspect-item
  "Executes one monkey inspecting the given item. Returns the new item-lists of all monkeys, where
  item-lists is a vector where the index corresponds to the name of the monkey and the value is a vector if items the monkey is currently holding."
  [monkey item-lists item]
  (let [{:keys [name operation test true-target false-target]} monkey
        new-value (int (/ (operation item) 3))
        target (if (test new-value) true-target false-target)]
    (assoc item-lists target (conj (get item-lists target) new-value))))

(defn round
  "plays a round and returns the new monkey state"
  [ms _]
  (loop [item-lists (mapv :items ms)
         inspected-item-counts (mapv :inspected-item-count ms)
         monkeys ms]
    (if (empty? monkeys)
      (mapv (fn [m i ic] (assoc m :items i :inspected-item-count ic)) ms item-lists inspected-item-counts)
      (let [monkey (first monkeys)
            name (:name monkey)
            items (get item-lists name)
            inspected-item-count (get inspected-item-counts name)
            new-item-lists (assoc (reduce (partial inspect-item monkey) item-lists items) name [])]
        (recur
         new-item-lists
         (assoc inspected-item-counts name (+ (count items) inspected-item-count))
         (rest monkeys))))))

(defn play-rounds [initial-state n]
  (reduce round initial-state (range n)))

(def result111 (apply * (take 2 (reverse (sort (map :inspected-item-count (play-rounds initial-monkeys 20)))))))

(defn inspect-item2
  "Executes one monkey inspecting the given item. Returns the new item-lists of all monkeys, where
  item-lists is a vector where the index corresponds to the name of the monkey and the value is a vector if items the monkey is currently holding."
  [monkey cm item-lists item]
  (let [{:keys [name operation test true-target false-target]} monkey
        new-value (mod (operation item) cm)
        target (if (test new-value) true-target false-target)]
    (assoc item-lists target (conj (get item-lists target) new-value))))

(defn round2
  "plays a round and returns the new monkey state"
  [ms _]
  (let [common-multiple (apply * (map :div-val ms))]
    (loop [item-lists (mapv :items ms)
           inspected-item-counts (mapv :inspected-item-count ms)
           monkeys ms]
      (if (empty? monkeys)
        (mapv (fn [m i ic] (assoc m :items i :inspected-item-count ic)) ms item-lists inspected-item-counts)
        (let [monkey (first monkeys)
              name (:name monkey)
              items (get item-lists name)
              inspected-item-count (get inspected-item-counts name)
              new-item-lists (assoc (reduce (partial inspect-item2 monkey common-multiple) item-lists items) name [])]
          (recur
           new-item-lists
           (assoc inspected-item-counts name (+ (count items) inspected-item-count))
           (rest monkeys)))))))

(defn play-rounds2 [initial-state n]
  (reduce round2 initial-state (range n)))

(def result112 (apply * (take 2 (reverse (sort (map :inspected-item-count (play-rounds2 initial-monkeys 10000)))))))
