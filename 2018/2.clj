; (setq cider-jack-in-default "lein")
(ns aoc2.core)

(defn input [] (clojure.string/split-lines (slurp "2.input")))

(defn lettercounts [word]
  (loop [counts {}
         c (first word)
         tail (rest word)]
    (let [c-count (if (contains? counts c) (inc (counts c)) 1)
          counts (assoc counts c c-count)]
      (if (seq tail)
        (recur counts (first tail) (rest tail))
        counts))))

;; core frequencies replaces lettercounts
(defn exactly-n [word n]
  (let [counts (frequencies word)]
    (some #(= n (second %)) counts)))

(defn check [n] (count (filter #(exactly-n % n) (input))))

(println "part1: " (* (check 2) (check 3)))


(defn shared [a b]
  (loop [a1 (first a)
         b1 (first b)
         a (rest a)
         b (rest b)
         same ""]
    (let [same (if (= a1 b1) (str same a1) same)]
      (if (and (seq a) (seq b))
        (recur (first a) (first b) (rest a) (rest b) same)
        same ))))


(doseq [a (input)]
  (doseq [b (input)]
    (let [same (shared a b)
          x (count a)
          y (count same)
          diff (- x y)]
      (when (= 1 diff)
        (println same a b))
      )))

