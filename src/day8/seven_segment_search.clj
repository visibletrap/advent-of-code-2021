(ns day8.seven-segment-search
  (:require [clojure.java.io :as jio]
            [clojure.set :as set]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day8/input")]
    (->> (line-seq r)
         (map #(str/split % #"\|"))
         (map (juxt (comp #(str/split % #"\ ") str/trim first)
                    (comp #(str/split % #"\ ") str/trim second)))
         (map #(zipmap [:input :output] %))
         (into []))))

(defn part1
  [lines]
  (count (filter (comp #{2 3 4 7} count) (mapcat :output lines))))

(comment
  (part1 input-data)
  )


(defn find-number-1-code
  [line-codes]
  (filter (comp #{2} count) line-codes))

(defn find-number-7-code
  [line-codes]
  (filter (comp #{3} count) line-codes))

(defn find-number-4-code
  [line-codes]
  (filter (comp #{4} count) line-codes))

(defn find-pos0
  [one seven]
  (first (set/difference seven one)))

(defn find-pos13
  [one four]
  (set/difference four one))

(def number->positions
  ;   0
  ; 1   2
  ;   3
  ; 4   5
  ;   6
  {0 [0 1 2 4 5 6]
   1 [2 5]
   2 [0 2 3 4 6]
   3 [0 2 3 5 6]
   4 [1 2 3 5]
   5 [0 1 3 5 6]
   6 [0 1 3 4 5 6]
   7 [0 2 5]
   8 [0 1 2 3 4 5 6]
   9 [0 1 2 3 5 6]})

(defn char-positions->codes
  [char-positions]
  (set (map (comp set #(map char-positions %) second) number->positions)))

(defn valid-positions?
  [char-positions code-line]
  (every? (char-positions->codes char-positions) (map set code-line)))

(def chars "abcdefg")

(defn line->answered-number
  [line]
  (let [{:keys [input output]} line
        line-codes (into input output)
        char-positions
        (let [one (-> line-codes find-number-1-code first set)
              seven (-> line-codes find-number-7-code first set)
              four (-> line-codes find-number-4-code first set)
              pos0 (find-pos0 one seven)
              pos25 one
              pos13 (find-pos13 one four)]
          (->> (for [p0 [pos0]
                     p2 pos25
                     p1 pos13
                     :let [p5 (first (remove #{p2} pos25))
                           p3 (first (remove #{p1} pos13))
                           pos46 (remove #{p0 p1 p2 p3 p5} chars)]]
                 (for [p4 pos46
                       :let [p6 (first (remove #{p4} pos46))]]
                   [p0 p1 p2 p3 p4 p5 p6]))
               (apply concat)
               (filter #(valid-positions? % line-codes))
               first))
        code->number (-> number->positions
                         (update-vals #(set (map char-positions %)))
                         set/map-invert)]
    (->> (map set output)
         (map code->number)
         (apply str)
         (parse-long)))

  (defn part2
    [lines]
    (->> lines
         (map line->answered-number)
         (apply +))))

(comment
  (part2 input-data)
  )
