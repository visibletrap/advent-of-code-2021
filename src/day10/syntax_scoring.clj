(ns day10.syntax-scoring
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day10/input")]
    (->> (line-seq r)
         (map (comp #(str/split % #"")))
         (into []))))

(def match-char
  {"[" "]"
   "<" ">"
   "{" "}"
   "(" ")"})

(def match? (set match-char))

(defn autocomplete
  [chars]
  (->> chars
       reverse
       (mapv match-char)))

(defn interpret
  [line]
  (let [out (reduce
              (fn [s a]
                (case a
                  ("[" "(" "{" "<") (conj s a)
                  ("]" ")" "}" ">") (if (match? [(peek s) a])
                                      (pop s)
                                      (reduced {:success false
                                                :cause :corrupted
                                                :expect (peek s)
                                                :found a}))))
              []
              line)]
    (if (vector? out)
      (if (empty? out)
        {:success true}
        {:success false
         :cause :incomplete
         :fix (autocomplete out)})
      out)))

(def char->score1
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(defn part1
  [lines]
  (->> lines
       (map (juxt identity interpret))
       (filter (comp #{:corrupted} :cause second))
       (map (comp char->score1 :found second))
       (apply +)))

(comment
  (part1 input-data)
  )

(def char->score2
  {")" 1
   "]" 2
   "}" 3
   ">" 4})

(defn cal-part2-score
  [chars]
  (->> chars
       (map char->score2)
       (reduce (fn [t s] (+ (* t 5) s)) 0)))

(defn part2
  [lines]
  (let [scores (->> lines
                    (map (juxt identity interpret))
                    (filter (comp #{:incomplete} :cause second))
                    (map (comp cal-part2-score :fix second))
                    (sort)
                    (into []))
        median-pos (/ (dec (count scores)) 2)]
    (get scores median-pos)))

(comment
  (part2 input-data)
  )
