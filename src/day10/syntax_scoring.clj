(ns day10.syntax-scoring
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day10/input")]
    (->> (line-seq r)
         (map (comp #(str/split % #"")))
         (into []))))

(def match?
  #{["[" "]"] ["(" ")"] ["{" "}"] ["<" ">"]})

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
         :cause :incomplete})
      out)))

(def score
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(defn part1
  [lines]
  (->> lines
       (map (juxt identity interpret))
       (filter (comp #{:corrupted} :cause second))
       (map (comp score :found second))
       (apply +)))

(comment
  (part1 input-data)
  )
