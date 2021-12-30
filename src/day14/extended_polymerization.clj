(ns day14.extended-polymerization
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day14/input")]
    (let [[[polymer-template] pair-insertion-rules]
          (split-with (comp not #(str/includes? % " ")) (line-seq r))]
      {:polymer-template polymer-template
       :pair-insertion-rules (mapv #(str/split % #" -> ") pair-insertion-rules)})))

(defn rules->index-updates
  [rules]
  (->> rules
       (map (juxt (comp vec first) second))
       (map (juxt first
                  (fn [[[a b :as z] [c]]]
                    [[z -1] [[a c] 1] [[c b] 1]])))
       (into {})))

(defn count-pairs
  [template]
  (frequencies (map vector template (next template))))

(defn compute-all-index-updates
  [index index-updates]
  (mapcat (fn [[k v]]
            (map (juxt first (comp #(* % v) second)) (get index-updates k)))
          index))

(defn apply-updates
  [index updates]
  (->> (reduce #(update %1 (first %2) (fnil + 0) (second %2)) index updates)
       (remove (comp zero? second))
       (into {})))

(defn index->char-counts
  [index last-char]
  (-> (->> index
           (map (juxt ffirst second))
           (reduce #(update %1 (first %2) (fnil + 0) (second %2)) {}))
      (update last-char (fnil inc 0))))

(defn polymerization-index
  [index index-updates n]
  (reduce
    (fn [m _]
      (let [updates (compute-all-index-updates m index-updates)]
        (apply-updates m updates)))
    index
    (range n)))

(defn polymerization
  [template rules n]
  (polymerization-index (count-pairs template) (rules->index-updates rules) n))

(defn solve
  [{template :polymer-template rules :pair-insertion-rules} n]
  (let [chars (-> (polymerization template rules n)
                  (index->char-counts (last template)))
        vs (-> chars vals sort)]
    (- (last vs) (first vs))))

(defn part1
  [in]
  (solve in 10))

(defn part2
  [in]
  (solve in 40))

(comment
  (part1 input-data)
  ; 2360
  (part2 input-data)
  ; 2967977072188
  )
