(ns day13.transparent-origami
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day13/input")]
    (let [[dot-strs [_ & fold-strs]]
          (split-with #(str/includes? % ",") (line-seq r))]
      {:dots (mapv (comp #(mapv parse-long %) #(str/split % #",")) dot-strs)
       :folds (->> fold-strs
                   (map (comp last #(str/split % #" ")))
                   (map #(str/split % #"="))
                   (mapv (juxt (comp keyword first)
                               (comp parse-long second))))})))

(def axis->pos {:x 0 :y 1})

(defn axis-val
  [dot axis]
  (nth dot (axis->pos axis)))

(defn folded-pos
  [v pos]
  (- v (* (- v pos) 2)))

(defn fold
  [dots axis fold-pos]
  (let [axis-pos (get axis->pos axis)
        {:keys [stay move]} (group-by (comp #(if (< fold-pos %) :move :stay)
                                            #(axis-val % axis))
                                      dots)
        moved (mapv (fn [d] (update d axis-pos #(folded-pos % fold-pos))) move)]
    (into (set stay) moved)))

(defn part1
  [{dots :dots [[axis fold-pos] & _] :folds}]
  (count (fold dots axis fold-pos)))

(comment
  (part1 input-data)
  )

(defn max-val
  [dots axis]
  (axis-val (apply max-key #(axis-val % axis) dots) axis))

(defn fold-all
  [dots folds]
  (reduce (fn [d [a p]] (fold d a p)) dots folds))

(defn dot-vals->dot-vectors
  [dots]
  (->> (for [x (range 0 (inc (max-val dots :x)))
             y (range 0 (inc (max-val dots :y)))]
         [x y])
       (group-by #(axis-val % :y))
       (sort-by first)
       (map second)
       (mapv (fn [x-dots] (mapv #(if (contains? dots %) "#" ".") x-dots)))))

(defn print-dots
  [dots]
  (doseq [line (dot-vals->dot-vectors dots)]
    (println line)))

(defn part2
  [{:keys [dots folds]}]
  (print-dots (fold-all dots folds)))

(comment
  (part2 input-data)
  )
