(ns day2.dive
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input
  (with-open [r (jio/reader "src/day2/input")]
    (into [] (line-seq r))))

(def data
  (->> input
       (map #(str/split % #" "))
       (map #(update % 1 parse-long))))

;; Part 1
(let [{:strs [forward down up]}
      (->> data
           (group-by first)
           (map (juxt first (comp #(apply + %) #(map second %) second)))
           (into {}))]
  (* (- down up) forward))

;; Part 2
(-> (reduce (fn [acc [cmd v]]
              (if (= cmd "forward")
                (let [a (:a acc)]
                  (-> acc
                      (update :h + v)
                      (update :d + (* v a))))
                (update acc :a (if (= cmd "down") + -) v)))
            {:d 0
             :h 0
             :a 0}
            data)
    ((juxt :d :h))
    (#(apply * %)))
