(ns day3.binary-diagnostic
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def data
  (with-open [r (jio/reader "src/day3/input")]
    (into [] (map (comp #(mapv parse-long %) #(str/split % #""))) (line-seq r))))

(defn transpose
  [lines]
  (apply mapv vector lines))

(defn take-key-from-compare-vals-by
  [f m]
  (first (apply f second m)))

(def take-higher-or-1
  (some-fn #(when (apply = (vals %)) 1)
           (partial take-key-from-compare-vals-by max-key)))

(def take-lower-or-0
  (some-fn #(when (apply = (vals %)) 0)
           (partial take-key-from-compare-vals-by min-key)))

(defn bin-vec->decimal
  [v]
  (-> v str/join (Long/parseLong 2)))

(defn part1
  [lines]
  (let [bin->freq (->> lines transpose (map frequencies))]
    (->> [take-higher-or-1 take-lower-or-0]
         (map (comp bin-vec->decimal #(mapv % bin->freq)))
         (apply *))))

(part1 data)


(defn extract-line
  [select-fn lines]
  (-> (reduce
        (fn [lines- i]
          (let [bin->freq (->> lines- transpose (mapv frequencies))
                selected-bin (select-fn (get bin->freq i))
                [_ & others :as lines--] (filterv (comp #{selected-bin} #(get % i)) lines-)]
            (if (seq others)
              lines--
              (reduced lines--))))
        lines
        (range))
      first))

(defn part2
  [lines]
  (->> [#(extract-line take-higher-or-1 %) #(extract-line take-lower-or-0 %)]
       (map (comp bin-vec->decimal #(% lines)))
       (apply *)))

(part2 data)
