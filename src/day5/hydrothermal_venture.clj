(ns day5.hydrothermal-venture
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day5/sample")]
    (into []
          (map (fn [x]
                 (->> (re-seq #"(.*,.*) -> (.*,.*)" x)
                      first
                      next
                      (mapcat #(str/split % #","))
                      (map parse-long)
                      (partition-all 2)
                      (map vec)
                      (into #{}))))
          (line-seq r))))

(defn x-dots
  [dots]
  (map first dots))

(defn y-dots
  [dots]
  (map second dots))

(defn line-dots
  [dots]
  (let [xs (x-dots dots)
        ys (y-dots dots)]
   (for [x (range (apply min xs) (inc (apply max xs)))
         y (range (apply min ys) (inc (apply max ys)))]
     [x y])))

(defn horizontal?
  [dots]
  (apply = (y-dots dots)))

(defn vertical?
  [dots]
  (apply = (x-dots dots)))

(defn all-dots
  [lines]
  (mapcat line-dots lines))

(defn part1
  [lines]
  (->> lines
       (filter (some-fn horizontal? vertical?))
       all-dots
       frequencies
       (filter (comp #(<= 2 %) second))
       count))

(comment
  (part1 input-data))
