(ns day5.hydrothermal-venture
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day5/input")]
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

(defn horizontal-line?
  [dots]
  (apply = (y-dots dots)))

(defn vertical-line?
  [dots]
  (apply = (x-dots dots)))

(defn diagonal-line?
  [dots]
  (= (Math/abs ^long (apply - (x-dots dots)))
     (Math/abs ^long (apply - (y-dots dots)))))

(defn horizontal-dots
  [dots]
  (let [xs (x-dots dots)
        [[_ y]] (seq dots)]
    (mapv vector (range (apply min xs) (inc (apply max xs))) (repeat y) )))

(defn vertical-dots
  [dots]
  (let [[[x]] (seq dots)
        ys (y-dots dots)]
    (mapv vector (repeat x) (range (apply min ys) (inc (apply max ys))))))

(defn north-west-and-south-east-dots?
  [dots]
  (let [[[x1 y1] [x2 y2]] (sort-by first (seq dots))]
    (and (< x1 x2) (< y1 y2))))

(defn south-west-and-north-east-dots?
  [dots]
  (let [[[x1 y1] [x2 y2]] (sort-by first (seq dots))]
    (and (< x1 x2) (> y1 y2))))

(defn dots-between-north-west-and-south-east-dots
  [dots]
  (let [[[x1 y1] [x2 y2]] (sort-by first dots)]
    (mapv vector (range x1 (inc x2)) (range y1 (inc y2)))))

(defn dots-between-south-west-and-north-east-dots
  [dots]
  (let [[[x1 y1] [x2 y2]] (sort-by first dots)]
    (mapv vector (range x1 (inc x2)) (range y1 (dec y2) -1))))

(defn diagonal-dots
  [dots]
  (cond
    (north-west-and-south-east-dots? dots)
    (dots-between-north-west-and-south-east-dots dots)

    (south-west-and-north-east-dots? dots)
    (dots-between-south-west-and-north-east-dots dots)))

(defn gen-dots
  [dots]
  (cond
    (horizontal-line? dots) (horizontal-dots dots)
    (vertical-line? dots) (vertical-dots dots)
    (diagonal-line? dots) (diagonal-dots dots)))

(defn part1
  [lines]
  (->> lines
       (filter (some-fn horizontal-line? vertical-line?))
       (mapcat gen-dots)
       frequencies
       (filter (comp #(<= 2 %) second))
       count))

(comment
  (part1 input-data))

(defn part2
  [lines]
  (->> lines
       (filter (some-fn horizontal-line? vertical-line? diagonal-line?))
       (mapcat gen-dots)
       frequencies
       (filter (comp #(<= 2 %) second))
       count))

(comment
  (part2 input-data))
