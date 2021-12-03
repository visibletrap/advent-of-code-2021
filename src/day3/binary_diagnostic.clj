(ns day3.binary-diagnostic
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def data
  (with-open [r (jio/reader "src/day3/input")]
    (into [] (map (comp #(mapv parse-long %) #(str/split % #""))) (line-seq r))))

(defn gamma-rate
  [d]
  (let [half (/ (count d) 2)]
    (->> (apply map
                (comp #(if (>= % half) 1 0)
                      #(apply + %&))
                d)
         (into []))))

(def flip {1 0 0 1})

(defn epsilon-rate
  [d]
  (mapv flip (gamma-rate d)))

(defn bin-vec->decimal
  [v]
  (-> v str/join (Long/parseLong 2)))

(defn part1
  [lines]
  (* (-> lines gamma-rate bin-vec->decimal)
     (-> lines epsilon-rate bin-vec->decimal)))

(part1 data)


(defn find-major
  [d]
  (let [half (/ (count d) 2)]
    (->> (apply map
                (comp #(if (>= % half) 1 0)
                      #(apply + %&))
                d)
         (into []))))

(defn find-minor
  [d]
  (let [half (/ (count d) 2)]
    (->> (apply map
                (comp #(if (>= % half) 0 1)
                      #(apply + %&))
                d)
         (into []))))

(defn part2-decoder
  [f lines]
  (-> (reduce (fn [d i]
                (if (seq (rest d))
                  (filterv (comp #{(-> d f (get i))} #(get % i)) d)
                  (reduced d)))
              lines
              (range))
      first))

(defn part2
  [lines]
  (* (->> lines (part2-decoder find-major) bin-vec->decimal)
     (->> lines (part2-decoder find-minor) bin-vec->decimal)))

(part2 data)
