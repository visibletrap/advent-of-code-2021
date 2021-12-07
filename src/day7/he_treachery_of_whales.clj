(ns day7.he-treachery-of-whales
  (:require [clojure.java.io :as jio]
            [clojure.java.math :as math]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day7/input")]
    (into [] (map parse-long (str/split (first (line-seq r)) #",")))))

(defn cal-move-to
  [current-positions target-position]
  (apply + (map (comp math/abs #(- % target-position)) current-positions)))

(defn part1
  [current-positions]
  (->> (range (apply min current-positions) (apply max current-positions))
       (map #(cal-move-to current-positions %))
       (apply min)))

(comment
  (part1 input-data)
  )
