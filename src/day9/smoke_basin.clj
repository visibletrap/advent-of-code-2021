(ns day9.smoke-basin
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day9/input")]
    (->> (line-seq r)
         (mapv (comp #(mapv parse-long %) #(str/split % #"")))
         (into []))))

(defn point->val
  [[x y] heightmap]
  (get-in heightmap [y x] Integer/MAX_VALUE))

(defn expand-points
  [[x y]]
  (->> [[identity identity]
        [dec identity]
        [identity dec]
        [inc identity]
        [identity inc]]
       (map (fn [[fx fy]] [(fx x) (fy y)]))))

(defn low-point?
  [point heightmap]
  (let [[point-val & other-vals] (->> (expand-points point)
                                      (map #(point->val % heightmap)))]
    (every? #(< point-val %) other-vals)))

(defn part1
  [heightmap]
  (let [w (count (first heightmap))
        h (count heightmap)]
    (->> (for [x (range 0 w)
               y (range 0 h)]
           [x y])
         (filter #(low-point? % heightmap))
         (map #(point->val % heightmap))
         (map inc)
         (apply +))))

(comment
  (part1 input-data)
  )
