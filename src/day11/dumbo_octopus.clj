(ns day11.dumbo-octopus
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day11/input")]
    (->> (line-seq r)
         (map (comp #(mapv parse-long %) #(str/split % #"")))
         (into []))))

(defn safe-inc
  [v]
  (when v (inc v)))

(defn inc-line
  [line]
  (mapv safe-inc line))

(defn inc-all
  [grid]
  (mapv inc-line grid))

(defn inc-pos
  [grid [x y]]
  (update-in grid [y x] safe-inc))

(defn get-flashable-line-pos
  [line]
  (some (fn [[i v]] (when (and v (< 9 v)) i)) (map-indexed vector line)))

(defn get-flashable-grid-pos
  [grid]
  (some (fn [[i ln]] (when-let [x (get-flashable-line-pos ln)] [x i]))
        (map-indexed vector grid)))

(def adjacent-position-fns
  (for [x [dec identity inc]
        y [dec identity inc]
        :when (not= x y identity)]
    [x y]))

(def not-neg? (comp not neg?))

(defn in-range?
  [v max-v]
  ((every-pred not-neg? #(< % max-v)) v))

(defn find-adjacent-positions
  [[x y] [max-x max-y]]
  (->> adjacent-position-fns
       (map (fn [[fx fy]] [(fx x) (fy y)]))
       (filter (every-pred (comp #(in-range? % max-x) first)
                           (comp #(in-range? % max-y) second)))))

(defn mark-flashed-nil
  [grid [x y]]
  (assoc-in grid [y x] nil))

(defn nil->0
  [grid]
  (mapv (fn [ln] (mapv #(or % 0) ln)) grid))

(defn flash
  [grid pos]
  (let [max-pos [(count (first grid)) (count grid)]
        aps (find-adjacent-positions pos max-pos)]
    (-> (reduce inc-pos grid aps)
        (mark-flashed-nil pos))))

(defn step
  [grid]
  (-> (reduce (fn [g i]
                (assert (< i 900) "limit reached")
                (if-let [p (get-flashable-grid-pos g)]
                  (flash g p)
                  (reduced g)))
              (inc-all grid)
              (range 1000))
      nil->0))

(defn count-0
  [grid]
  (apply + (map (comp count #(filter zero? %)) grid)))

(defn part1
  [grid]
  (-> (reduce (fn [{:keys [grid] :as acc} _]
                (let [g+ (step grid)]
                  (-> acc
                      (assoc :grid g+)
                      (update :counter + (count-0 g+)))))
              {:grid grid :counter 0}
              (range 100))
      :counter))

(comment
  (part1 input-data))
