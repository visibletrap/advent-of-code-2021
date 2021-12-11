(ns day9.smoke-basin
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day9/input")]
    (->> (line-seq r)
         (map (comp #(mapv parse-long %) #(str/split % #"")))
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

(def not-neg? (comp not neg?))

(defn in-range?
  [v max-v]
  ((every-pred not-neg? #(< % max-v)) v))

(defn expand-surrounding-points
  [[x y] [max-x max-y]]
  (->> [[dec identity]
        [identity dec]
        [inc identity]
        [identity inc]]
       (map (fn [[fx fy]] [(fx x) (fy y)]))
       (filter (every-pred (comp #(in-range? % max-x) first)
                           (comp #(in-range? % max-y) second)))))

(defn border? [v] (= v 9))
(def not-border? (comp not border?))

(defn find-basin-points
  [heightmap point]
  (let [limit [(-> heightmap first count) (count heightmap)]]
   (reduce (fn [{:keys [search-points result-points] :as acc} _]
             (if-let [p (first search-points)]
               (let [acc+ (-> acc
                              (update :checked-points conj p)
                              (update :search-points #(into [] (next %))))]
                 (if (-> p (point->val heightmap) not-border?)
                   (let [next-points
                         (->> (expand-surrounding-points p limit)
                              (remove result-points))]
                     (-> acc+
                         (update :search-points #(into % next-points))
                         (update :result-points conj p)))
                   acc+))
               (reduced acc)))
           {:search-points [point]
            :checked-points #{}
            :result-points #{}}
           (range))))

(defn all-points
  [heightmap]
  (-> (let [w (count (first heightmap))
            h (count heightmap)]
        (for [x (range 0 w)
              y (range 0 h)]
          [x y]))
      (set)))

(defn find-all-basins
  [heightmap]
  (let [points (all-points heightmap)]
    (-> (reduce
          (fn [{:keys [unchecked-points] :as acc} _]
            (if-let [p (first unchecked-points)]
              (if (-> p (point->val heightmap) border?)
                (update acc :unchecked-points disj p)
                (let [{:keys [checked-points result-points]}
                      (find-basin-points heightmap p)]
                  (-> acc
                      (update :unchecked-points #(apply disj % checked-points))
                      (update :basin-points conj result-points))))
              (reduced acc)))
          {:unchecked-points points
           :basin-points []}
          (range))
        :basin-points)))

(defn part2
  [heightmap]
  (->> (find-all-basins heightmap)
       (map count)
       (sort >)
       (take 3)
       (apply *)))

(comment
  (part2 input-data))
