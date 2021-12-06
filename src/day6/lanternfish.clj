(ns day6.lanternfish
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.set :refer [rename-keys]]))

(def input-data
  (with-open [r (jio/reader "src/day6/input")]
    (into [] (map parse-long (str/split (first (line-seq r)) #",")))))

(defn days->lfs
  [days]
  (let [freqs (frequencies days)]
    (-> {}
        (into (map #(vector [:og %] (get freqs % 0)) (range 0 7)))
        (into (map #(vector [:ng %] 0) (range 0 9))))))

(defn reset-negative-days
  [lfs]
  (let [og (get lfs [:og -1])
        ng (get lfs [:ng -1])]
    (-> lfs
        (dissoc [:og -1] [:ng -1])
        (merge {[:og 6] (+ og ng)}))))

(defn new-gens-from-neg-lfs
  [lfs]
  {[:ng 8] (apply + (map second (filter (comp neg? second first) lfs)))})

(def decrease-day-mapping
  {[:og 6] [:og 5]
   [:og 5] [:og 4]
   [:og 4] [:og 3]
   [:og 3] [:og 2]
   [:og 2] [:og 1]
   [:og 1] [:og 0]
   [:og 0] [:og -1]
   [:ng 8] [:ng 7]
   [:ng 7] [:ng 6]
   [:ng 6] [:ng 5]
   [:ng 5] [:ng 4]
   [:ng 4] [:ng 3]
   [:ng 3] [:ng 2]
   [:ng 2] [:ng 1]
   [:ng 1] [:ng 0]
   [:ng 0] [:ng -1]})

(defn decrease-days
  [lfs]
  (rename-keys lfs decrease-day-mapping))

(defn next-day
  [lfs]
  (let [lfs- (decrease-days lfs)]
    (merge (reset-negative-days lfs-) (new-gens-from-neg-lfs lfs-))))

(defn next-days
  [lfs n]
  (reduce (fn [lfs _] (next-day lfs)) lfs (range n)))

(defn count-lfs
  [lfs]
  (->> lfs (map second) (apply +)))

(defn solve
  [lf-days n]
  (-> (days->lfs lf-days)
      (next-days n)
      (count-lfs)))

(defn part1
  [lf-days]
  (solve lf-days 80))

(comment
  (part1 input-data)
  )

(defn part2
  [lf-days]
  (solve lf-days 256))

(comment
  (part2 input-data)
  )
