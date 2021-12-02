(ns day1.sonar-sweep
  (:require [clojure.java.io :as jio]))

;; Part 1
(defn count-larger
  [[start & next-vals]]
  (-> (reduce (fn [{:keys [current-val] :as acc} next-val]
                (-> acc
                    (update :counter (if (> next-val current-val) inc identity))
                    (assoc :current-val next-val)))
              {:current-val start
               :counter 0}
              next-vals)
      :counter))

(count-larger (map parse-long (line-seq (jio/reader "src/day1/input"))))


;; Part 2
(->> (reduce (fn [{:keys [current] :as acc} _]
               (if (seq current)
                 (-> acc
                     (update :result conj (vec (take 3 current)))
                     (update :current rest))
                 (reduced acc)))
             {:current (map parse-long (line-seq (jio/reader "src/day1/input")))
              :result []}
             (range))
     :result
     (filter (comp #{3} count))
     (map #(apply + %))
     count-larger)
