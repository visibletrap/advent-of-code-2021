(ns day1.sonar-sweep
  (:require [clojure.java.io :as jio]))

(def input (->> (jio/reader "src/day1/input") line-seq (map parse-long)))

(defn window
  [n coll]
  (lazy-seq
    (when (pos? n)
      (when-let [s (seq coll)]
        (cons (take n s) (window n (rest s)))))))

(defn count-larger
  [numbers]
  (->> numbers
       (window 2)
       (filter (comp #{2} count))
       (filter #(apply < %))
       count))

;; Part 1
(count-larger input)

;; Part 2
(->> input
     (window 3)
     (map #(apply + %))
     count-larger)



;; Original implementation

;;; part1
(defn old-count-larger
  [[start & next-vals]]
  (-> (reduce (fn [{:keys [current-val] :as acc} next-val]
                (-> acc
                    (update :counter (if (> next-val current-val) inc identity))
                    (assoc :current-val next-val)))
              {:current-val start
               :counter 0}
              next-vals)
      :counter))

;;; part 2
(->> (reduce (fn [{:keys [current] :as acc} _]
               (if (seq current)
                 (-> acc
                     (update :result conj (vec (take 3 current)))
                     (update :current rest))
                 (reduced acc)))
             {:current input
              :result []}
             (range))
     :result
     (filter (comp #{3} count))
     (map #(apply + %))
     old-count-larger)
