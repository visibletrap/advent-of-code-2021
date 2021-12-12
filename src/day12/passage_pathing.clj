(ns day12.passage-pathing
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]
            [clojure.zip :as z]))

(def input-data
  (with-open [r (jio/reader "src/day12/input")]
    (->> (line-seq r)
         (mapv #(str/split % #"-")))))

(defn index-paths
  [paths]
  (let [fwd (->> paths
                 (group-by first)
                 (#(update-vals % (partial mapv second))))
        rev (->> paths
                 (remove (some-fn (comp #{"start"} first)
                                  (comp #{"end"} second)))
                 (map (juxt second first))
                 (group-by first)
                 (#(update-vals % (partial mapv second))))]
    (-> (merge-with into fwd rev)
        (assoc "end" nil))))

(defn part1
  [paths]
  (-> (reduce (fn [{:keys [edges queue] :as acc} _]
                (if-let [{:keys [n path visited]} (first queue)]
                  (let [path+ (conj path n)
                        visited+ (if (Character/isLowerCase (first n))
                                   (conj visited n)
                                   visited)
                        acc- (update acc :queue (comp vec next))]
                    (if-let [ns (seq (get edges n))]
                      (let [next-visits
                            (->> ns
                                 (remove visited)
                                 (map #(hash-map :n % :path path+ :visited visited+)))]
                        (update acc- :queue into next-visits))
                      (update acc- :results conj path+)))
                  (reduced acc)))
              {:edges (index-paths paths)
               :queue [{:n "start" :path [] :visited #{}}]
               :results []}
              (range 1000000))
      :results
      count))

(comment
  (part1 input-data)
  )