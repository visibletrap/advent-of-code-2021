(ns day14.extended-polymerization
  (:require [clojure.java.io :as jio]
            [clojure.string :as str]))

(def input-data
  (with-open [r (jio/reader "src/day14/input")]
    (let [[[polymer-template] pair-insertion-rules]
          (split-with (comp not #(str/includes? % " ")) (line-seq r))]
      {:polymer-template polymer-template
       :pair-insertion-rules (mapv #(str/split % #" -> ") pair-insertion-rules)})))

(defn rules->insertion-patterns
  [rules]
  (into {}
        (map (juxt (comp vec first)
                   (juxt ffirst (comp first second) (comp second first))))
        rules))

(defn polymerization
  [template insertion-patterns]
  (def template template)
  (let [tuples (into []
                     (map #(get insertion-patterns % %))
                     (mapv vector template (next template)))]
    (-> (into [] (comp (map pop) cat) (pop tuples))
        (into (peek tuples)))))

(defn polymerization-rounds
  [template insertion-patterns n]
  (reduce (fn [t _] (polymerization t insertion-patterns)) template (range n)))

(defn part1
  [{template :polymer-template rules :pair-insertion-rules}]
  (let [insertion-patterns (rules->insertion-patterns rules)
        vs (-> template
               seq
               vec
               (polymerization-rounds insertion-patterns 10)
               frequencies
               vals
               sort)]
    (- (last vs) (first vs))))

(comment
  (part1 input-data)
  )
