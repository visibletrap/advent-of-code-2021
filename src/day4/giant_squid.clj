(ns day4.giant-squid
  (:require [clojure.string :as str]))

(def input-data
  (let [[l1 & other-lines] (str/split (slurp "src/day4/input") #"\n\n")]
    {:drawn-numbers (mapv parse-long (str/split l1 #","))
     :raw-boards (mapv (comp (fn [lines]
                               (mapv (comp #(mapv parse-long %)
                                           #(str/split % #"\s+")
                                           str/trim)
                                     lines))
                             #(str/split % #"\n"))
                       other-lines)}))

(defn index-board
  [raw-board]
  {:win-groups (into raw-board (apply map vector raw-board))
   :marked-numbers []
   :unmarked-numbers (into #{} (apply concat raw-board))})

(defn contains-number?
  [board n]
  (contains? (:unmarked-numbers board) n))

(defn apply-drawn-number-to-board
  [board n]
  (if (contains-number? board n)
    (let [new-marked-numbers (conj (:marked-numbers board) n)]
     (-> board
         (assoc :marked-numbers new-marked-numbers)
         (update :unmarked-numbers disj n)))
    board))

(defn get-win-group
  [board]
  (some #(when (every? (set (:marked-numbers board)) %) %) (:win-groups board)))

(defn win?
  [board]
  (boolean (get-win-group board)))

(defn find-win-boards
  [boards]
  (filter win? boards))

(defn apply-drawn-number
  [state dn]
  (-> state
      (update :boards (fn [bs] (mapv #(apply-drawn-number-to-board % dn) bs)))
      (update :drawn-numbers conj dn)))

(defn play-until-first-win
  [{state :state all-drawn-numbers :drawn-numbers}]
  (reduce (fn [state dn]
            (let [new-state (apply-drawn-number state dn)]
              (if (seq (find-win-boards (:boards new-state)))
                (reduced new-state)
                new-state)))
          state
          all-drawn-numbers))

(defn init-state
  [raw-boards]
  {:drawn-numbers []
   :boards (mapv index-board raw-boards)})

(defn cal-output
  [state]
  (let [b (first (find-win-boards (:boards state)))
        {:keys [unmarked-numbers marked-numbers]} b]
    (* (apply + unmarked-numbers) (peek marked-numbers))))

(defn part1
  [{:keys [raw-boards drawn-numbers]}]
  (-> {:state (init-state raw-boards) :drawn-numbers drawn-numbers}
      play-until-first-win
      cal-output))

(comment
  (part1 input-data)
  )

(defn remove-win-boards
  [boards]
  (into [] (remove win? boards)))

(defn play-until-last-win
  [{state :state all-drawn-numbers :drawn-numbers}]
  (reduce (fn [state dn]
            (let [{:keys [boards] :as new-state} (apply-drawn-number state dn)]
              (if (and (= (count boards) 1) (win? (first boards)))
                (reduced new-state)
                (update new-state :boards remove-win-boards))))
          state
          all-drawn-numbers))

(defn part2
  [{:keys [raw-boards drawn-numbers]}]
  (-> {:state (init-state raw-boards) :drawn-numbers drawn-numbers}
      play-until-last-win
      cal-output))

(comment
  (part2 input-data)
  )
