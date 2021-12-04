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
  (let [all-numbers (into #{} (apply concat raw-board))]
    {:win-groups (into raw-board (apply map vector raw-board))
     :all-numbers all-numbers
     :marked-numbers []
     :unmarked-numbers all-numbers}))

(defn contains-number?
  [board n]
  (contains? (:all-numbers board) n))

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
  (let [new-state
        (-> state
            (update :boards (fn [bs] (mapv #(apply-drawn-number-to-board % dn) bs)))
            (update :drawn-numbers conj dn))]
    (if (seq (find-win-boards (:boards new-state)))
      (reduced new-state)
      new-state)))

(defn play-until-win
  [{state :state all-drawn-numbers :drawn-numbers}]
  (reduce apply-drawn-number state all-drawn-numbers))

(defn init-state
  [raw-boards]
  {:drawn-numbers []
   :boards (mapv index-board raw-boards)})

(defn part1
  [data]
  (let [state (play-until-win (assoc data :state (init-state (:raw-boards data))))
        {:keys [boards drawn-numbers]} state]
    (* (->> boards
            (find-win-boards)
            first
            :unmarked-numbers
            (apply +))
       (peek drawn-numbers))))

(comment
  (part1 input-data)
  )
