(ns day12.part02
  (:require [clojure.string :as str]))

(defn get-graph [filename]
  (let [pair-strings (str/split (slurp filename) #"\n")]
    (loop [i 0
           graph {}]
      (if (< i (count pair-strings))
        (let [[v1 v2] (str/split (nth pair-strings i) #"\-")
              new-graph (-> graph
                            (update v1 (fn [neighbors] (conj (set neighbors) v2)))
                            (update v2 (fn [neighbors] (conj (set neighbors) v1))))]
          (recur (inc i) new-graph))
        graph))))

(defn is-subpath-valid? [subpath]
  (let [freq-map (frequencies subpath)
        vertices (keys freq-map)
        small-caves (->> vertices
                         (filter (fn [vertex] (= vertex (str/lower-case vertex))))
                         (filter (fn [vertex] (not= vertex "start")))
                         (filter (fn [vertex] (not= vertex "end"))))]
    (and
     (= (freq-map "start") 1)
     (->> small-caves (every? #(<= (freq-map %) 2)))
     (<=
      (count (->> small-caves (filter #(>= (freq-map %) 2))))
      1))))

(defn count-number-of-valid-paths [graph subpath]
  (let [last-vertex (last subpath)]
    (if (= last-vertex "end")
      1
      (let [new-vertices (graph last-vertex)
            new-paths (map #(conj (apply vector subpath) %) new-vertices)]
        (->> new-paths
             (filter is-subpath-valid?)
             (map #(count-number-of-valid-paths graph %))
             (apply +))))))

(defn part2 [graph]
  (count-number-of-valid-paths graph ["start"]))

(defn main []
  (let [graph (get-graph "./data.txt")]
    (println
     "PART 2:"
     (part2 graph))))

(main)
