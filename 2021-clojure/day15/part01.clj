(ns day15.part01
  (:require [clojure.string :as str]))

(defn get-matrix [filename]
  (let [str-data (slurp filename)]
    (->> str-data
      (str/split-lines)
      (map
       (fn [row]
         (as-> row $
           (str/split $ #"")
           (map read-string $)
           (apply vector $))))
      (apply vector))))

(defn get-all-coords [matrix]
  (let [i-upper-bound (count matrix)
        j-upper-bound (count (first matrix))]
    (for [i (range i-upper-bound)
          j (range j-upper-bound)]
      [i j])))

(defn compute-initial-distances [matrix start-coord]
  (let [keys (get-all-coords matrix)
        vals (repeat ##Inf)]
    (-> (zipmap keys vals)
        (assoc start-coord 0))))

(defn get-candidate-neighbors [[i j]]
  [[(dec i) j]
   [(inc i) j]
   [i (inc j)]
   [i (dec j)]])

(defn get-neighbors [i-max j-max coord]
  (let [candidates (get-candidate-neighbors coord)]
    (filter
     (fn [[i' j']] (and (<= 0 i' i-max) (<= 0 j' j-max)))
     candidates)))

;; DIJSKTRA FUNCTIONS
(defn dikjstra-update-distances [matrix i-max j-max current-coord distances unvisited-set]
  (let [neighbors (get-neighbors i-max j-max current-coord)]
    (->> neighbors
         (filter #(contains? unvisited-set %))
         (reduce
          (fn [distances neighbor]
            (update distances neighbor (fn [old-val]
                                         (min
                                          old-val
                                          (+ (distances current-coord) (get-in matrix neighbor))))))
          distances))))

(defn dijkstra [i
                matrix
                i-max
                j-max
                target-coord
                current-coord
                distances
                unvisited-set]
  (if (= i 250);(= current-coord target-coord)
    (distances current-coord)
    (let [new-distances (dikjstra-update-distances matrix i-max j-max current-coord distances unvisited-set)
          updated-unvisited (disj unvisited-set current-coord)
          next-coord (apply min-key distances updated-unvisited)]
      (recur
       (inc i)
       matrix
       i-max
       j-max
       target-coord
       next-coord
       new-distances
       updated-unvisited))))

(defn part1 [matrix]
  (let [start-coord [0 0]
        i-max (dec (count matrix))
        j-max (dec (count (first matrix)))
        target-coord [(dec i-max) (dec j-max)]
        initial-distances (compute-initial-distances matrix start-coord)
        initial-unvisited-set (set (get-all-coords matrix))]
    (dijkstra
     0
     matrix
     i-max
     j-max
     target-coord
     start-coord
     initial-distances
     initial-unvisited-set)))

;; END DIJSKTRA FUNCTIONS


(defn main []
  (let [matrix (get-matrix "./data.txt")]
    (println
     "PART 1:"
     (part1 matrix))))

(main)
