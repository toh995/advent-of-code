(ns day09.day09
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-matrix [filename]
  (let [str-data (slurp filename)] 
    (as-> str-data $
      (str/split str-data #"\n")
      (map
       (fn [row]
         (as-> row $
           (str/split $ #"")
           (map read-string $)
           (apply vector $)))
       $)
      (apply vector $))))

(defn get-candidate-adjacent-coords [[i j]]
  [[(dec i) j]
   [(inc i) j]
   [i (inc j)]
   [i (dec j)]])

(defn get-adjacent-coords [[i j] i-max j-max]
  (let [candidates (get-candidate-adjacent-coords [i j])]
    (filter
     (fn [[i' j']] (and (<= 0 i' i-max) (<= 0 j' j-max)))
     candidates)))

(defn coord-is-minimal? [matrix coord]
  (let [[i j] coord
        i-max (dec (count matrix))
        j-max (dec (count (first matrix)))
        adjacent-coords (get-adjacent-coords [i j] i-max j-max)
        fixed-val (get-in matrix [i j])]
    (every?
     (fn ([[i' j']] (< fixed-val (get-in matrix [i' j']))))
     adjacent-coords)))

(defn compute-risk-level [matrix coord]
  (let [[i j] coord]
    (inc (get-in matrix [i j]))))

(defn adj-coord-in-basin? [matrix old-coord adj-coord]
  (<=
   (get-in matrix old-coord)
   (get-in matrix adj-coord)
   8))

(defn find-basin-set [matrix coord seen]
    (let [i-max (dec (count matrix))
          j-max (dec (count (first matrix)))
          adjacent-coords (get-adjacent-coords coord i-max j-max)
          new-coords (->> adjacent-coords
                          (filter #(not (contains? seen %)))
                          (filter #(adj-coord-in-basin? matrix coord %)))
          new-seen (set/union seen (set new-coords))]
      (if (= (count new-coords) 0)
        new-seen
        (loop [i 0
               new-seen new-seen]
          (if (< i (count new-coords))
            (let [coord (nth new-coords i)]
              (recur (inc i) (find-basin-set matrix coord new-seen)))
            new-seen)))))

(defn find-basin-size [matrix start-coord]
  (count (find-basin-set matrix start-coord #{start-coord})))

(defn part1 [matrix]
  (let [all-coords (for [i (range (count matrix))
                         j (range (count (first matrix)))]
                     [i j])]
    (->> all-coords
         (filter #(coord-is-minimal? matrix %))
         (map #(compute-risk-level matrix %))
         (apply +))))

(defn part2 [matrix]
  (let [all-coords (for [i (range (count matrix))
                         j (range (count (first matrix)))]
                     [i j])]
    (->> all-coords
         (filter #(coord-is-minimal? matrix %))
         (map #(find-basin-size matrix %))
         (sort)
         (reverse)
         (take 3)
         (apply *))))

(defn main []
  (let [matrix (get-matrix "./data.txt")]
    (println
     "PART 1:"
     (part1 matrix)
     "PART 2:"
     (part2 matrix))))

(main)
