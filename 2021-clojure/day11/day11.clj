(ns day11.day11
  (:require [clojure.string :as str]))

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

(defn get-all-coords [matrix]
  (for [i (range (count matrix))
        j (range (count (first matrix)))]
    [i j]))

(defn is-flash-coord? [matrix coord]
  (> (get-in matrix coord) 9))

(defn find-flash-coords [matrix]
  (filter #(is-flash-coord? matrix %) (get-all-coords matrix)))

(defn count-flashes [matrix]
  (let [all-coords (get-all-coords matrix)]
    (->> all-coords
         (filter (fn [coord] (= 0 (get-in matrix coord))))
         (count))))

(defn all-are-flashed? [matrix]
  (let [all-coords (get-all-coords matrix)]
    (every?
     (fn [coord] (= 0 (get-in matrix coord)))
     all-coords)))

(defn increment-matrix-by-1 [matrix]
  (->> matrix
       (map
        (fn [row]
          (->> row
               (map inc)
               (apply vector))))
       (apply vector)))

(defn get-candidate-adjacent-coords [[i j]]
  [[(dec i) j]
   [(inc i) j]
   [i (inc j)]
   [i (dec j)]
   [(inc i) (inc j)]
   [(inc i) (dec j)]
   [(dec i) (inc j)]
   [(dec i) (dec j)]])

(defn get-adjacent-coords [matrix coord]
  (let [i-max (dec (count matrix))
        j-max (dec (count (first matrix)))
        candidates (get-candidate-adjacent-coords coord)]
    (filter
     (fn [[i' j']] (and (<= 0 i' i-max) (<= 0 j' j-max)))
     candidates)))

(defn increment-adjacent-coords [og-matrix coord]
  (let [adjacent-coords (get-adjacent-coords og-matrix coord)]
    (loop [i 0
           matrix og-matrix]
      (if (< i (count adjacent-coords))
        (let [adjacent-coord (nth adjacent-coords i)]
          (if (= 0 (get-in matrix adjacent-coord))
            (recur (inc i) matrix)
            (recur (inc i) (update-in matrix adjacent-coord inc))))
        matrix))))

(defn perform-flash [matrix coord]
  (if (is-flash-coord? matrix coord)
    (as-> matrix $
      (assoc-in $ coord 0)
      (increment-adjacent-coords $ coord)
      (let [adjacent-coords (get-adjacent-coords $ coord)]
        (loop [i 0
               matrix $]
          (if (< i (count adjacent-coords))
            (let [adjacent-coord (nth adjacent-coords i)]
              (recur (inc i) (perform-flash matrix adjacent-coord)))
            matrix))))
    matrix))

(defn perform-step [matrix]
  (let [incremented-matrix (increment-matrix-by-1 matrix)
        flash-coords (find-flash-coords incremented-matrix)]
    (loop [matrix incremented-matrix
           i 0]
      (if (< i (count flash-coords))
        (let [coord (nth flash-coords i)]
          (recur (perform-flash matrix coord) (inc i)))
        matrix))))

(defn part1 [og-matrix]
  (let [num-steps 100]
    (loop [i 0
           matrix og-matrix
           flash-count 0]
      (if (<= i num-steps)
        (let [new-matrix (perform-step matrix)
              new-flash-count (+ flash-count (count-flashes matrix))]
          (recur (inc i) new-matrix new-flash-count))
        flash-count))))

(defn part2 [og-matrix]
  (loop [i 0
         matrix og-matrix]
    (if (all-are-flashed? matrix)
      i
      (recur (inc i) (perform-step matrix)))))

(defn main []
  (let [matrix (get-matrix "./data.txt")]
    (println
     "PART 1:"
     (part1 matrix)
     "PART 2:"
     (part2 matrix))))

(main)
