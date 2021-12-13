(ns day13.day13
  (:require [clojure.string :as str]))

(defn get-coords [filename]
  (let [str-data (-> filename (slurp) (str/split #"\n\n") (first))
        lines (str/split-lines str-data)]
    (map
     (fn [line] (map read-string (str/split line #",")))
     lines)))

(defn get-folds [filename]
  (let [str-data (-> filename (slurp) (str/split #"\n\n") (second))
        str-lines (str/split-lines str-data)]
    (->> str-lines
         (map #(str/replace % #"fold along " ""))
         (map #(str/split % #"="))
         (map (fn [[variable num]] {:variable variable
                                    :num (read-string num)})))))

(defn reflect-x [axis-val coord]
  (let [[x y] coord]
    (if (< x axis-val)
      [x y]
      (let [delta (- x axis-val)
            new-x (- axis-val delta)]
        [new-x y]))))

(defn reflect-y [axis-val coord]
  (let [[x y] coord]
    (if (< y axis-val)
      [x y]
      (let [delta (- y axis-val)
            new-y (- axis-val delta)]
        [x new-y]))))

(defn reflect [fold coord]
  (case (fold :variable)
    "x" (reflect-x (fold :num) coord)
    "y" (reflect-y (fold :num) coord)
    :else coord))

(defn perform-fold [fold coords]
  (->> coords
       (map (partial reflect fold))
       (distinct)))

(defn perform-folds [folds coords-original]
  (loop [i 0
         coords coords-original]
    (if (< i (count folds))
      (let [fold (nth folds i)]
        (recur (inc i) (perform-fold fold coords)))
      coords)))

(defn coords->string-line [coords y]
  (let [coord-set (->> coords (map (fn [[x y]] [x y])) (set))
        xs (map first coord-set)
        x-max (apply max xs)]
    (->> (range (inc x-max))
         (map (fn [x] (if (contains? coord-set [x y]) "#" " ")))
         (str/join ""))))

(defn coords->string [coords]
  (let [ys (map second coords)
        y-max (apply max ys)]
    (->> (range (inc y-max))
         (map (fn [y] (coords->string-line coords y)))
         (str/join "\n"))))

(defn part1 [folds coords]
  (count (perform-fold (first folds) coords)))

(defn part2 [folds coords]
  (let [folded-coords (perform-folds folds coords)]
    (coords->string folded-coords)))

(defn main []
  (let [coords (get-coords "./data.txt")
        folds (get-folds "./data.txt")]
    (println
     "PART 1:"
     (part1 folds coords)
     "PART 2:")
    (println
     (part2 folds coords))))

(main)
