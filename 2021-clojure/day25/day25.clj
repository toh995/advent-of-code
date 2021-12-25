(ns day25.day25
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-matrix [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (map vec)
       (vec)))

(defn get-east-herd [matrix]
  (let [coords (for [i (range (count matrix))
                     j (range (count (first matrix)))]
                 [i j])]
    (->> coords
         (filter (fn [coord] (= \> (get-in matrix coord))))
         (set))))

(defn get-south-herd [matrix]
  (let [coords (for [i (range (count matrix))
                     j (range (count (first matrix)))]
                 [i j])]
    (->> coords
         (filter (fn [coord] (= \v (get-in matrix coord))))
         (set))))

(defn move-east [matrix east-herd south-herd]
  (let [all-herds (set/union east-herd south-herd)]
    (reduce
     (fn [accum coord]
       (let [[i j] coord
             new-coord [i (mod (inc j) (count (first matrix)))]]
         (if (contains? all-herds new-coord)
           (conj accum coord)
           (conj accum new-coord))))
     #{}
     east-herd)))

(defn move-south [matrix east-herd south-herd]
  (let [all-herds (set/union east-herd south-herd)]
    (reduce
     (fn [accum coord]
       (let [[i j] coord
             new-coord [(mod (inc i) (count matrix)) j]]
         (if (contains? all-herds new-coord)
           (conj accum coord)
           (conj accum new-coord))))
     #{}
     south-herd)))

(defn part1 [matrix]
    (loop [i 1
           east-herd (get-east-herd matrix)
           south-herd (get-south-herd matrix)]
      (let [new-east-herd (move-east matrix east-herd south-herd)
            new-south-herd (move-south matrix new-east-herd south-herd)]
        (if (and
             (= east-herd new-east-herd)
             (= south-herd new-south-herd))
          i
          (recur (inc i) new-east-herd new-south-herd)))))

(defn main []
  (let [matrix (get-matrix "./data.txt")]
    (println
     (part1 matrix))))

(main)
