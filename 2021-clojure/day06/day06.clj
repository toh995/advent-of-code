(ns day06.day06
  (:require [clojure.string :as str]))

;; returns a collection of integers
(defn get-data [filename]
  (let [str-data (slurp filename)
        string-vector (str/split str-data #",")]
    (map read-string string-vector)))

(defn build-fish-map [fish-coll]
  (let [freq-map (frequencies fish-coll)
        keys (range 9)
        vals (map #(freq-map % 0) keys)]
    (zipmap keys vals)))

(defn compute-new-fish-map [old-fish-map]
  {0 (old-fish-map 1)
   1 (old-fish-map 2)
   2 (old-fish-map 3)
   3 (old-fish-map 4)
   4 (old-fish-map 5)
   5 (old-fish-map 6)
   6 (+ (old-fish-map 7) (old-fish-map 0)) ;0 represents fish that have been reset
   7 (old-fish-map 8)
   8 (old-fish-map 0)}) ;new fish that have been spawned

(defn compute-new-fish-map-repeated [fish-map-og num-repetitions]
  (loop [i 0
         fish-map fish-map-og]
    (if (< i num-repetitions)
      (recur (inc i) (compute-new-fish-map fish-map))
      fish-map)))

(defn part1 [fish-coll]
  (let [fish-map-og (build-fish-map fish-coll)
        num-repetitions 80
        final-fish-map (compute-new-fish-map-repeated fish-map-og num-repetitions)]
    (apply + (vals final-fish-map))))


(defn part2 [fish-coll]
  (let [fish-map-og (build-fish-map fish-coll)
        num-repetitions 256
        final-fish-map (compute-new-fish-map-repeated fish-map-og num-repetitions)]
    (apply + (vals final-fish-map))))

(defn main []
  (let [fish-coll (get-data "./data.txt")]
    (println
     "PART 1:"
     (part1 fish-coll)
     "PART 2:"
     (part2 fish-coll))))

(main)
