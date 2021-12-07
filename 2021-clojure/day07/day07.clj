(ns day07.day07
  (:require [clojure.string :as str]))

;; returns a collection of integers
(defn get-data [filename]
  (let [str-data (slurp filename)
        string-vector (str/split str-data #",")]
    (map read-string string-vector)))

;; tried a fancy solution for part 1...
(defn part1 [positions]
  (let [target-idx (-> (count positions) (quot 2))
        target-position (nth (sort positions) target-idx)]
    (->> positions
         (map #(Math/abs (- % target-position)))
         (apply +))))

;; for part 2, I couldn't find a fancy solution here...
;; so let's do brute force

;; compute the sum of 1 + 2 + 3 + ... + n
(defn sum-to-n [n]
  (/ (* n (inc n)) 2))
 
(defn compute-required-fuel-singular [target start]
  (let [distance (Math/abs (- target start))]
    (sum-to-n distance)))

(defn compute-required-fuel-total [target positions]
  (apply + (map
            (fn [start] (compute-required-fuel-singular target start))
            positions)))

(defn part2 [positions]
  (let [candidate-targets (range (apply min positions) (inc (apply max positions)))]
    (apply min (map
                (fn [target] (compute-required-fuel-total target positions))
                candidate-targets))))

(defn main []
  (let [positions (get-data "./data.txt")]
    (println
     "PART 1:"
     (part1 positions)
     "PART 2:"
     (part2 positions))))

(main)
