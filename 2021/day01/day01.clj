(ns day01
  (:require [clojure.string :as str]))

; returns a vector of integers
(defn get-data [filename]
  (as-> filename $
      (slurp $) ;read the file
      (str/split $ #"\n") ;split the string into a vector of strings
      (map read-string $))) ;convert strings to integers

(defn part1 [numbers]
  (->> numbers
    (partition 2 1) ;partition the numbers into pairs
    (reduce
     (fn [accum pair]
       (as-> (apply - (reverse pair)) diff
         (if (> diff 0)
           (inc accum)
           accum)))
     0)))

(defn part2 [numbers]
  (->> numbers
       (partition 3 1) ;parition the numbers into trios
       (map #(apply + %1)) ;sum each trio
       (part1)))

(defn main []
  (as-> (get-data "./data.txt") numbers
    (println
     "PART 1:"
     (part1 numbers)
     "PART 2:"
     (part2 numbers))))

(main)
