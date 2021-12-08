(ns day08.part01
  (:require [clojure.string :as str]))

(defn get-lines [filename]
  (let [str-data (slurp filename)]
    (str/split str-data #"\n")))

(defn get-outputs [line]
  (-> line
      (str/split #" \| ")
      (second)
      (str/split #" ")))

(defn part1 [lines]
  (let [output-strings (flatten (map get-outputs lines))
        lengths (map count output-strings)]
    (count
     (filter #(some #{%} [2 3 4 7]) lengths))))

(defn main []
  (let [lines (get-lines "./data.txt")]
    (println
     "PART 1:"
     (part1 lines))))

(main)
