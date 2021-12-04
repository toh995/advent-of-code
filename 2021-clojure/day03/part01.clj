(ns day03.part01
  (:require [clojure.string :as str]))

;; returns a vector of strings, where each string represents a row in the data
;; EXAMPLE OUTPUT: ["011001101000" "010101111100"]
(defn get-data [filename]
  (as-> filename $
    (slurp $) ;read the file
    (str/split $ #"\n"))) ;split the string by newline

;; EXAMPLE INPUT: ["00100" "11110"]
;; EXAMPLE OUTPUT: [["0" 1"] ["0" "1"] ["1" "1"] ["0" "1"] ["0" "0"]]
(defn rows-to-columns [rows]
  (->> rows
       (map #(str/split % #""))
       (apply map vector)))

;; returns the most frequent entry in the given collection
(defn most-frequent-entry [coll]
  (key
   (apply max-key val (frequencies coll))))

;; returns the least frequent entry in the given collection
(defn least-frequent-entry [coll]
  (key
   (apply min-key val (frequencies coll))))

(defn compute-gamma [columns]
  (as-> columns $
    (map most-frequent-entry $)
    (apply str $) ;combine the vector of strings into one string
    (Integer/parseInt $ 2))) ;parse the string of 0's and 1's into a number

(defn compute-epsilon [columns]
  (as-> columns $
    (map least-frequent-entry $)
    (apply str $) ;combine the vector of strings into one string
    (Integer/parseInt $ 2))) ;parse the string of 0's and 1's into a number

(defn part1 [rows]
  (let [columns (rows-to-columns rows)
        gamma (compute-gamma columns)
        epsilon (compute-epsilon columns)]
    (* gamma epsilon)))

(defn main []
  (as-> (get-data "./data.txt") rows
    (println
     "PART 1:"
     (part1 rows))))
(main)
