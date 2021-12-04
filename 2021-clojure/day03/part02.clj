(ns day03.part02
  (:require [clojure.string :as str]))

;; returns a 2D vector
;; each "inner" vector represents a "row" or binary number
;; EXAMPLE OUTPUT: [["0" "0" "1" "0" "0"] ["1" "1" "1" "1" "0"]]
(defn get-data [filename]
  (as-> filename $
    (slurp $) ;read the file
    (str/split $ #"\n") ;split the string by newline
    (map #(str/split % #"") $))) ;transform each row from a string into a vector of strings

;; EXAMPLE INPUT: ["0" "0" "1" "0" "0"]
;; EXAMPLE OUTPUT: 4
(defn row-to-number [row]
  (as-> row $ 
      (apply str $)
      (Integer/parseInt $ 2)))

(defn compute-row [bit-criterion-fn rows]
   (loop [rows rows
         i 0]
    (let [bit-criterion (bit-criterion-fn i rows)
          filtered-rows (filter #(= (% i) bit-criterion) rows)]
      (if (= (count filtered-rows) 1)
        (first filtered-rows)
        (recur filtered-rows (inc i))))))

(defn compute-value [bit-criterion-fn rows]
  (->> rows
      (compute-row bit-criterion-fn)
      (row-to-number)))

(defn oxygen-bit-criterion [i rows]
  (let [column (map #(% i) rows)
        freq-map (frequencies column)]
    (cond
      (= (freq-map "0") (freq-map "1")) "1"
      (> (freq-map "0") (freq-map "1")) "0"
      :else "1")))

(defn co2-bit-criterion [i rows]
  (let [column (map #(% i) rows)
        freq-map (frequencies column)]
    (cond
      (= (freq-map "0") (freq-map "1")) "0"
      (> (freq-map "0") (freq-map "1")) "1"
      :else "0")))

(defn part2 [rows]
  (let [oxygen-val (compute-value oxygen-bit-criterion rows)
        co2-val (compute-value co2-bit-criterion rows)]
    (* oxygen-val co2-val)))

(defn main []
  (let [rows (get-data "./data.txt")]
    (println
     "PART 2:"
     (part2 rows))))
(main)
