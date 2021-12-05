(ns day05.day05
  (:require [clojure.string :as str]))

;; returns a 3D vector
;; EXAMPLE INPUT:
;; "0,9 -> 5,9
;; 8,0 -> 0,8"
;;
;; EXAMPLE OUTPUT:
;; [
;;   [[0 9] [5 9]]
;;   [[8 0] [0 8]]
;; ]
(defn get-data [filename]
  (let [str-data (slurp filename)
        lines (str/split str-data #"\n")]
    (->> lines
         (map (fn [line]
                (let [points (str/split line #" -> ")]
                  (->> points
                       (map (fn [point] (str/split point #",")))
                       (map (fn [[x y]] [(read-string x) (read-string y)]))))))))) ;; coerce strings to numbers

;; return a collection of points representing the given line
(defn compute-points [line]
  ;; ensure that x1 < x2.
 (let [[[x1-og y1-og] [x2-og y2-og]] line
       x1 (if (<= x1-og x2-og) x1-og x2-og)
       y1 (if (<= x1-og x2-og) y1-og y2-og)
       x2 (if (<= x1-og x2-og) x2-og x1-og)
       y2 (if (<= x1-og x2-og) y2-og y1-og)]
   (cond
    ;; horizontal/vertical lines
    (or (= x1 x2) (= y1 y2)) (for [x (range (min x1 x2) (inc (max x1 x2)))
                                   y (range (min y1 y2) (inc (max y1 y2)))]
                               [x y])
    ;; diagonal line, y increasing
    (< y1 y2) (let [xs (range x1 (inc x2))
                    ys (range y1 (inc y2))]
                (map vector xs ys))
    ;; diagonal line, y decreasing
    (> y1 y2) (let [xs (range x1 (inc x2))
                    ys (range y1 (dec y2) -1)]
                (map vector xs ys)))))

(defn part1 [lines]
  (->> lines
       ;; filter for only horizontal or vertical lines
       (filter
        (fn [[[x1 y1] [x2 y2]]]
          (or (= x1 x2) (= y1 y2))))
       (map compute-points)
       (apply concat)
       (frequencies)
       (filter (fn [[_ val]] (>= val 2)))
       (count)))

(defn part2 [lines]
  (->> lines
       (map compute-points)
       (apply concat)
       (frequencies)
       (filter (fn [[_ val]] (>= val 2)))
       (count)))

(defn main []
  (let [lines (get-data "./data.txt")]
    (println
     "PART 1:"
     (part1 lines)
     "PART 2:"
     (part2 lines))))

(main)
