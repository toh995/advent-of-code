(ns day20.day20
  (:require [clojure.string :as str]))

(defn get-reference-string [filename]
  (-> filename
      (slurp)
      (str/split-lines)
      (first)))

(defn get-og-input [filename]
  (let [lines (->> filename (slurp) (str/split-lines) (drop 2) (vec))
        y-max (dec (count lines))
        coords (for [x (range (count (first lines)))
                     y (range (count lines))]
                 [x y])]
    (reduce
     (fn [accum [x y]]
       (let [new-val (get-in lines [(- y-max y) x])]
         (assoc-in accum [x y] new-val)))
     {}
     coords)))

(defn get-box-coords [[x y]]
  [[(dec x) (inc y)]
   [x (inc y)]
   [(inc x) (inc y)]
   [(dec x) y]
   [x y]
   [(inc x) y]
   [(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]])

(defn get-new-pixel-value [reference-string default-val coord input]
  (as-> coord $
    (get-box-coords $)
    (map (fn [coord] (get-in input coord default-val)) $)
    (map
     (fn [char]
       (case char
         \. "0"
         \# "1"))
     $)
    (apply str $)
    (Integer/parseInt $ 2)
    (nth reference-string $)))

(defn enhance-image [reference-string default-val input]
  (let [xs (keys input)
        ys (->> (vals input)
                (map keys)
                (apply concat))
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)
        coords (for [x (range (dec x-min) (+ 2 x-max))
                     y (range (dec y-min) (+ 2 y-max))]
                 [x y])
        num-coords (count coords)]
    (loop [i 0
           output input]
      (if (< i num-coords)
        (let [coord (nth coords i)
              new-pixel-value (get-new-pixel-value reference-string default-val coord input)]
          (recur (inc i) (assoc-in output coord new-pixel-value)))
        output))))

(defn enhance-many-times [reference-string input num-iterations]
  (loop [i 0 
        output input]
    (cond
     (>= i num-iterations) output
     (even? i) (recur (inc i) (enhance-image reference-string \. output))
     (odd? i) (recur (inc i) (enhance-image reference-string \# output)))))

(defn part1 [reference-string input]
  (let [output (enhance-many-times reference-string input 2)
        pixel-values (->> (vals output)
                          (map vals)
                          (apply concat))]
    (->> pixel-values
         (filter #(= % \#))
         (count))))

(defn part2 [reference-string input]
  (let [output (enhance-many-times reference-string input 50)
        pixel-values (->> (vals output)
                          (map vals)
                          (apply concat))]
    (->> pixel-values
         (filter #(= % \#))
         (count))))

(defn main []
  (let [filename "./data.txt"
        reference-string (get-reference-string filename)
        og-input (get-og-input filename)]
    (println
     "PART 1:"
     (part1 reference-string og-input)
     "PART 2:"
     (part2 reference-string og-input))))

(main)
