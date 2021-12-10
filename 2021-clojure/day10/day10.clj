(ns day10.part01
  (:require [clojure.string :as str]))

(defn get-lines [filename]
  (let [str-data (slurp filename)]
    (str/split str-data #"\n")))

(defn is-start-char? [char]
  (contains? #{\( \[ \{ \<} char))

(defn is-end-char? [char]
  (contains? #{\) \] \} \>} char))

(defn chars-match? [char1 char2]
  (case char1
    \( (= char2 \))
    \[ (= char2 \])
    \{ (= char2 \})
    \< (= char2 \>)
    :else false))

(defn start-char->end-char [start-char]
  (case start-char
    \( \)
    \[ \]
    \{ \}
    \< \>
    :else false))

(defn first-illegal-char [line]
  (loop [i 0
         start-chars ()]
    (if (< i (count line))
      (let [char (nth line i)]
        (cond
          (is-start-char? char) (recur (inc i) (conj start-chars char))
          (is-end-char? char) (if (chars-match? (peek start-chars) char)
                                (recur (inc i) (pop start-chars))
                                char)
          :else nil))
      nil)))

(defn compute-orphan-start-chars [line]
  (loop [i 0
         start-chars ()]
    (if (< i (count line))
      (let [char (nth line i)]
        (cond
          (is-start-char? char) (recur (inc i) (conj start-chars char))
          (is-end-char? char) (if (chars-match? (peek start-chars) char)
                                (recur (inc i) (pop start-chars))
                                nil)
          :else nil))
      start-chars)))

(defn compute-missing-end-chars [line]
  (let [orphan-start-chars (compute-orphan-start-chars line)]
    (if orphan-start-chars
      (map start-char->end-char orphan-start-chars)
      nil)))

(defn compute-char-points-part1 [char]
  (case char
    \) 3
    \] 57
    \} 1197
    \> 25137))

(defn compute-char-value-part2 [char]
  (case char
    \) 1
    \] 2
    \} 3
    \> 4))

(defn compute-char-points-part2 [chars]
  (reduce
   (fn [accum char] (+ (* accum 5) (compute-char-value-part2 char)))
   0
   chars))

(defn part1 [lines]
  (->> lines
       (map first-illegal-char)
       (filter identity)
       (map compute-char-points-part1)
       (apply +)))

(defn part2 [lines]
  (as-> lines $
    (map compute-missing-end-chars $)
    (filter identity $)
    (map compute-char-points-part2 $)
    (sort $)
    (nth $ (quot (count $) 2))))

(defn main []
  (let [lines (get-lines "./data.txt")]
    (println
     "PART 1:"
     (part1 lines)
     "PART 2:"
     (part2 lines))))

(main)
