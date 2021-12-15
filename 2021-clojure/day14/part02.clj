(ns day14.part02
  (:require [clojure.string :as str]))

(defn get-starting-string [filename]
  (let [str-data (slurp filename)]
    (first (str/split-lines str-data))))

(defn get-pair-counts [filename]
  (let [starting-string (-> filename (slurp) (str/split-lines) (first))
        pairs (partition 2 1 starting-string)]
    (->> pairs
         (map #(apply str %))
         (frequencies))))

(defn get-char-counts [filename]
  (let [starting-string (-> filename (slurp) (str/split-lines) (first))]
    (-> starting-string
      (str/split #"")
      (frequencies))))

(defn get-instructions [filename]
  (let [str-data (-> filename (slurp) (str/split #"\n\n") (second))
        lines (str/split-lines str-data)]
    (map
     (fn [line]
       (let [[pair new-char] (str/split line #" -> ")]
         {:pair pair
          :new-char new-char
          :new-pairs [(str (first pair) new-char)
                      (str new-char (second pair))]}))
     lines)))

(defn update-pair-counts [instructions og-pair-counts]
  (reduce
   (fn [pair-counts instruction]
     (let [{pair :pair
            new-pairs :new-pairs} instruction]
       (if (og-pair-counts pair)
         (-> pair-counts
             (update pair (fn [old-val] (-' old-val (og-pair-counts pair))))
             (update (first new-pairs) (fn [old-val] (if old-val (+' old-val (og-pair-counts pair)) (og-pair-counts pair))))
             (update (second new-pairs) (fn [old-val] (if old-val (+' old-val (og-pair-counts pair)) (og-pair-counts pair)))))
         pair-counts)))
   og-pair-counts
   instructions))

(defn update-char-counts [instructions og-pair-counts og-char-counts]
  (reduce
   (fn [char-counts instruction]
     (let [{pair :pair
            new-char :new-char} instruction]
       (if (og-pair-counts pair)
         (update char-counts new-char (fn [old-val] (if old-val (+' old-val (og-pair-counts pair)) (og-pair-counts pair))))
         char-counts)))
   og-char-counts
   instructions))

(defn perform-steps [instructions og-pair-counts og-char-counts num-iterations]
  (loop [i 0
         pair-counts og-pair-counts
         char-counts og-char-counts]
    (if (< i num-iterations)
      (recur
       (inc i)
       (update-pair-counts instructions pair-counts)
       (update-char-counts instructions pair-counts char-counts))
      char-counts)))

(defn part2 [instructions og-pair-counts og-char-counts]
  (let [final-char-counts (perform-steps instructions og-pair-counts og-char-counts 40)]
    (as-> final-char-counts $
      (vals $)
      (-' (apply max $) (apply min $)))))

(defn main []
  (let [filename "./data.txt"
        pair-counts (get-pair-counts filename)
        char-counts (get-char-counts filename)
        instructions (get-instructions filename)]
    (println
     "PART 2:"
     (part2 instructions pair-counts char-counts))))

(main)
