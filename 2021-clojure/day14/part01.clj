(ns day14.part01
  (:require [clojure.string :as str]))

(defn get-starting-string [filename]
  (let [str-data (slurp filename)]
    (first (str/split-lines str-data))))

(defn get-instructions [filename]
  (let [str-data (-> filename (slurp) (str/split #"\n\n") (second))
        lines (str/split-lines str-data)]
    (map
     (fn [line]
       (let [[pair-string new-char] (str/split line #" -> ")]
         {:pair-string pair-string
          :new-char new-char}))
     lines)))

(defn find-insertion-indexes [string pair-string]
  (filter
   (fn [i] (= pair-string (subs string (dec i) (inc i))))
   (range 1 (count string))))

(defn perform-step [instructions string]
  (as-> instructions $
    (map
     (fn [instruction]
       (let [insertion-indexes (find-insertion-indexes string (instruction :pair-string))]
         (map #(assoc instruction :insertion-index %) insertion-indexes)))
     $)
    (apply concat $)
    (sort-by :insertion-index $)
    (concat [{:insertion-index 0, :new-char ""}] $ [{:insertion-index (count string), :new-char ""}])
    (partition 2 1 $)
    (reduce
     (fn [accum [instruction1 instruction2]]
       (str
        accum
        (instruction1 :new-char)
        (subs string (instruction1 :insertion-index) (instruction2 :insertion-index))))
     ""
     $)))

(defn perform-steps [instructions og-string num-iterations]
  (loop [i 0
         string og-string]
    (if (< i num-iterations)
      (recur (inc i) (perform-step instructions string))
      string)))

(defn part1 [instructions og-string]
  (let [final-string (perform-steps instructions og-string 10)]
    (as-> final-string $
         (frequencies $)
         (vals $)
         (- (apply max $) (apply min $)))))

(defn main []
  (let [starting-string (get-starting-string "./data.txt")
        instructions (get-instructions "./data.txt")]
    (println
     "PART 1:"
     (part1 instructions starting-string))))

(main)
