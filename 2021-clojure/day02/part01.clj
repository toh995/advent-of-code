(ns day02.part01
  (:require [clojure.string :as str]))

;; returns a 2D vector
;; each "inner" vector represents an instruction
;; EXAMPLE OUTPUT: [["forward" 5] ["down" 5]]
(defn get-data [filename]
  (as-> filename $
    (slurp $) ;read the file
    (str/split $ #"\n") ;split the string by newline
    (map #(str/split % #" ") $) ;create inner vectors
    (map #(update % 1 read-string) $))) ;in each inner vector, coerce the second entry from string to integer

;; returns a map of the final position
;; EXAMPLE OUTPUT: {:horizontal 2033, :depth 750}
(defn compute-final-position [instructions]
  (reduce
   (fn [accum [direction distance]]
     (case direction
       "forward" (update accum :horizontal (partial + distance))
       "down" (update accum :depth (partial + distance))
       "up" (update accum :depth (partial + (- distance)))))
   {:horizontal 0, :depth 0}
   instructions))

 (defn compute-final-answer [instructions]
   (as-> instructions $
     (compute-final-position $)
     (* ($ :horizontal) ($ :depth))))

(defn main []
  (as-> (get-data "./data.txt") instructions
    (println
     "PART 1:"
     (compute-final-answer instructions))))

(main)
