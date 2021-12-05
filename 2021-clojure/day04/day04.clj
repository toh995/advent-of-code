(ns day04.day04
  (:require [clojure.string :as str]))

(defn matrix-string-to-matrix [matrix-string]
  (as-> matrix-string $
    (str/split $ #"\n")
    (map str/trim $)
    (map #(str/split % #" +") $)
    (map (partial map read-string) $) ;coerce strings to integer
    (map #(apply vector %) $) ;coerce to vector
    (apply vector $))) ;coerce to vector

(defn str-data-to-matrices [str-data]
  (as-> str-data $
    (subs $ (+ 2 (str/index-of str-data "\n"))) ;discard the first part of str-data, which corresponds to "moves"
    (str/split $ #"\n\n") ;create a vector of matrices, in string format
    (map matrix-string-to-matrix $)))

(defn matrix-to-board [matrix]
  {:matrix matrix
   :unmarked-sum (->> matrix ;sum all entries in the given matrix
                      (map #(apply + %)) ;sum each row
                      (apply +)) ;sum up the sums
   :row-counts {0 0, 1 0, 2 0, 3 0, 4 0}
   :col-counts {0 0, 1 0, 2 0, 3 0, 4 0}
   :is-winner false})

;; returns a collection of boards
(defn parse-boards [str-data]
  (->> (str-data-to-matrices str-data)
       (map matrix-to-board)))

;; returns a collection of numbers
(defn parse-moves [str-data]
  (as-> str-data $
      (subs $ 0 (str/index-of str-data "\n"))
      (str/split $ #",")
      (map read-string $) ;coerce strings to numbers
      (apply vector $))) ;coerce to vector

(defn find-index-matches [move board]
  (for [i (range 5)
        j (range 5)
        :when (= move (get-in board [:matrix i j]))]
    [i j]))

(defn update-board [move board]
  (if-let [[i j] (first (find-index-matches move board))]
    ;; If we found a match, then update the board accordingly
    (as-> board board
      (update board :unmarked-sum (partial + (- move)))
      (update-in board [:row-counts i] inc)
      (update-in board [:col-counts j] inc)
      (assoc board :is-winner (or
                               (= 5 (get-in board [:row-counts i]))
                               (= 5 (get-in board [:col-counts j])))))
    ;; Otherwise, just return the board without updating
    board))

(defn compute-winning-val [winning-board move]
  (* move (winning-board :unmarked-sum)))

(defn part1 [moves og-boards]
  (loop [boards og-boards
         k 0]
    (let [move (moves k)
          updated-boards (map (partial update-board move) boards)]
      (if-let [winning-board (->> updated-boards (filter :is-winner) first)]
        (compute-winning-val winning-board move)
        (recur updated-boards (inc k))))))

(defn part2 [moves og-boards]
  (loop [boards og-boards
         k 0]
    (let [move (moves k)
          updated-boards (map (partial update-board move) boards)
          filtered-boards (filter (complement :is-winner) updated-boards)]
      (if (= 0 (count filtered-boards))
        (compute-winning-val (first updated-boards) move)
        (recur filtered-boards (inc k))))))

(defn main []
  (let [str-data (slurp "./data.txt")
        moves (parse-moves str-data)
        og-boards (parse-boards str-data)]
    ;; (println (map #(count (first (% :matrix))) og-boards))))
    (println
     "PART 1:"
     (part1 moves og-boards)
     "PART 2:"
     (part2 moves og-boards))))

(main)
