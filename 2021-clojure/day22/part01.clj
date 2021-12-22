(ns day22.part01
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-coords [line-string]
  (let [coords-string (-> line-string
                          (str/split #" ")
                          (second)
                          (str/replace #"[xyz]=" ""))]
    (as-> coords-string $
      (str/split $ #",")
      (map #(str/split % #"\.\.") $)
      (flatten $)
      (map read-string $))))

(defn get-action [line-string]
  (let [action-string (-> line-string (str/split #" ") (first))]
    (if (= action-string "on") :on :off)))

(defn line-string->map [line-string]
  (let [action (get-action line-string)
        [x-min x-max y-min y-max z-min z-max] (get-coords line-string)]
    {:action action
     :x-min x-min
     :x-max x-max
     :y-min y-min
     :y-max y-max
     :z-min z-min
     :z-max z-max}))

(defn get-lines [filename]
  (->> filename
       (slurp)
       (str/split-lines)
       (map line-string->map)))

(defn part1 [lines]
  (->> lines
       (filter (fn [line]
                 (let [{:keys [x-min x-max y-min y-max z-min z-max]} line]
                   (and
                    (>= x-max -50)
                    (<= x-min 50)
                    (>= y-max -50)
                    (<= y-min 50)
                    (>= z-max -50)
                    (<= z-min 50)))))
       (reduce
        (fn [accum line]
          (let [{:keys [action x-min x-max y-min y-max z-min z-max]} line
                all-points (set (for [x (range (max x-min -50) (inc (min x-max 50)))
                                      y (range (max y-min -50) (inc (min y-max 50)))
                                      z (range (max z-min -50) (inc (min z-max 50)))]
                                  [x y z]))]
            (if (= action :on)
              (set/union accum all-points)
              (set/difference accum all-points))))
        #{})
       (count)))

(defn main []
  (let [lines (get-lines "./data.txt")]
    (println
     "PART 1:"
     (part1 lines))))

(main)
