(ns day22.part02
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

(defn has-intersection? [cube1 cube2]
  (and
   (>= (cube1 :x-max) (cube2 :x-min))
   (<= (cube1 :x-min) (cube2 :x-max))
   (>= (cube1 :y-max) (cube2 :y-min))
   (<= (cube1 :y-min) (cube2 :y-max))
   (>= (cube1 :z-max) (cube2 :z-min))
   (<= (cube1 :z-min) (cube2 :z-max))))

(defn get-intersection [cube1 cube2]
  (if (has-intersection? cube1 cube2)
    {:x-min (max (cube1 :x-min) (cube2 :x-min))
     :x-max (min (cube1 :x-max) (cube2 :x-max))
     :y-min (max (cube1 :y-min) (cube2 :y-min))
     :y-max (min (cube1 :y-max) (cube2 :y-max))
     :z-min (max (cube1 :z-min) (cube2 :z-min))
     :z-max (min (cube1 :z-max) (cube2 :z-max))}
    nil))

(defn compute-new-cubes [og-cube-coll og-line]
  (let [start-accum (if (= (og-line :action) :on)
                      [(assoc og-line :sign 1)]
                      [])]
    (reduce
     (fn [accum cube]
       (let [intersection (get-intersection cube og-line)]
         (if intersection
           (conj accum (assoc intersection :sign (- (cube :sign))))
           accum)))
     start-accum
     og-cube-coll)))

(defn build-cube-coll [lines]
  (reduce
   (fn [accum line] 
     (concat accum (compute-new-cubes accum line)))
   []
   lines))

(defn compute-cube-signed-volume [cube]
  (*
   (inc (- (cube :x-max) (cube :x-min)))
   (inc (- (cube :y-max) (cube :y-min)))
   (inc (- (cube :z-max) (cube :z-min)))
   (cube :sign)))

(defn part2 [lines]
  (->> (build-cube-coll lines)
       (map compute-cube-signed-volume)
       (apply +)))

(defn main []
  (let [lines (get-lines "./data.txt")]
    (println
     "PART 2:"
     (part2 lines))))

(main)
