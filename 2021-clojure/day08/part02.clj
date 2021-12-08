(ns day08.part02
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn get-lines [filename]
  (let [str-data (slurp filename)]
    (str/split str-data #"\n")))

;; SETS
(defn get-one-set [sets]
  (->> sets
       (filter #(= (count %) 2))
       (first)))

(defn get-four-set [sets]
  (->> sets
       (filter #(= (count %) 4))
       (first)))

(defn get-six-set [sets top top-left middle bottom-left bottom]
  (->> sets
       (filter #(= (count %) 6))
       (filter #(set/superset? % #{top top-left middle bottom-left bottom}))
       (first)))

(defn get-seven-set [sets]
  (->> sets
       (filter #(= (count %) 3))
       (first)))

(defn get-nine-set [sets top]
  (let [four-set (get-four-set sets)]
    (->> sets
         (filter #(= (count %) 6))
         (filter #(set/subset? (set/union four-set #{top}) %))
         (first))))

(defn get-zero-set [sets top bottom bottom-left]
  (let [one-set (get-one-set sets)]
    (->> sets
         (filter #(= (count %) 6))
         (filter #(set/subset? (set/union one-set #{top bottom bottom-left}) %))
         (first))))

;; LETTERS
(defn get-top [sets]
  (let [one-set (get-one-set sets)
        seven-set (get-seven-set sets)]
    (first (set/difference seven-set one-set))))

(defn get-bottom [sets top]
  (let [four-set (get-four-set sets)
        nine-set (get-nine-set sets top)]
    (first (set/difference nine-set (set/union four-set #{top})))))

(defn get-bottom-left [sets top all-letters]
  (let [nine-set (get-nine-set sets top)]
    (first (set/difference all-letters nine-set))))

(defn get-top-left [sets top bottom bottom-left]
  (let [one-set (get-one-set sets)
        zero-set (get-zero-set sets top bottom bottom-left)]
    (first (set/difference zero-set (set/union one-set #{top bottom bottom-left})))))

(defn get-middle [sets top bottom bottom-left all-letters]
  (let [zero-set (get-zero-set sets top bottom bottom-left)]
    (first (set/difference all-letters zero-set))))
   
(defn get-top-right [sets top top-left middle bottom-left bottom]
  (let [one-set (get-one-set sets)
        six-set (get-six-set sets top top-left middle bottom-left bottom)]
    (first (set/difference one-set six-set))))
 
(defn get-bottom-right [sets top-right]
  (let [one-set (get-one-set sets)]
    (first (set/difference one-set #{top-right}))))

;; UTILITY FUNCTIONS
(defn build-digit-map [sets]
  (let [all-letters #{\a \b \c \d \e \f \g}
        top (get-top sets)
        bottom (get-bottom sets top)
        bottom-left (get-bottom-left sets top all-letters)
        top-left (get-top-left sets top bottom bottom-left)
        middle (get-middle sets top bottom bottom-left all-letters)
        top-right (get-top-right sets top top-left middle bottom-left bottom)
        bottom-right (get-bottom-right sets top-right)]
    {#{top top-left bottom-left bottom bottom-right top-right} "0"
     #{top-right bottom-right} "1"
     #{top top-right middle bottom-left bottom} "2"
     #{top top-right middle bottom-right bottom} "3"
     #{top-left middle top-right bottom-right} "4"
     #{top top-left middle bottom-right bottom} "5"
     #{top top-left middle bottom-left bottom bottom-right} "6"
     #{top top-right bottom-right} "7"
     #{top top-left middle bottom-left bottom bottom-right top-right} "8"
     #{top top-left middle bottom bottom-right top-right} "9"}))

(defn string-to-sets [string]
  (let [substrings (str/split string #" ")]
    (map set substrings)))

(defn remove-leading-zeros [string]
  (if (= (first string) \0)
    (recur (subs string 1))
    string))

(defn compute-number [line]
  (let [[left-str right-str] (str/split line #" \| ")
        left-sets (string-to-sets left-str)
        right-sets (string-to-sets right-str)
        digit-map (build-digit-map left-sets)
        number-string (apply str (map digit-map right-sets))]
    (read-string (remove-leading-zeros number-string))))

(defn part2 [lines] 
  (apply + (map compute-number lines)))

(defn main []
  (let [lines (get-lines "./data.txt")]
    (println
     "PART 2:"
     (part2 lines))))

(main)
