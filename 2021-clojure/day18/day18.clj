(ns day18.day18
  (:require [clojure.string :as str]))

(defn get-lines [filename]
  (-> filename
      (slurp)
      (str/split-lines)))

(defn get-number-at [string i]
  (->> (subs string i)
       (take-while #(Character/isDigit %))
       (apply str)
       (read-string)))

(defn get-number-at-reverse [string i]
  (->> (subs string 0 (inc i))
       (vec)
       (rseq)
       (take-while #(Character/isDigit %))
       (vec)
       (rseq)
       (apply str)
       (read-string)))

(defn explode-build-left-string [string i]
  (let [left-substring (subs string 0 (dec i))
        old-num-idx (->> left-substring
                        (vec)
                        (rseq)
                        (take-while #(not (Character/isDigit %)))
                        (count)
                        (- (count left-substring))
                        (dec))]
    (if (= -1 old-num-idx)
      left-substring
      (let [old-num (get-number-at-reverse left-substring old-num-idx)
            old-num-length (count (str old-num))
            addend (get-number-at string i)
            new-num (+ old-num addend)
            left (subs left-substring 0 (inc (- old-num-idx old-num-length)))
            right (subs left-substring (inc old-num-idx))]
        (str left new-num right)))))

(defn explode-build-right-string [string i]
  (let [left-num (get-number-at string i)
        left-num-length (count (str left-num))
        right-num (get-number-at string (+ i left-num-length 1))
        right-num-length (count (str right-num))
        right-substring (subs string (+ i left-num-length right-num-length 2))
        old-num-idx (->> right-substring
                         (take-while #(not (Character/isDigit %)))
                         (count))]
    (if (= old-num-idx (count right-substring))
      right-substring
      (let [old-num (get-number-at right-substring old-num-idx)
            old-num-length (count (str old-num))
            new-num (+ old-num right-num)
            left (subs right-substring 0 old-num-idx)
            right (subs right-substring (+ old-num-idx old-num-length))]
        (str left new-num right)))))

(defn explode [string i]
  (let [left-string (explode-build-left-string string i)
        mid-string "0"
        right-string (explode-build-right-string string i)]
    (str left-string mid-string right-string)))

(defn split-build-mid-string [num]
  (let [left-num (int (Math/floor (/ num 2)))
        right-num (int (Math/ceil (/ num 2)))]
    (str "[" left-num "," right-num "]")))

(defn split [string i]
  (let [num (get-number-at string i)
        num-length (count (str num))
        left-string (subs string 0 i)
        mid-string (split-build-mid-string num)
        right-string (subs string (+ i num-length))]
    (str left-string mid-string right-string)))

(defn get-explode-index [string]
  (let [string-length (count string)]
    (loop [i 0
           num-nested-pairs 0]
      (if (>= i string-length)
        nil
        (let [char (nth string i)]
          (cond
            (= char \[) (recur (inc i) (inc num-nested-pairs))
            (= char \]) (recur (inc i) (dec num-nested-pairs))
            (= char \,) (recur (inc i) num-nested-pairs)
            (= num-nested-pairs 5) i
            :else (let [num (get-number-at string i)
                        num-length (count (str num))]
                    (recur (+ i num-length) num-nested-pairs))))))))

(defn get-split-index [string]
  (let [string-length (count string)]
    (loop [i 0]
      (if (>= i string-length)
        nil
        (let [char (nth string i)]
          (if (not (Character/isDigit char))
            (recur (inc i))
            (let [num (get-number-at string i)
                  num-length (count (str num))]
              (if (>= num 10) i (recur (+ i num-length))))))))))

(defn reduce-snail-number [og-string]
  (loop [string og-string]
    (let [explode-idx (get-explode-index string)]
      (if explode-idx
        (recur (explode string explode-idx))
        (let [split-idx (get-split-index string)]
          (if split-idx
            (recur (split string split-idx))
            string))))))

(defn sum-snail-numbers [strings]
  (reduce
   (fn [accum new-string]
     (let [new-snail-number (str "[" accum "," new-string "]")]
       (reduce-snail-number new-snail-number)))
   strings))

(defn compute-magnitude [snail-number]
  (cond
    (int? snail-number) snail-number
    (coll? snail-number) (let [[left right] snail-number]
                           (+
                            (* 3 (compute-magnitude left))
                            (* 2 (compute-magnitude right))))))

(def compute-magnitude-memoized (memoize compute-magnitude))

(defn part1 [lines]
  (let [reduced-string (sum-snail-numbers lines)
        final-snail-number (-> reduced-string (str/replace #"," " ") (read-string))]
    (compute-magnitude final-snail-number)))

(defn part2 [lines]
  (let [pairs (for [x lines
                    y lines
                    :when (not= x y)]
                [x y])]
    (->> pairs
         (map sum-snail-numbers)
         (map #(str/replace % #"," " "))
         (map read-string)
         (map compute-magnitude-memoized)
         (apply max))))

(defn main []
  (let [lines (get-lines "./data.txt")]
    (println
     "PART 1:"
     (part1 lines)
     "PART 2:"
     (part2 lines))))

(main)
