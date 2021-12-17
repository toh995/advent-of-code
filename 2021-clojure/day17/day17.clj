(defn sum-to [n]
  (/ (* n (inc n)) 2))

(defn update-step-map [step-map step-nums new-coord]
  (reduce
   (fn [accum step-num]
     (assoc accum step-num (conj (accum step-num #{}) new-coord)))
   step-map
   step-nums))

(defn get-steps-for-y [y-min y-max y]
  (loop [ret []
         step-num 1
         curr-sum y
         to-add (dec y)]
    (if (< curr-sum y-min)
      ret
      (let [new-ret (if (<= y-min curr-sum y-max) (conj ret step-num) ret)
            next-sum (+ curr-sum to-add)
            new-to-add (dec to-add)]
        (recur new-ret (inc step-num) next-sum new-to-add)))))

(defn get-y-complement [y]
  (dec (Math/abs y)))

(defn get-y-step-complement [y step-num]
  (+ step-num (dec (* 2 (Math/abs y)))))

(defn build-y-step-map [y-min y-max]
  (loop [y y-min
         y-step-map {}]
    (if (= 0 y)
      y-step-map
      (let [y-steps (get-steps-for-y y-min y-max y)
            y-steps-complement (map #(get-y-step-complement y %) y-steps)
            y-complement (get-y-complement y)
            new-y-step-map (-> y-step-map
                               (update-step-map y-steps y)
                               (update-step-map y-steps-complement y-complement))]
        (recur (inc y) new-y-step-map)))))

(defn get-steps-for-x [x-min x-max x max-steps]
  (loop [ret []
         step-num 1
         curr-sum x
         to-add (dec x)]
    (if (or (> curr-sum x-max) (> step-num max-steps))
      ret
      (let [new-ret (if (<= x-min curr-sum x-max) (conj ret step-num) ret)
            next-sum (+ curr-sum to-add)
            new-to-add (if (= 0 to-add) 0 (dec to-add))]
        (recur new-ret (inc step-num) next-sum new-to-add)))))

(defn build-x-step-map [x-min x-max max-steps]
  (loop [x x-max
         x-step-map {}]
    (if (< (sum-to x) x-min)
      x-step-map
      (let [x-steps (get-steps-for-x x-min x-max x max-steps)
            new-x-step-map (update-step-map x-step-map x-steps x)]
        (recur (dec x) new-x-step-map)))))

(defn part2 [x-min x-max y-min y-max]
  (let [y-step-map (build-y-step-map y-min y-max)
        max-steps (apply max (keys y-step-map))
        x-step-map (build-x-step-map x-min x-max max-steps)
        step-nums (keys y-step-map)]
    (->> step-nums
         (map (fn [step-num]
                (for [x (x-step-map step-num #{})
                      y (y-step-map step-num #{})]
                  [x y])))
         (apply concat)
         (distinct)
         (count))))

(defn main []
  (let [x-min 135
        x-max 155
        y-min -102
        y-max -78]
    (println
     "PART 2:"
     (part2 x-min x-max y-min y-max))))

(main)
