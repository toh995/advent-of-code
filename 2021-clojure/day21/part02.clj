(ns day21.part01)

(def possible-dice-rolls (for [x [1 2 3]
                               y [1 2 3]
                               z [1 2 3]]
                           (+ x y z)))

(def dice-roll-frequencies (frequencies possible-dice-rolls))

(defn num-p1-wins [p1-score p2-score p1-pos p2-pos turn]
  (cond
    (>= p1-score 21) 1
    (>= p2-score 21) 0
    :else (cond
            (= turn 1) (->> dice-roll-frequencies
                            (map (fn [[roll-sum freq]]
                                   (let [new-space (mod (+ roll-sum p1-pos) 10)
                                         p1-new-pos (if (= new-space 0) 10 new-space)
                                         p1-new-score (+ p1-score p1-new-pos)]
                                     (* freq (num-p1-wins p1-new-score p2-score p1-new-pos p2-pos 2)))))
                            (apply +))
            (= turn 2) (->> dice-roll-frequencies
                            (map (fn [[roll-sum freq]]
                                   (let [new-space (mod (+ roll-sum p2-pos) 10)
                                         p2-new-pos (if (= new-space 0) 10 new-space)
                                         p2-new-score (+ p2-score p2-new-pos)]
                                     (* freq (num-p1-wins p1-score p2-new-score p1-pos p2-new-pos 1)))))
                            (apply +)))))

(def num-p1-wins-memoized (memoize num-p1-wins))

(defn num-p2-wins [p1-score p2-score p1-pos p2-pos turn]
  (cond
    (>= p1-score 21) 0
    (>= p2-score 21) 1
    :else (cond
            (= turn 1) (->> dice-roll-frequencies
                            (map (fn [[roll-sum freq]]
                                   (let [new-space (mod (+ roll-sum p1-pos) 10)
                                         p1-new-pos (if (= new-space 0) 10 new-space)
                                         p1-new-score (+ p1-score p1-new-pos)]
                                     (* freq (num-p1-wins p1-new-score p2-score p1-new-pos p2-pos 2)))))
                            (apply +))
            (= turn 2) (->> dice-roll-frequencies
                            (map (fn [[roll-sum freq]]
                                   (let [new-space (mod (+ roll-sum p2-pos) 10)
                                         p2-new-pos (if (= new-space 0) 10 new-space)
                                         p2-new-score (+ p2-score p2-new-pos)]
                                     (* freq (num-p1-wins p1-score p2-new-score p1-pos p2-new-pos 1)))))
                            (apply +)))))

(def num-p2-wins-memoized (memoize num-p2-wins))

(defn part2 []
  (max
   (num-p1-wins-memoized 0 0 10 4 1)
   (num-p2-wins-memoized 0 0 10 4 1)))

(defn main []
  (println
   "PART 2:"
   (part2)))

(main)
