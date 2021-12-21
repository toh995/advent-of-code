(ns day21.part01)

(defn play-game [p1-start-pos p2-start-pos]
  (loop [p1-pos p1-start-pos
         p2-pos p2-start-pos
         p1-score 0
         p2-score 0
         move 0]
    (let [p1-new-pos (let [new-space (mod (apply + p1-pos (range (+ (* move 6) 1) (+ (* move 6) 4))) 10)]
                       (if (= new-space 0) 10 new-space))
          p2-new-pos (let [new-space (mod (apply + p2-pos (range (+ (* move 6) 4) (+ (* move 6) 7))) 10)]
                       (if (= new-space 0) 10 new-space))
          p1-new-score (+ p1-score p1-new-pos)
          p2-new-score (+ p2-score p2-new-pos)]
      (cond
        (>= p1-new-score 1000) {:p1-score p1-new-score
                                :p2-score p2-score
                                :dice-rolls (+ 3 (* move 6))}
        (>= p2-new-score 1000) {:p1-score p1-new-score
                                :p2-score p2-new-score
                                :dice-rolls (* (inc move) 6)}
        :else (recur
               p1-new-pos
               p2-new-pos
               p1-new-score
               p2-new-score
               (inc move))))))

(defn part1 []
  (let [{p1-score :p1-score
         p2-score :p2-score
         dice-rolls :dice-rolls} (play-game 10 4)]
    (* dice-rolls (min p1-score p2-score))))

(defn main []
  (println
   "PART 1:"
   (part1)))

(main)
