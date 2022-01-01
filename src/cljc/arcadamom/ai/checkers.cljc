(ns arcadamom.ai.checkers)


(defn row [check n color]
  (reduce merge (for [x (range n (+ n 8))]
                  (if (check x) {x color} {x "e"}))))

(def starting
  (merge
   (row even? 1 "w")
   (row odd? 9 "w")
   (row even? 17 "w")
   (row even? 25 "e")
   (row even? 33 "e")
   (row odd? 41 "b")
   (row even? 49 "b")
   (row odd? 57 "b")))

(def limits [1 9 17 25 33 41 49 57 8 16 24 32 40 48 56 64])

(defn upper-limit [n]
  (cond (and (>= n 1) (<= n 8)) 8
        (and (>= n 9) (<= n 16)) 16
        (and (>= n 17) (<= n 24)) 24
        (and (>= n 25) (<= n 32)) 32
        (and (>= n 33) (<= n 40)) 40
        (and (>= n 41) (<= n 48)) 48
        (and (>= n 49) (<= n 56)) 56
        (and (>= n 57) (<= n 64)) 64
        :else "Error"))


(defn lower-limit [n]
  (cond (and (>= n 1) (<= n 8)) 1
        (and (>= n 9) (<= n 16)) 9
        (and (>= n 17) (<= n 24)) 17
        (and (>= n 25) (<= n 32)) 25
        (and (>= n 33) (<= n 40)) 33
        (and (>= n 41) (<= n 48)) 41
        (and (>= n 49) (<= n 56)) 49
        (and (>= n 57) (<= n 64)) 57
        :else "Error"))


(defn specific-squares [game-state color]
  (map (fn [[k v]] k)
       (select-keys game-state (for [[k v] game-state :when (= v color)] k))))

(defn opposite [color]
  (if (= color "b") "w" "b"))


(defn moves-helper [n color]
  (cond
    (= color "b")
    (cond (= 1 (mod n 8)) [(- n 7)]
          (= 0 (mod n 8)) [(- n 9)]
          :else [(- n 7) (- n 9)])
    (= color "w")
    (cond (= 1 (mod n 8)) [(+ n 9)]
          (= 0 (mod n 8)) [(+ n 7)]
          :else [(+ n 7) (+ n 9)])))

(defn moves [n color game-state]
  (if (= color "e")
    []
    (let [move (moves-helper n color)]
      (remove nil?
              (for [x move]
                (cond (and
                       (some #(= % x) (specific-squares game-state (opposite color)))
                       (some #(= % (- x (- n x))) (specific-squares game-state "e"))
                       (not (some #(= x %) limits)))
                      (- x (- n x))
                      (some #(= % x) (specific-squares game-state "e"))
                      x
                      :else
                      (do)))))))

(defn left-diagonals-up
  ([] (left-diagonals-up 1))
  ([n] (lazy-seq (cons n (left-diagonals-up (- n 9))))))

(defn get-left-diagonals-up [n]
  (if (or (< (- n (lower-limit n)) 2) (< (upper-limit n) 24))
    nil
    (take 3 (left-diagonals-up n))))

(defn left-diagonals-down
  ([] (left-diagonals-down 1))
  ([n] (lazy-seq (cons n (left-diagonals-down (+ n 7))))))

(defn get-left-diagonals-down [n]
  (if (or (< (- n (lower-limit n)) 2) (> (upper-limit n) 48))
    nil
    (take 3 (left-diagonals-down n))))

(defn right-diagonals-up
  ([] (right-diagonals-up 1))
  ([n] (lazy-seq (cons n (right-diagonals-up (- n 7))))))

(defn get-right-diagonals-up [n]
  (if (or (< (- (upper-limit n) n) 2) (< (upper-limit n) 24))
    nil
    (take 3 (right-diagonals-up n))))

(defn right-diagonals-down
  ([] (right-diagonals-down 1))
  ([n] (lazy-seq (cons n (right-diagonals-down (+ n 9))))))

(defn get-right-diagonals-down [n]
  (if (or (< (- (upper-limit n) n) 2) (> (upper-limit n) 48))
    nil
    (take 3 (right-diagonals-down n))))


(defn available-jumps [game-state]
  (let [empties
        (specific-squares game-state "e")
        diagonals
        (for [x empties]
          [(get-right-diagonals-up x)
           (get-right-diagonals-down x)
           (get-left-diagonals-up x)
           (get-right-diagonals-down x)])
        flattened-diagonals
        (for [subcoll diagonals, item subcoll]
          item)
        jumps
        (for [x flattened-diagonals]
          (select-keys game-state (vec x)))
        legal-jumps
        (remove nil?
                (for [x jumps]
                  (if (= 3 (count (set (vals x))))
                    x
                    nil)))]
    legal-jumps))


(defn swap-play [some-map]
  (let [[a b c] (vec (sort some-map))]
    {(first a) (second c)
     (first b) "e"
     (first c) (second a)}))

(defn play [n destination game-state]
  (if (or (= 7 (- n destination)) (= 9 (- n destination)))
    (if (and
         (some #(= % destination) (specific-squares game-state "e"))
         (some #(= % destination) (moves n "b" game-state)))
      (assoc game-state n "e" destination "b")
      game-state)
    (merge
     game-state
     (swap-play (hash-map destination "e"
                          (- n (/ (- n destination) 2)) "w"
                          n "b")))))



(defn cpu-play [game-state]
  (let [x
        (filter #(and
                  (= "w" (last (vals %)))
                  (> (first (keys %)) (last (keys %)))) (available-jumps game-state))
        plays
        (specific-squares game-state "w")
        plays-with-moves
        (for [play plays]
          (if (empty? (moves play "w" game-state))
            (do)
            play))]
    (if (empty? x)
      (let [check (rand-nth (remove nil? plays-with-moves))]
        (assoc game-state
               check "e"
               (rand-nth (moves check "w" game-state))
               "w"))
      (merge game-state
             (swap-play (first x))))))

(defn won [color game-state]
  (empty? (specific-squares game-state (if (= color "w") "b" "w"))))

(defn all-possible-moves [color game-state]
  (let [tacs (specific-squares game-state color)]
    (flatten
     (for [x tacs]
       (moves x color game-state)))))

(defn win? [game-state]
  (or (won "w" game-state)
      (won "b" game-state)
      (empty? (all-possible-moves "w" game-state))
      (empty? (all-possible-moves "b" game-state))))
