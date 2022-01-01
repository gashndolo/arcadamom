(ns arcadamom.ai.fourinarow)

 ;; ;; ;; ;; ;; ;; ;;; ;;; ;; ;; ;; ; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;

 ;; ;; ;; ;; ;; ;; ;;; ;;; The play AI needs some looking at
 ;; ;; ;; ;; ;; ;; ;;; ;;; Especially determining whether to block a two in a row
 ;; ;; ;; ;; ;; ;; ;;; ;;; Or setting up it's own two in a row

 ;; ;; ;; ;; ;; ;; ;;; ;;; ;; ;; ;; ; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;


(defn subsets
  [s]
  (if (empty? s)
    #{#{}}
    (let [ts (subsets (rest s))]
      (->> ts
           (map #(conj (set %) (first s)))
           (clojure.set/union ts)))))


;(def game-state
;  {32 "r" 33 "y" 2 "r" 34 "r" 35 "r" 36 "r" 37 "r" 38 "r" 39 "y"})

 ;; ;; ;; ;; ;; ;; ;;; ;;; ;; ;; ;; ; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;
;; named poorly more of a helper func
;(defn already-played [game-state n]
;  (filter #(= n (mod % 7)) (map (fn [[k v]] k) game-state)))

(defn current-game-points [game-state x]
  (map (fn [[k v]] k)
       (select-keys game-state (for [[k v] game-state :when (= v x)] k))))



;(defn place [game-state point]
;  (last
;   (sort
;    (vec
;     (clojure.set/difference
;      (set (possible-plays point))
;      (set (already-played game-state point)))))))
;;
;;
;;
;; ;; ;; ;; ;; ;; ;; ;; ; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;; 

(defn upper-limit [n]
  (cond (and (>= n 1) (<= n 7)) 7
        (and (>= n 8) (<= n 14)) 14
        (and (>= n 15) (<= n 21)) 21
        (and (>= n 22) (<= n 28)) 28
        (and (>= n 29) (<= n 35)) 35
        (and (>= n 36) (<= n 42)) 42
        :else "Error"))


(defn lower-limit [n]
  (cond (and (>= n 1) (<= n 7)) 1
        (and (>= n 8) (<= n 14)) 8
        (and (>= n 15) (<= n 21)) 15
        (and (>= n 22) (<= n 28)) 22
        (and (>= n 29) (<= n 35)) 29
        (and (>= n 36) (<= n 42)) 36
        :else "Error"))

;; Used less than instead of greater than and heavily confused myself
;; if it's this then don't do it 
;is not the best way to reason about stuff
(defn flats
  ([] (flats 1))
  ([n] (lazy-seq (cons n (flats (inc n))))))

(defn get-flats [n cnt]
  (let [x (cond (= cnt 3) 2 (= cnt 2) 1 :else 3)]
    (if (< (- (upper-limit n) n) x)
      nil
      (take cnt (flats n)))))

(defn left-diagonals
  ([] (left-diagonals 1))
  ([n] (lazy-seq (cons n (left-diagonals (+ n 6))))))


(defn get-left-diagonals [n cnt]
  (let [x (cond (= cnt 3) 2 (= cnt 2) 1 :else 3)]
    (if  (> n 35)
      nil
      (if (< (- n (lower-limit n)) x)
        nil
        (take cnt (left-diagonals n))))))


(defn right-diagonals
  ([] (right-diagonals 1))
  ([n] (lazy-seq (cons n (right-diagonals (+ n 8))))))


(defn get-right-diagonals [n cnt]
  (let [x (cond (= cnt 3) 2 (= cnt 2) 1 :else 3)]
    (if (> n 35)
      nil
      (if (< (- (upper-limit n) n) x)
        nil
        (take cnt (right-diagonals n))))))

(defn bottoms
  ([] (bottoms 1))
  ([n] (lazy-seq (cons n (bottoms (+ n 7))))))

(defn get-bottoms [n cnt]
  (let [x (if (= cnt 2) < <=)]
    (if (x (- (+ (mod n 7) 35) n) 7)
      nil
      (take cnt (bottoms n)))))


(defn possible-three-in-a-rows [n cnt]
  (let [x []]
    (conj x
          (get-flats n cnt)
          (get-left-diagonals n cnt)
          (get-right-diagonals n cnt)
          (get-bottoms n cnt))))

;; func is called get reds but will also obviously get yellows
;; get reds that are three in a row (i.e. with possible of fours
(defn get-reds [game-state color cnt]
  (let [reds (map (fn [[k v]] k)
                  (select-keys game-state (for [[k v] game-state :when (= v color)] k)))]
    (let [intermediate
          (for [x reds]
            (possible-three-in-a-rows x cnt))]
      (for [subcoll intermediate, item subcoll]
        (set item)))))

;; check if there are any three in a rows in the game-state currently
;; func called current three in a rows but using the cnt variable we can find two in a rows to build a three in a row
(defn current-three-in-a-rows [game-state color cnt]
  (map (fn [x] (vec x))
       (clojure.set/intersection
        (set (get-reds game-state color cnt))
        (set
         (filter #(= (count %) cnt)
                 (subsets (current-game-points game-state color)))))))

(def left-edges [1 8 15 22 29 36])
(def right-edges [7 14 21 28 35 42])

(defn helper-possible-wins [some-row]
  (let [srtd (sort some-row)
        diff (- (second srtd) (first srtd))
        sols (atom [])]
    (cond (= diff 1) (do
                       (if (= (rem (first srtd) 7) 1)
                         (do)
                         (swap! sols conj (dec (first srtd))))
                       (if (= (rem (last srtd) 7) 0)
                         (do)
                         (swap! sols conj (inc (last srtd)))))
          (= diff 7) (do
                       (if (< (first srtd) 8)
                         (do)
                         (swap! sols conj (- (first srtd) 7)))
                       (if (> (last srtd) 35)
                         (do)
                         (swap! sols conj (+ 7 (last srtd)))))
          (= diff 8) (do
                       (if (or (< (first srtd) 7) (some #(= (first srtd) %) left-edges))
                         (do)
                         (swap! sols conj (- (first srtd) 8)))
                       (if (or (> (last srtd) 35) (some #(= (last srtd) %) right-edges))
                         (do)
                         (swap! sols conj (+ (last srtd) 8))))
          (= diff 6) (do
                       (if (or (= (rem (last srtd) 7) 1) (some #(= (last srtd) %) left-edges))
                         (do)
                         (swap! sols conj (+ (last srtd) 6)))
                       (if (or (< (first srtd) 7) (some #(= (first srtd) %) right-edges))
                         (do)
                         (swap! sols conj (- (first srtd) 6)))))
    @sols))

;; Named possible wins but can also be used to get possible 2 and three in a rows
(defn possible-wins [game-state color cnt]
  (let [x (current-three-in-a-rows game-state color cnt)]
    (if (empty? x)
      []
      (for [j x]
        (helper-possible-wins j)))))

(defn legal-plays [game-state]
  (let [already-played (map (fn [[k v]] k) game-state)]
    (for [x [1 2 3 4 5 6 0]]
      (let [j (filter #(= x (mod % 7)) already-played)]
        (if (empty? j)
          (if (= x 0)
            (+ 35 7)
            (+ 35 (mod x 7)))
          (- (apply min j) 7))))))

;; be sure to remove the repetition when you get the chance!
(defn play [game-state]
  (let [win
        (clojure.set/intersection
         (set (legal-plays game-state))
         (set (flatten (possible-wins game-state "y" 3))))
        block
        (clojure.set/intersection
         (set (legal-plays game-state))
         (set (flatten (possible-wins game-state "r" 3))))
        set-up
        (clojure.set/intersection
         (set (legal-plays game-state))
         (set (flatten (possible-wins game-state "y" 2))))
        block-set-up
        (clojure.set/intersection
         (set (legal-plays game-state))
         (set (flatten (possible-wins game-state "r" 2))))]
    (cond (not (empty? win)) (assoc game-state (first win) "y")
          (not (empty? block)) (assoc game-state (first block) "y")
          (not (empty? block-set-up)) (assoc game-state (first block-set-up) "y")
          (not (empty? set-up)) (assoc game-state (first set-up) "y")
          :else (let [x (filter #(> % 0) (legal-plays game-state))]
                  (if (empty? x)
                    game-state
                    (assoc game-state (rand-nth x) "y"))))))


(defn possible-plays [n]
  (filter #(= n (mod % 7)) (range 1 43)))

;; remember that for 7 we'll use 0
;; because of the modulo
(defn drop-tac [game-state n]
  (let [x (clojure.set/intersection
           (set (legal-plays game-state))
           (set (possible-plays n)))]
    (if (empty? x)
      (do)
      (assoc game-state (first x) "r"))))



(defn won [game-state color]
  (if (empty? (current-three-in-a-rows game-state color 4))
    false
    true))

(defn win? [m]
  (or (won m "r")
      (won m "y")
      false))
