(ns arcadamom.ai.tictactoe)

(def helper
  [[1 2 3]
   [4 5 6]
   [7 8 9]
   [1 4 7]
   [2 5 8]
   [3 6 9]
   [1 5 9]
   [3 5 7]])

(defn subsets
  [s]
  (if (empty? s)
    #{[]}
    (let [ts (subsets (rest s))]
      (->> ts
           (map #(conj (vec %) (first s)))
           (clojure.set/union ts)))))

;get-exes is really get-exes-with-possibility-of-three
;we will also use it to determine if "0" has won
(defn get-exes [m x cnt]
  (map #(vec (sort %))
       (filter (fn [x] (= (count x) cnt))
               (vec
                (subsets
                 (set
                  (map (fn [[k v]] k)
                       (select-keys m (for [[k v] m :when (= v x)] k)))))))))

;; returns the places where we can put an "0"
(defn get-point [m tac]
  (for [x (vec (get-exes m tac 2)) y helper]
    (if (= 1
           (count (clojure.set/difference (set y) (set x))))
      (first (vec (clojure.set/difference (set y) (set x)))))))

;; simply returns the empty boxes where a tac can be placed
(defn options [m]
  (vec (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                               (map (fn [[k v]] k) m))))

;; places a tac to stop the player winning if in a position to do so
;; if not places it randomly
(defn block [game-status]
  (if (empty? (get-exes game-status "x" 2))
    (assoc game-status (rand-nth (options game-status)) "0")
    (if (empty?
         (clojure.set/intersection
          (set (filter #(not (= nil %)) (get-point game-status "x")))
          (set (options game-status))))
      (assoc game-status (rand-nth (options game-status)) "0")
      (assoc game-status
             (first (clojure.set/intersection
                     (set (filter #(not (= nil %)) (get-point game-status "x")))
                     (set (options game-status)))) "0"))))

(defn won [game-status-map tac-type]
  (if (empty?
       (clojure.set/intersection
        (set (get-exes game-status-map tac-type 3))
        (set helper)))
    false
    true))

(defn win? [m]
  (or (won m "x")
      (won m "0")
      false))
 
(defn play [game-status]
  (if (empty?
       (clojure.set/intersection
        (set (filter #(not (= nil %)) (get-point game-status "0")))
        (set (options game-status))))
    (block game-status)
    (assoc game-status
           (first (clojure.set/intersection
                   (set (filter #(not (= nil %)) (get-point game-status "0")))
                   (set (options game-status)))) "0")))

