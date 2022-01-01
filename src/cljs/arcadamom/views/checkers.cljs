(ns arcadamom.views.checkers
  (:require
   [re-frame.core :as rf]
   [arcadamom.views.home :refer [home]]
   [arcadamom.ai.checkers :as checkers]))

(rf/reg-sub
 :get-game/checkers
 (fn [db _]
   (:checkers/state db)))


(rf/reg-event-db
 :clear-play/checkers
 [(rf/path :checkers/current-play)]
 (fn [_ _]
   []))

(rf/reg-event-db
 :checkers/play
 (fn [db [_ x]]
   (update db :checkers/current-play conj x)))

(rf/reg-sub
 :checkers/get-play
 (fn [db _]
   (get db :checkers/current-play)))

(rf/reg-event-db
 :play
 (fn [db _]
   (assoc db
          :checkers/state (checkers/cpu-play (get db :checkers/state))
          :user-turn true)))

(rf/reg-sub
 :user-turn?
 (fn [db _]
   (:user-turn db)))

(rf/reg-event-db
 :checkers/move
 (fn [db [_ current-play game]]
   (if (and (> (count current-play) 1) (some #(= (second current-play) %) (checkers/moves (first current-play) "b" game)))
     (if (some #(= (first current-play) %) (checkers/specific-squares game "b"))
      (-> db
          (assoc :checkers/state (checkers/cpu-play (checkers/play (first current-play) (second current-play) game))
                 :checkers/current-play []
                 :user-turn false))
       (assoc db :checkers/current-play []))
     (if (and (> (count current-play) 1) (not (some #(= (second current-play) %) (checkers/moves (first current-play) "b" game))))
       (assoc db :checkers/current-play [])
       (do)))))



(rf/reg-sub
 :checkers/clicked
 (fn [db _]
   (get db :checkers/current-play)))

(defn draught [x game-state]
  (cond (= (x game-state) "b") [:div {:class "black-draught"}]
        (= (x game-state) "w") [:div {:class "white-draught"}]
        :else [:div {:class "empty-draught"}]))

(def whites [1 3 5 7 10 12 14 16 17 19 21 23 26 28 30 32 33 35 37 39 42 44 46 48 49 51 53 55 58 60 62 64])
(defn checkers []
  (let [game @(rf/subscribe [:get-game/checkers])
        moves (if (or (= "e" (get game (first @(rf/subscribe [:checkers/clicked]))))
                      (= "w" (get game (first @(rf/subscribe [:checkers/clicked])))))
                (rf/dispatch [:clear-play/checkers])
                (checkers/moves (first @(rf/subscribe [:checkers/clicked])) "b" game))
        current-play @(rf/subscribe [:checkers/get-play])
        user-turn @(rf/subscribe [:user-turn?])]
    
      [:div
       [home]
       ;[:p "hello Checkers"]
       ;[:p (str current-play)]
       [:button {:on-click (fn []
                             (rf/dispatch [:clear-play/checkers]))} "Cancel Play"]
       [:div.checkerscontainer
        (doall
         (for [x (vec (range 1 65))]
           ^{:key x}
           [:div.tile-checkers
            (merge (if (some #(= x %) whites) {:class "white"} {:class "green"})
                   (if (some #(= x %) moves) {:class "possible-play"})
                   {:on-click (fn []
                                (rf/dispatch-sync [:checkers/play x])
                                (rf/dispatch-sync [:checkers/move @(rf/subscribe [:checkers/get-play]) game]))})
            [:p {:class (str (get game x))} ]]))]]))

