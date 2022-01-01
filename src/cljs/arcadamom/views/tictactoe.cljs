(ns arcadamom.views.tictactoe
  (:require
   [re-frame.core :as rf]
   [arcadamom.views.home :refer [home]]
   [arcadamom.ai.tictactoe :as ttt]))

(rf/reg-event-db
 :play
 (fn [db [_ current-state]]
  (assoc db
         :tictactoe/state
         (ttt/play current-state))))

(rf/reg-event-db
 :set-tac
 [(rf/path :tictactoe/state)]
 (fn [tictactoestate [_ val tac]]
   (assoc tictactoestate val tac)))

(rf/reg-sub
 :get-game
 (fn [db _]
   (:tictactoe/state db)))

(rf/reg-event-db
 :game/reset
 (fn [db _]
   (assoc db
          :tictactoe/state {}
          :game-won?/tictactoe false)))

(rf/reg-sub
 :clicked?
 :<- [:get-game]
 (fn [game-status [_ id]]
   (get game-status id)))

(rf/reg-event-db
 :game-over
 (fn [db [_ game-status]]
   (assoc db :game-won?/tictactoe (ttt/win? game-status))))


(rf/reg-sub
 :game-over?
 (fn [db _]
   (:game-won?/tictactoe db)))


(defn tictactoe []
  (let [game-over @(rf/subscribe [:game-over?])]
    [:div
     [home]
     ;[:p "Let's play some tictactoe!"]
     [:button {:on-click #(rf/dispatch [:game/reset])} "Reset board"]
     [:p 
      (if game-over
        (cond (ttt/won @(rf/subscribe [:get-game]) "x") "You won!"
              (ttt/won @(rf/subscribe [:get-game]) "0") "You Lost!"
              :else "Game Over")
        "Game in progress")]
     [:div.tictactoecontainer
      (doall
       (for [x (vec (range 1 10))]
         [:div.tile {:on-click (fn []
                                 (if game-over
                                   (do)
                                   (do
                                     (if (contains? @(rf/subscribe [:get-game]) x)
                                       (do)
                                       (do
                                         (rf/dispatch-sync [:set-tac x "x"])
                                         (rf/dispatch-sync [:game-over
                                                            @(rf/subscribe [:get-game])])
                                         (if @(rf/subscribe [:game-over?])
                                           (do)
                                           (rf/dispatch-sync [:play
                                                              @(rf/subscribe [:get-game])]))
                                         (rf/dispatch-sync [:game-over
                                                            @(rf/subscribe [:get-game])]))))))}
          [:p {:class "tac"} @(rf/subscribe [:clicked? x])]]))]
     ]))
