(ns arcadamom.views.home
  (:require
   [re-frame.core :as rf]
   [arcadamom.ai.checkers :refer [starting]]))



(rf/reg-event-db
 :set-game
 (fn [db [_ game]]
   (assoc db :game game)))

(rf/reg-event-db
 :init-checkers
 (fn [db _]
   (assoc db 
          :checkers/state starting
          :checkers/current-play [])))

(defn home []
  [:div.linkcontainer 
   [:a {:class "game-link"
        :href "/tictactoe"
        :on-click #(rf/dispatch [:set-game "tictactoe"])} "Tic Tac Toe"]
   [:a {:class "game-link"
        :href "/fourinarow"
        :on-click #(rf/dispatch [:set-game "fourinarow"])} "Four in a row"]
   [:a {:class "game-link"
        :href "/checkers"
        :on-click #(do
                     (rf/dispatch [:set-game "checkers"])
                     (rf/dispatch [:init-checkers]))} "Checkers"]])