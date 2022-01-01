(ns arcadamom.views.fourinarow
  (:require
   [arcadamom.views.home :refer [home]]
   [re-frame.core :as rf]
   [arcadamom.ai.fourinarow :as fia]))

(rf/reg-event-fx
 :play/fourinarow
 (fn [{:keys [db]} [_ current-state]]
   {:db (assoc db
               :fourinarow/state
               (fia/play current-state))}))

(rf/reg-event-db
 :drop-tac
 [(rf/path :fourinarow/state)]
 (fn [fourinarowstate [_ val]]
   (fia/drop-tac fourinarowstate val)))

(rf/reg-sub
 :get-game/fourinarow
 (fn [db _]
   (:fourinarow/state db)))

(rf/reg-event-db
 :fourinarow/reset
 (fn [db _]
   (assoc db
          :fourinarow/state {}
          :game-won?/fourinarow false)))

(rf/reg-sub
 :clicked?/fourinarow
 :<- [:get-game/fourinarow]
 (fn [game-status [_ id]]
   (get game-status id)))

(rf/reg-event-db
 :game-over/fourinarow
 (fn [db [_ game-status]]
   (assoc db :game-won?/fourinarow (fia/win? game-status))))

(rf/reg-sub
 :game-over?/fourinarow
 (fn [db _]
   (:game-won?/fourinarow db)))

(defn set-class [val]
  (cond (= val "r") {:class "red"} 
        (= val "y") {:class "yellow"}
        :else nil))

(defn fourinarow []
  (let [game-over @(rf/subscribe [:game-over?/fourinarow])]
   [:div
    [home]
    [:button {:on-click #(rf/dispatch [:fourinarow/reset])} "Reset board"]
    [:div.drop
     (doall
      (for [x (vec (range 1 8))]
        ^{:key x}
        [:div.tile-four-in-a-row-drop
         {:on-click (fn []
                      (if game-over
                        (do)
                        (if  (< (nth (vec (fia/legal-plays @(rf/subscribe [:get-game/fourinarow]))) (dec x)) 1)
                          (do)
                          (do
                            (rf/dispatch-sync [:drop-tac (if (= x 7) 0 x)])
                            (rf/dispatch-sync [:game-over/fourinarow
                                               @(rf/subscribe [:get-game/fourinarow])])
                            (if @(rf/subscribe [:game-over?/fourinarow])
                              (do)
                              (rf/dispatch-sync [:play/fourinarow
                                                 @(rf/subscribe [:get-game/fourinarow])]))
                            (rf/dispatch-sync [:game-over/fourinarow
                                               @(rf/subscribe [:get-game/fourinarow])])))))}]))]
    [:br]
    [:hr {:class "break"}]
    [:div.fourinarowcontainer
     (doall
      (for [x (vec (range 1 43))]
        ^{:key x}
        [:div.tile-four-in-a-row
         (set-class @(rf/subscribe [:clicked?/fourinarow x]))]))]]))