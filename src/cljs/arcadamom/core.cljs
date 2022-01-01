(ns arcadamom.core
  (:require
   [reitit.coercion.spec :as reitit-spec]
   [reitit.frontend :as rtf]
   [reitit.frontend.easy :as rtfe]
   [reagent.dom :as dom]
   [reagent.core :as r]
   [re-frame.core :as rf]
   [arcadamom.views.home :as home]
   [arcadamom.routes.app :refer [app-routes]]))



(rf/reg-event-db
 :router/navigated
 (fn [db [_ new-match]]
   (assoc db :router/current-route new-match)))

(rf/reg-sub
 :router/current-route
 (fn [db]
   (:router/current-route db)))

(def router
  (rtf/router
   (app-routes)
   {:data {:coercion reitit-spec/coercion}}))

(defn navbar []
  (let [burger-active (r/atom false)]
    (fn []
      [:nav.navbar
       [:div.container
        [:div.navbar-brand
         [:a.navbar-item
          {:href "/"
           :style {:font-weight "bold"}}
          "Arcadamom"]
         [:span.navbar-burger.burger
          {:data-target "nav-menu"
           :on-click #(swap! burger-active not)
           :class (when @burger-active "is-active")}
          [:span]
          [:span]
          [:span]]]
        [:div#nav-menu.navbar-menu
         {:class (when @burger-active "is-active")}
         [:div.navbar-start
          ;[:a.navbar-item
           ;{:href "/"}
           ;"Home"]
          ]
         [:div.navbar-end
          [:div.navbar-item
        ;;    (case @(rf/subscribe [:auth/user-state])
        ;;      :loading
        ;;      [:div {:style {:width "5em"}}
        ;;       [:progress.progress.is-dark.is-small {:max 100} "30%"]]
        ;;      :authenticated
        ;;      [:div.buttons
        ;;       [auth/nameplate @(rf/subscribe [:auth/user])]
        ;;       [auth/logout-button]]
        ;;      :anonymous
        ;;      [:div.buttons
        ;;       [auth/login-button]
        ;;       [auth/register-button]])
           ]]]]])))

(defn page [{{:keys [view name]} :data
             path :path}]
  [:section.section>div.container
   (if view
     [view]
     [:div "No view specified for route: " name " (" path ")"])])

(defn app []
  (let [current-route @(rf/subscribe [:router/current-route])]
    [:div.app
     [navbar]
     [:section.section>div.container
      [page current-route]]]))

(defn init-routes! []
  (rtfe/start!
   router
   (fn [new-match]
     (when new-match
       (rf/dispatch [:router/navigated new-match])))
   {:use-fragment false}))
              
(defn ^:dev/after-load mount-components []
  (rf/clear-subscription-cache!)
  (.log js/console "Mounting Components...")
  (init-routes!)
  (dom/render [#'app] (.getElementById js/document "content"))
  (.log js/console "Components Mounted!"))

(defn init! []
  (.log js/console "Initializing App...")
  (mount-components))