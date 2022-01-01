(ns arcadamom.routes.app
  (:require
   #?@(:clj [[arcadamom.layout :as layout]
             [arcadamom.middleware :as middleware]]
       :cljs [[arcadamom.views.home :as home]
              [arcadamom.views.tictactoe :as tictactoe]
              [arcadamom.views.fourinarow :as fourinarow]
              [arcadamom.views.checkers :as checkers]])))

#?(:clj
   (defn home-page [request]
     (layout/render
      request
      "home.html")))

(defn app-routes []
  [""
   #?(:clj {:middleware [middleware/wrap-csrf]
            :get home-page})
   ["/"
    (merge
     {:name ::home}
     #?(:cljs
        {:view #'home/home}))]
   ["/tictactoe"
    (merge
     {:name ::tictactoe}
     #?(:cljs {:view #'tictactoe/tictactoe}))]
   ["/fourinarow"
    (merge
     {:name ::fourinarow}
     #?(:cljs {:view #'fourinarow/fourinarow}))]
   ["/checkers"
    (merge
     {:name ::checkers}
     #?(:cljs {:view #'checkers/checkers}))]])