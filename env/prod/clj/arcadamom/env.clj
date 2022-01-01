(ns arcadamom.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[arcadamom started successfully]=-"))
   :stop
   (fn []
     (log/info "\n-=[arcadamom has shut down successfully]=-"))
   :middleware identity})
