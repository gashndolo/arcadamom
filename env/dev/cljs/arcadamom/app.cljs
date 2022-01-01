(ns ^:dev/once arcadamom.app
  (:require
   [devtools.core :as devtools]
   [arcadamom.core :as core]))

(enable-console-print!)

(println "loading env/dev/cljs/arcadamom/app.cljs...")

(devtools/install!)

(core/init!)