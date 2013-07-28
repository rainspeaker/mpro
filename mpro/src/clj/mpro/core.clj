(ns mpro.core
  (:require [appengine-magic.core :as ae]))


(defn mpro-app-handler [request]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body "Hello, world!"})


(ae/def-appengine-app mpro-app #'mpro-app-handler)