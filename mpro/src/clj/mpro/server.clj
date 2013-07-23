(ns mpro.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response]
            [korma.core :as korma]
            [mpro.dbclient :as dbclient]
            [mpro.clients :as clt]
            [clojure.string :as string]
            [mpro.comp :as comp]
            ;; moustache here!
            )
  (:use [hiccup.core] [hiccup.def]
        [mpro.helper-macros])
  (:gen-class))




;; (defn client-menu [active-client]
;;   (cond
;;    (= active-client :none) (client-menu)))


;;consider making default values for these args, not sure of the
;;syntax right now


(defn render-home []
  {:status 400
   :headers {"Content-Type" "text/html"}
   :body (html (comp/home
                (clt/construct-clients-by-group :current :any)
                :none
                :none
                "Initial Testing"
                "I haz bo0ostrap!"))})

(defn render-app []
  (render-home))

(defn handler [request]
  (if (= "/" (:uri request))
      (response/redirect "/help.html")
      (render-app)))

(def app
  (-> handler
      (resources/wrap-resource "public")))

(def mpr
  (mch/app ["client"]))

(defn -main [& args]
  (jetty/run-jetty app {:port 3001}))

;; (defonce server (jetty/run-jetty app {:port 3000 :join? false}))



