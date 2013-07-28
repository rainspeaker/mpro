(ns mpro.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.middleware.file :as file]
            [ring.middleware.params :as params]
            [ring.middleware.file-info :as file-info]
            [ring.util.response :as response]
            [appengine-magic.core :as ae]
            [korma.core :as korma]
            [mpro.dbclient :as dbclient]
            [mpro.clients :as clt]
            [clojure.string :as string]
            [mpro.comp :as comp]
            )
  (:use [hiccup.core] [hiccup.def]
        [mpro.helper-macros]
        [net.cgrand.moustache])
  (:gen-class))


(defn integer-of-string [s]
   "returns nil if s does not represent an integer" 
    (try 
      (Integer/parseInt s)
      (catch Exception e)))

;; (defn client-menu [active-client]
;;   (cond
;;    (= active-client :none) (client-menu)))


;;consider making default values for these args, not sure of the
;;syntax right now


(defn render-home [client-status client-ispaid active-client active-tab title motd]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (html (comp/home
                (clt/construct-clients-by-group client-status client-ispaid)
                active-client
                active-tab
                title
                motd))})

(defn render-app-w-client-id [id]
  (render-home :any :any id :none "Initial Testing" "I has bo0ostrappp"))

(defn render-app-straight [req]
  (render-home :any :any :none :none
               "Initial Testing" "I haz bo0ostrapppp"))

(def mpro
  (app (resources/wrap-resource "/public")
       (file-info/wrap-file-info)
       [""] render-app-straight
       ["client"] render-app-straight
       ["client" [id integer-of-string]]
       {:get
        (fn [req] (do ;;(println req)
                     (render-app-w-client-id id)))
        :post (app
               (params/wrap-params)
               #(do (let [partial-client
                          (clt/post-to-partial-client
                           %
                           (clt/construct-client-by-id id))]
                      (do
                        (println
                         (str "\n\n\n********************************************************************\n"
                              (% :form-params) "\n\n"
                              partial-client))
                        (clt/update-partial-client partial-client)))
                    (render-app-w-client-id id)))}))

(ae/def-appengine-app mpro-app #'mpro)
