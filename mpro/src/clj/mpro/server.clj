(ns mpro.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response])
  (:use [hiccup.core] [hiccup.def])
  (:gen-class))

(def stylesheets-listing
  {:primary "css/primary.css"
   :bootstrap "css/bootstrap.css"})

(def scripts-listing
  {:home "js/cljs.js"
   :bootstrap "js/bootstrap.js"
   :jq "//ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"})

(def internal-links
  {})

(defn get-stylesheets [sheets]
  (map #(stylesheets-listing %) sheets))

(defn get-scripts [scripts]
  (cons (scripts-listing :jq) (map #(scripts-listing %) scripts)))

(defn comp-head [scripts sheets]
  "Scripts and sheets as keywords. They will be included in order received. "
  (apply vector
         (concat
          [:head]
          (for [script (get-scripts scripts)] [:script {:src script}])
          (for [sheet (get-stylesheets sheets)]
            [:link {:rel "stylesheet" :href sheet}]))))

(def current-client-list-dev
  ["Julie Tchiled"
   "MC Nail"
   "Marilee Mariloo"
   "Edgar Rainden"
   "John J. J. Schmidt"
   "Felt Tipps"
   "Ruth Dickson"
   "Artgar Funkel"
   "Paul Simone"
   "Simone de Boovwah"
   "Jean-Paul Monmartre"
   "John Luck Pickard"
   "Swedishe Scheff"
   "Wishbone the Dog"
   "Wishbone the City Commissioner"
   "Major Rathbone"
   "Mr. Mister"
   "Jonathon Boulton"
   "Morton Williams"])

(defn current-clients []
  (map #(identity {:id (first %) :full-name (second %)}) (map-indexed vector current-client-list-dev)))



(defn comp-client-menu []
  (apply vector (concat
                 [:ul {:class "nav nav-list nav-stacked"}
                  [:li {:id "page_selector_noclient"}
                   [:a {:href "#noclient" :data-toggle "tab"} "Home"]]
                  [:li {:class "nav-header"} "Current Clients"]]
                 (for [current-client (current-clients)]
                   [:li {:id (str "pane_selector_client" (current-client :id))}
                    [:a {:href (str "#client" (current-client :id))
                         :data-toggle "tab"}
                     (current-client :full-name)]]))))

(defn client-menu [active-client]
  (cond
   (= active-client :none) ()))



)))

;;consider making default values for these args, not sure of the
;;syntax right now
(defn comp-home [active-client active-tab page-title motd]
  (let [head (comp-head [:bootstrap :home] [:bootstrap :primary])
        body
        [:body
         [:div {:class "container-fluid"}
          [:div {:class "page-header"}
           [:h1 (str page-title " ")
            [:small motd]]]
          [:div {:class "row"}
           [:div {:class "span4"}
            [:div {:id "clientlist"}
             (client-menu :none)]]
           [:div {:class "span8"}
            (apply
             vector
             (concat
              [:div {:class "tab-content"}]
              (for [[id client] (current-clients)]
                [:div {:class "tab-pane" :id (str "client" id)}
                  [:h3 client]
                 [:ul {:class "nav nav-tabs"}
                  [:li {:class "active"}
                   [:a {:href (str "#cprofile" id) :data-toggle "tab"}
                    "Client Profile"]]
                  [:li [:a {:href (str "#notes" id) :data-toggle "tab"}
                        "Notes"]]
                  [:li [:a {:href (str "#worksheet" id) :data-toggle "tab"}
                        "Worksheet"]]]
                 [:div {:class "tab-content"}
                  [:div {:class "tab-pane active" :id (str "cprofile" id)}
                   "Flagon Land. "]
                  [:div {:class "tab-pane" :id (str "notes" id)}
                   "Dragon Land."]
                  [:div {:class "tab-pane" :id (str "worksheet" id)}
                   "Wagon Land."]]])))]]]]]
  [:html head body]))

(defn render-home []
  {:status 400
   :headers {"Content-Type" "text/html"}
   :body (html (comp-home :none :none "Initial Testing" "I haz bo0ostrap!")})

(defn render-app []
  (render-home))

(defn handler [request]
  (if (= "/" (:uri request))
      (response/redirect "/help.html")
      (render-app)))

(def app 
  (-> handler
    (resources/wrap-resource "public")))

(defn -main [& args]
  (jetty/run-jetty app {:port 3000}))

