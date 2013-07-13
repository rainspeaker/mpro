(ns mpro.server
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.resource :as resources]
            [ring.util.response :as response]
            [korma.core :as korma]
            [mpro.dbclient :as dbclient])
  (:use [hiccup.core] [hiccup.def]
        [mpro.helper-macros])
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
  (hiccup-for [:head
               :for$ [[script (get-scripts scripts)] [:script {:src script}]]
               :for$ [[sheet (get-stylesheets sheets)] [:link {:rel "stylesheet" :href sheet}]]]))

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
  (map-indexed #(identity {:id %1 :full-name %2}) current-client-list-dev))



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

(defn comp-client-menu []
  (hiccup-for [:ul {:class "nav nav-list nav-stacked"}
               [:li {:id "page_selector_noclient"}
                [:a {:href "#noclient" :data-toggle "tab"} "Home"]]
               [:li {:class "nav-header"} "Current Clients"]
               :for$
               [[current-client (current-clients)]
                [:li {:id (str "pane_selector_client" (current-client :id))}
                 [:a {:href (str "#client" (current-client :id))
                      :data-toggle "tab"}
                  (current-client :full-name)]]]]))

(defn client-menu [active-client]
  (cond
   (= active-client :none) (comp-client-menu)))


;;consider making default values for these args, not sure of the
;;syntax right now
(defn comp-home [active-client active-tab page-title motd]
  (let [n (korma/select dbclient/client (korma/with dbclient/technology))
        head (comp-head [:home :bootstrap] [:primary :bootstrap])
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
            (hiccup-for
             [:div {:class "tab-content"}
              :for$ [[client (current-clients)
                      :let [id (client :id)]
                      :let [full-name (client :full-name)]]
                    [:div {:class "tab-pane" :id (str "client" id)}
                     [:h3 full-name]
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
                       "Wagon Land."]]]]])]]]]]
    [:html head body]))

(defn render-home []
  {:status 400
   :headers {"Content-Type" "text/html"}
   :body (html (comp-home :none :none "Initial Testing" "I haz bo0ostrap!"))})

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

