(ns mpro.comp
  (:require [clojure.string :as string]
            [mpro.clients :as clt]
            [clj-time.format :as tformat])
  (:use [mpro.helpers]
        [mpro.helper-macros]))

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

(defn head [scripts sheets]
  "Scripts and sheets as keywords. They will be included in order received. "
  (hiccup-for [:head
               :for$ [[script (get-scripts scripts)] [:script {:src script}]]
               :for$ [[sheet (get-stylesheets sheets)] [:link {:rel "stylesheet" :href sheet}]]]))

(defn client-menu [active-client clients]
  (hiccup-for [:ul {:class "nav nav-list nav-stacked"}
               [:li {:id "page_selector_noclient"}
                [:a {:href "#noclient" :data-toggle "tab"} "Home"]]
               [:li {:class "nav-header"} "Current Clients"]
               :for$
               [[client clients ]
                [:li {:id (str "pane_selector_client" (-> client :id))}
                 [:a {:href (str "#client" (-> client :id))
                      :data-toggle "tab"}
                  (-> client :name :full)]]]]))

(defn make-bs-radio-group
  ([name active options]
     (hiccup-for
      [:div {:class "btn-group" :data-toggle "buttons-radio"}
       :for$
       [[[kw vlu display] options]
        [:button {:class  (if (not= kw active) "btn" "btn active")
                  :name name
                  :type "button"
                  :value vlu} display]]]))
  ([name options]
     (hiccup-for
      [:div {:class "btn-group"}
       :for$
       [[[kw vlu display] options]
        [:button {:class "btn"
                  :name name
                  :type "button"
                  :value vlu} display]]])))

(defn make-bs-control-group [labeltags labeltext controltags controls & help]
  [:div {:class "control-group"}
   (if (and labeltags labeltext)
     [:label (if (contains? labeltags :class)
               (assoc labeltags :class (str "" (labeltags :class)))
               (assoc labeltags :class ""))
      labeltext])
   (hiccat
    [:div (if (contains? controltags :class)
            (assoc controltags :class (str "controls " (controltags :class)))
            (assoc controltags :class "controls"))
     :cat$ controls])
   (if (seq help)
     (let [[helptags helptext] help]
       (if (and helptags helptext)
         (hiccat
          [:span (if (contains? labeltags :class)
                   (assoc labeltags :class (str "help-block" (labeltags :class)))
                   (assoc labeltags :class "help-block"))
           :cat$ helptext]))))])

(defn make-empty-tags [tag params]
  (for [[attribs & anythingelse]
        params]
    [tag (if (and (attribs :value) (not (string/blank? (attribs :value))))
              attribs
              (dissoc attribs :value))]))

(defn make-text-inputs [params]
  (->> params
       (map #(cons (assoc (first %) :type "text") rest %))
       (make-generic-inputs)))

(defn make-empty-tag-maker [tag params]
  (fn [special-params]
    (->> special-params
         (map #(cons (into (first params) (first %))
                     (concat (rest params) (rest %))))
         (make-empty-tags tag))))

(defn make-input-maker [params]
  (make-empty-tag-maker :input params))


(defn cprofile-essentials [client]
  (hiccat
   [:div 
    [:div {:class "row-fluid"}
     (make-bs-control-group
      {}
      "Preferred name, Email, Phone"
      {:class "controls-row"}
      ((make-input-maker [{:type "text"}])
       (map
        (fn [[l & attribs]]
          (let [[n v p sz] attribs]
            [{:name n :value v :placeholder l :class sz
              }]))
        [["Preferred name" "preferred-name"
          (-> client :name :preferred) "e.g. \"Robbie\"" "span3"]
         ["Email" "email"
          (-> client :primary-email :address)
          "e.g. \"blank@blankety.com\"" "span4"]
         ["CC" "phone-country-code"
          (-> client :primary-phone :country-code)
          "eg. +1" "span1"]
         ["Main phone number" "phone-main-number"
          (-> client :primary-phone :main-number)
          "eg. 555-555-5555" "span2"]
         ["Extension" "phone-extension"
          (-> client :primary-phone :extension)
          "eg. \"ex. 555\"" "span2"]]))
      {}
      ;; needs to be set up in case one or both of these fields is missing
      (if false
        [[:a {:href (-> client :primary-email :mailto-link)}
         "Email "]
        "or "
        [:a {:href (-> client :primary-phone :tel-link)}
         "call."]]))
     (make-bs-control-group
     {}
     "Full name"
     {:class "controls-row"}
     ((make-input-maker [{:type "text"}])
      (map
       (fn [[l & attribs]]
         (let [[n v p sz] attribs]
           [{:name n :value v :placeholder l :class sz}]))
       [["Honorific" "honorific-name"
         (-> client :name :honorific) "e.g. Mr." "span2"]
        ["First" "first-name"
         (-> client :name :first) "e.g. Robert" "span3"]
        ["Middle" "middle-name"
         (-> client :name :middle) "e.g. Burr" "span2"]
        ["Last" "last-name"
         (-> client :name :last) "e.g. Eginton" "span3"]
        ["Post-fix" "post-fix-name"
         (-> client :name :post-fix) "e.g. Jr, IV, etc." "span2"]])))]]))

(defn cprofile-health [client]
  (hiccat
   [:div
    [:legend "Health and Ill-health"]
    [:div {:class "row-fluid"}
     (make-bs-control-group
     {}
     "Complaint"
     {}
     ;; this is currently only displaying the first complaint! 
     [[:textarea {:rows "2" :class "span12" :name "complaint"
                  :placeholder "Client's chief complaint"}
       (-> client :complaint first :complaint)]]
     {}
     [(str "Last modified on "
           (tformat/unparse
            (tformat/formatters :rfc822)
            (-> client :complaint first :complaint-date)))])
     (make-bs-control-group
      {}
      "Diagnosis"
      {}
      [[:textarea {:rows "4" :class "span12" :name "health-notes"
                    :placeholder "Make notes on the client's diagnosishere."}
        (-> client :health-notes :health-notes)]]
      {}
      ;;need to add time tracking to this and all other notes and
      ;;continually modified things. 
      [(str "Last modified on IMPLEMENTME")])
     (make-bs-control-group
      {}
      "Family history"
      {}
      [[:textarea {:rows "4" :class "span12" :name "family-history"
                    :placeholder "Note down the client's family history here."}
        (-> client :family-history :family-history)]]
      {}
      [(str "Last modified on IMPLEMENTME")])
     ]]))

(defn cprofile-perpetual-professional [client]
  [:div {}
   [:legend "Professional notes"]
   [:div
    (make-bs-control-group
     {}
     nil
     {}
     [[:textarea {:rows "10" :class "span12" :name "perpetual-professional-notes"
                  :placeholder "Make notes to remind yourself of peculiarities of working with this client here (eg. places they don't like to be touched, etc)."}]]
     {}
     [(str "Last modified on IMPLEMENTME")])]])

(defn cprofile-perpetual-personal [client]
  [:div {}
   [:legend "Personal notes"]
   [:div
    (make-bs-control-group
     {}
     nil
     {}
     [[:textarea {:rows "10" :class "span12" :name "perpetual-professional-notes"
                  :placeholder "Make notes to remind yourself of personal/social details of this client (eg. things they don't like to talk about, etc)."}]]
     {}
     [(str "Last modified on IMPLEMENTME")])]])

(defn cprofile-demographic [client]
  [:div {}
   [:legend "Demographics"]
   [:div 
    [:div {:class "control-group"}
     [:label {:class "control-label"} "Gender"]
     [:div {:class "controls"}
      (make-bs-radio-group "gender" (-> client :demographic :gender)
                           (for [[kw st] clt/client-gender-map]
                             [kw st (string/capitalize st)]))]]
    [:div {:class "control-group"}
     [:label {:class "control-label"} "Sex"]
     [:div {:class "controls"}
      (make-bs-radio-group "sex" (-> client :demographic :sex)
                           (for [[kw st] clt/client-sex-map]
                             [kw st (string/capitalize st)]))]]
    [:div {:class "control-group"}
     [:label {:class "control-label"} "Age Group"]
     [:div {:class "controls"}
      (make-bs-radio-group "age-group" (-> client :demographic :age-group)
                           (for [[kw st] clt/client-age-group-map]
                             [kw st (string/capitalize st)]))]]
    (make-bs-control-group
     {:class "control-label"}
     "Ethnicity"
     {}
     ((make-input-maker [{:type "text" :class "span8"}])
      [[{:value (-> client :demographic :ethnicity) :placeholder "Ethnicity"}]]))
    (make-bs-control-group
     {:class "control-label"}
     "Profession"
     {}
     ((make-input-maker [{:type "text" :class "span8"}])
      [[{:value (-> client :demographic :profession) :placeholder "Profession"}]]))]])

(defn cprofile-addresses [client]
  (hiccat
   [:div
    [:legend "Address(es)"]
    :cat$
    (for [client-address (-> client :address)]
      [:div
       (map
        (fn [[l n v p sz & others]]
          (make-bs-control-group
           nil nil
           {}
           ((make-input-maker [{:type "text"}])
            [[{:value v :name n :placeholder p :class sz}]])))
        [["Mail-to name" "mailing-name"
          (-> client-address :mailing-name)
          "Mail-to name" "span10"]
         ["Street address" "street-address"
          (-> client-address :street-address)
          "Street address" "span10"]
         ["City, State" "city-state"
          (-> client-address :city-state)
          "City, State" "span10"]
         ["ZIP code" "zip"
          (-> client-address :zip)
          "ZIP code" "span6"]])])]))

(defn cprofile [client]
  [:form {}
   [:fieldset
    [:div {:class "span10"} (cprofile-essentials client)]
    [:div {:class "row-fluid"}
     [:div {:class "span5 split-form split-form-left"}
      [:div (cprofile-perpetual-professional client)]]
     [:div {:class "span5 split-form"}
      [:div (cprofile-perpetual-personal client)]]]
    [:div {:class "row-fluid"}
     [:div {:class "span10"}
      (cprofile-health client)]]
    [:div {:class "row-fluid"}
     [:div {:class "span5 split-form split-form-left"}
      [:div (cprofile-demographic client)]]
     [:div {:class "span5 split-form"}
      [:div (cprofile-addresses client)]]]]])

(defn home [clients active-client active-tab page-title motd]
  (let [hd (head [:home :bootstrap] [:bootstrap :primary])
        bd
        [:body
         [:div {:class "container-fluid"}
          [:div {:class "page-header"}
           [:h1 (str page-title " ")
            [:small motd]]]
          [:div {:class "row-fluid"}
           [:div {:class "span2"}
            [:div {:id "clientlist"}
             (client-menu :none clients)]]
           [:div {:class "span10"}
            (hiccup-for
             [:div {:class "tab-content"}
              :for$ [[client clients
                      :let [id (-> client :id)]
                      :let [full-name-with-trappings (-> client :name :full-with-trappings)]]
                    [:div {:class "tab-pane" :id (str "client" id)}
                     [:h3 full-name-with-trappings]
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
                       (cprofile client)]
                      [:div {:class "tab-pane" :id (str "notes" id)}
                       "Dragon Land."]
                      [:div {:class "tab-pane" :id (str "worksheet" id)}
                       "Wagon Land."]]]]])]]]]]
    [:html hd bd]))
