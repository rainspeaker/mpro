(ns mpro.comp
  (:require [clojure.string :as string]
            [mpro.clients :as clt]
            [clj-time.format :as tformat]
            [hiccup.form :as hf])
  (:use [mpro.helpers]
        [mpro.helper-macros]
        [hiccup.def]))

(defn fdate [d]
  (tformat/unparse
   (tformat/formatters :rfc822)
   d))

(def stylesheets-listing
  {:primary "/css/primary.css"
   :bootstrap "/css/bootstrap.css"})

(def scripts-listing
  {:home "/js/cljs.js"
   :bootstrap "/js/bootstrap.js"
   :jq "//ajax.googleapis.com/ajax/libs/jquery/1.10.1/jquery.min.js"})

(def internal-links
  {})

(def sex-options
  [[:female "female"]
   [:male "male"]
   [:intersex "intersex"]])

(def gender-options
  [[:female "female"]
   [:male "male"]
   [:other "other"]])

(def age-group-options
  [[:adult "adult"]
   [:senior "senior"]
   [:teenager "teenager"]
   [:child "child"]])

(defn get-stylesheets [sheets]
  (map #(stylesheets-listing %) sheets))

(defn get-scripts [scripts]
  (cons (scripts-listing :jq) (map #(scripts-listing %) scripts)))

(defn make-cprofile-url [id]
  (str "/client/" id))

(defn head [scripts sheets]
  "Scripts and sheets as keywords. They will be included in order received. "
  (hiccup-for [:head
               :for$ [[script (get-scripts scripts)] [:script {:src script}]]
               :for$ [[sheet (get-stylesheets sheets)] [:link {:rel "stylesheet" :href sheet}]]]))

(defn client-menu [active-client clients]
  (hiccup-for [:ul {:class "nav nav-list nav-stacked"}
               [:li (if (= active-client :none)
                      {:id "page_selector_noclient"
                       :class "active"}
                      {:id "page_selector_noclient"})
                [:a {:href "#noclient" :data-toggle "tab"} "Home"]]
               [:li {:class "nav-header"} "Current Clients"]
               :for$
               [[client clients ]
                [:li (if (= active-client (-> client :id))
                       {:id (str "pane_selector_client" (-> client :id))
                        :class "active"}
                       {:id (str "pane_selector_client" (-> client :id))})
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

(defelem radio-button
  "Creates a radio button."
  ([group checked? value id]
    [:input {:type "radio"
             :name group
             :id   id
             :value value
             :checked checked?}]))

(defn make-regular-radio-group
  ([name active options id-maker]
     (hiccat
      [:div
       :cat$
       (map (fn [[kw vlu display]] 
              [:label {:class "radio"}
               (radio-button name (= active kw) vlu (id-maker name vlu))
                display])
            options)])))

(defn make-regular-select-group [group-class group-id name active options class-maker]
  (hiccat
   [:div
    [:select (merge {:name name}
                    (if (and group-class (not (string/blank? group-class)))
                      {:class group-class}
                      {})
                    (if (and group-id (not (string/blank? group-id)))
                      {:id group-id}
                      {}))
     (map (fn [[kw vlu display]]
            [:option {:name name
                      :selected (= active kw)
                      :value vlu
                      :class (class-maker name vlu)}
             display])
          options)]]))



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



(defn make-generic-inputs [params]
  :input params)

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
    [:div {:class "row-fluid essentials"}
     [:div {:class "control-group preferred-name-email-phone"}
      [:label "Preferred name, email, phone"]
      (hiccat
       [:div {:class "controls controls-row"}
        :cat$
        ((make-input-maker [{:type "text"}])
         (map
          (fn [[l & attribs]]
            (let [[n v p sz] attribs]
              [{:name n :value v :placeholder l :class sz
                }]))
          [["Preferred name" "preferred-name"
            (-> client :name :preferred) "e.g. \"Robbie\"" "span3 name preferred-name"]
           ["Email" "email-address"
            (-> client :primary-email :address)
            "e.g. \"blank@blankety.com\"" "span4 email primary-email"]
           ["CC" "phone-country-code"
            (-> client :primary-phone :country-code)
            "eg. +1" "span1 phone primary-phone"]
           ["Main phone number" "phone-main-number"
            (-> client :primary-phone :main-number)
            "eg. 555-555-5555" "span2 phone primary-phone"]
           ["Extension" "phone-extension"
            (-> client :primary-phone :extension)
            "eg. \"ex. 555\"" "span2 phone primary-phone"]]))
        (if false
          [:span {:class "email-or-call-client"}
           [[:a {:class "mailto-link" :href (-> client :primary-email :mailto-link)}
             "Email "]
            "or "
            [:a {:class "tel-link" :href (-> client :primary-phone :tel-link)}
             "call."]]])])]
     [:div {:class "control-group full-name"}
      [:label "Full name"]
      (hiccat
       [:div {:class "controls-row controls"}
        :cat$
        ((make-input-maker [{:type "text" :class "name"}])
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
            (-> client :name :post-fix) "e.g. Jr, IV, etc." "span2"]]))])]]]))

(defn cprofile-health [client]
  [:div
   [:legend "Complaints and Diagnoses"]
   [:div {:class "row-fluid complaints-diagnoses"}
    (hiccat
     [:ol
      :cat$
      (interpose
       [:hr]
       (map
        (fn [[id complaint-diagnosis]]
          [:li {:class "row-fluid"}
           [:div {:class "control-group complaint-diagnosis"
                  :id (str "complaint-diagnosis" id)}
            [:label {:class "main-label"}
             (str "C/D: "
                  (subs (-> complaint-diagnosis :complaint) 0
                        (if (> (count (-> complaint-diagnosis :complaint)) 35)
                          35
                          (count (-> complaint-diagnosis :complaint))))
                  "...")]
            [:label {:class "complaint"} [:small "Complaint"]]
            [:div {:class "controls complaint"}
             [:textarea {:rows "2" :class "span12"
                         :name (str "complaint"
                                    (-> complaint-diagnosis :id))
                         :placeholder "Client's complaint/problem"}
              (-> complaint-diagnosis :complaint)]
             [:span {:class "help-block"}
              (str "Last modified on "
                   (fdate
                    (-> complaint-diagnosis :complaint-last-modified)))]]
            [:label {:class "diagnosis"} [:small "Diagnosis"]]
            [:div {:class "controls diagnosis"}
             [:textarea {:rows "4" :class "span12"
                         :name (str "diagnosis"
                                    (-> complaint-diagnosis :id))
                         :placeholder "Diagnosis to above complaint"}
              (-> complaint-diagnosis :diagnosis)]
             [:span {:class "help-block"}
              (str "Last modified on "
                   (fdate
                    (-> complaint-diagnosis :diagnosis-last-modified)))]]]])
        (client :complaint-diagnosis)))])]
   ;;;
   ;;;
   ;;
   ;;;;
   ;;
   ;;
   ;;;;;
   ;;;;
   ;;;
   ;;
   ;;

   ;;;
   ;;;;
   (hiccat
    [:div {:class "row-fluid health-ill-health"}
     [:legend "Health and Ill-health"]
     [:div {:class "row-fluid"}
      (make-bs-control-group
       {:class "history"}
       "History / Environmental factors"
       {}
       [[:textarea {:rows "4" :class "span12" :name "history"
                    :placeholder "Note down the client's history and any environmental factors here."}
         (-> client :history :history)]]
       {}
       [(str "Last modified on "
             (fdate (-> client :history :last-modified)))])]])])

(defn cprofile-perpetual-professional [client]
  [:div {:class "perpetual-professional-notes"}
   [:legend "Professional notes"]
   [:div
    (make-bs-control-group
     {}
     nil
     {}
     [[:textarea {:rows "10" :class "span12" :name "perpetual-professional-notes"
                  :placeholder "Make notes to remind yourself of peculiarities of working with this client here (eg. places they don't like to be touched, etc)."}
       (if-not (string/blank?
                (-> client :perpetual-professional-notes :perpetual-professional-notes))
         (-> client :perpetual-professional-notes :perpetual-professional-notes))]]
     {}
     [(str "Last modified on "
           (fdate (-> client :perpetual-professional-notes :last-modified)))])]])

(defn cprofile-perpetual-personal [client]
  [:div {:class "perpetual-personal-notes"}
   [:legend "Personal notes"]
   [:div
    (make-bs-control-group
     {}
     nil
     {}
     [[:textarea {:rows "10" :class "span12" :name "perpetual-personal-notes"
                  :placeholder "Make notes to remind yourself of personal/social details of this client (eg. things they don't like to talk about, etc)."}
       (if-not (string/blank?
                (-> client :perpetual-personal-notes :perpetual-personal-notes))
         (-> client :perpetual-personal-notes :perpetual-personal-notes))]]
     {}
     [(str "Last modified on "
           (fdate (-> client :perpetual-personal-notes :last-modified)))])]])

(defn cprofile-demographic [client]
  [:div {:class "demographic"}
   [:legend "Demographics"]
   [:div 
    [:div {:class "control-group gender"}
     [:label {:class "control-label"} "Gender"]
     [:div {:class "controls"}
      (make-regular-select-group nil
                                  nil
                                 "gender"
                                 (-> client :demographic :gender)
                                 (for [[kw st] gender-options]
                                   [kw st (string/capitalize st)])
                                 #(identity %2))]]
    [:div {:class "control-group sex"}
     [:label {:class "control-label"} "Sex"]
     [:div {:class "controls"}
      (make-regular-select-group nil
                                 nil
                                "sex"
                                (-> client :demographic :sex)
                                (for [[kw st] sex-options]
                                  [kw st (string/capitalize st)])
                                #(identity %2))]]
    [:div {:class "control-group age-group"}
     [:label {:class "control-label"} "Age Group"]
     [:div {:class "controls"}
      (make-regular-select-group nil
                                 nil
                                "age-group"
                                (-> client :demographic :age-group)
                                (for [[kw st] age-group-options]
                                  [kw st (string/capitalize st)])
                                #(identity %2))]]
     [:div {:class "ethnicity"}
      (make-bs-control-group
       {:class "control-label"}
       "Ethnicity"
       {}
       ((make-input-maker [{:type "text" :class "span8"}])
        [[{:value (-> client :demographic :ethnicity)
           :name "ethnicity"
                  :placeholder "Ethnicity"}]]))]
     [:div {:class "profession"}
      (make-bs-control-group
       {:class "control-label"}
       "Profession"
       {}
       ((make-input-maker [{:type "text" :class "span8"}])
        [[{:value (-> client :demographic :profession)
           :name "profession"
           :placeholder "Profession"}]]))]]])

(defn cprofile-addresses [client]
  (hiccat
   [:div {:class "address"}
    [:legend "Address(es)"]
    (let [client-address (-> client :address)]
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
  [:form {
          :action (make-cprofile-url (-> client :id))
          :method "post"
          :class "cprofile-form"}
   [:fieldset
    [:div {:class "row-fluid"}
     [:div {:class "span11"} (cprofile-essentials client)]]
    [:div {:class "row-fluid"}
     [:div {:class "span11"}
      [:div {:class "span6 split-form split-form-left"}
       [:div (cprofile-perpetual-professional client)]]
      [:div {:class "span6 split-form"}
       [:div (cprofile-perpetual-personal client)]]]]
    [:div {:class "row-fluid"}
     [:div {:class "span11"}
      (cprofile-health client)]]
    [:div {:class "row-fluid"}
     [:div {:class "span11"}
      [:div {:class "span5 split-form split-form-left"}
       [:div (cprofile-demographic client)]]
      [:div {:class "span5 split-form"}
       [:div (cprofile-addresses client)]]]]
    [:input {:type "submit" :value "Save"
             :class "btn btn-primary"}]]])

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
             (client-menu active-client clients)]]
           [:div {:class "span10"}
            (hiccat
             [:div {:class "tab-content"}
              [:div {:class (if (= active-client :none)
                              "tab-pane active"
                              "tab-pane")
                     :id "noclient"}
               "Home! "]
              :cat$
              (for [client clients
                    :let [id (-> client :id)]
                    :let [full-name-with-trappings (-> client :name :full-with-trappings)]]
                [:div {:class (if (= active-client (-> client :id))
                                "tab-pane active"
                                "tab-pane")
                       :id (str "client" id)}
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
                  [:div {:class "tab-pane active cprofile"}
                   (cprofile client)]
                  [:div {:class "tab-pane session-notes"}
                   "Dragon Land."]
                  [:div {:class "tab-pane worksheet"}
                   "Wagon Land."]]])])]]]]]
    [:html hd bd]))
