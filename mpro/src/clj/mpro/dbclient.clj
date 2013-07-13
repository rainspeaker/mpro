(ns mpro.dbclient
  (:require [mpro.dbconfig :as d]
            [clojure.java.jdbc :as j]
            [clojure.java.jdbc.sql :as s]
            [clojure.string :as str])
  (:use [korma.core]
        [mpro.helpers]))

(declare account
         client
         client-name
         demographic
         address
         email
         phone
         what-brings
         billing-info
         billing-notes
         technology
         complaint
         family-history
         health-notes
         perpetual-personal-notes
         perpetual-professional-notes
         session-notes
         body-drawing
         session
         exercise
         workbook
         workbook-section)

(defn prepr-by-fn [qry k f err]
  (if  (not= nil (qry k))
    (if-let [matchup (f (qry k))]
      (assoc qry k matchup)
      (do
        (throw (Exception. err))
        qry))
    qry))

(defn transfm-by-fn [res k f]
  (if (not= nil (res k))
    (assoc res k (f (res k)))
    res))

(def bool-enums-prep
  {true "true"
   false "false"})

(def bool-enums-tran
  (revmap-shallow bool-enums-prep))

(defentity account
  (has-one client))

(defentity client
  (database d/db)
  (entity-fields :id :status :ispaid)
  (belongs-to account)
  (has-one client-name)
  (has-one demographic)
  (has-many address)
  (has-many email)
  (has-many phone)
  (has-many what-brings)
  (has-one billing-info)
  (has-many billing-notes)
  (many-to-many technology :client_technology)
  (has-many complaint)
  (has-one family-history)
  (has-one health-notes)
  (has-one perpetual-personal-notes)
  (has-one perpetual-professional-notes)
  (has-many session-notes)
  ;; (has-many body-drawing)
  (has-many session)
  (many-to-many exercise :client_exercise)
  (has-many workbook)

  (prepare (fn [stuff]
             (do 
                 (let [stuff (prepr-by-fn
                           stuff :status
                           {:current "current"
                            :dormant "dormant"
                            :former "former"}
                           "Tried to add client status other than :current/:dormant/:former")
                    stuff (prepr-by-fn
                           stuff :ispaid
                           {true "true" false "false"}
                           "Tried to add client ispaid other than bool")]
                stuff))))
  (transform (fn [stuff]
               (do 
                 (let [stuff (transfm-by-fn
                              stuff :status
                              {"current" :current
                               "dormant" :dormant
                               "former" :former}) 
                       stuff (transfm-by-fn
                              stuff :ispaid
                              {"true" true "false" false})]
                   stuff)))))

(defentity client-name
  (database d/db)
  (entity-fields :first-name
                 :middle-name
                 :last-name
                 :honorific
                 :post-fix
                 :preferred-name
                 :client_id)
  (belongs-to client)
  (prepare (fn [qry]
             (reduce #(prepr-by-fn %1 %2 str/trim)
                     qry
                     [:first-name :middle-name
                      :last-name :honorific
                      :post-fix]))))

(def demographic-enums-prep
  {:gender {:male "male"
            :female "female"
            :other "other"}
   :sex {:male "male"
         :female "female"
         :intersex "intersex"}
   :age-group {:child "child"
               :teenager "teenager"
               :adult "adult"
               :senior "senior"}})
(def demographic-enums-tran
  {:gender (revmap-shallow (demographic-enums-prep :gender))
   :sex (revmap-shallow (demographic-enums-prep :sex))
   :age-group (revmap-shallow (demographic-enums-prep :age-group))})

(defentity demographic
  (database d/db)
  (entity-fields :birthdate
                 :gender
                 :sex
                 :ethnicity
                 :profession
                 :age-group
                 :client_id)
  (belongs-to client)
  (prepare (fn [qry]
             (-> qry
                 (prepr-by-fn
                  :gender
                  (demographic-enums-prep :gender)                  
                  "Tried to put a gender other than :male/:female/:other")
                 (prepr-by-fn
                  :sex
                  (demographic-enums-prep :sex)                  
                  "Tried to put a sex other than :male/:female/:intersex")
                 (prepr-by-fn
                  :age-group
                  (demographic-enums-prep :age-group)                  
                  "Tried to put an age-group other than :child/:teenager/:adult/:senior"))))
  (transform (fn [res]
               (-> res
                   (transfm-by-fn
                    :gender
                    (demographic-enums-tran :gender))
                   (transfm-by-fn
                    :sex
                    (demographic-enums-tran :sex))
                   (transfm-by-fn
                    :age-group
                    (demographic-enums-tran :age-group))))))

(defentity billing-info
  (database d/db)
  (entity-fields :preferred-method
                 :numbers)
  (belongs-to client))

(defentity billing-notes
  (database d/db)
  (entity-fields :note :active)
  (belongs-to client)
  (prepare (fn [qry]
             (prepr-by-fn qry :active bool-enums-prep
                          "Tried to put a non-bool into a billing-notes :active slot")))
  (transform (fn [res]
               (transfm-by-fn res :active bool-enums-tran))))

(defentity address
  (database d/db)
  (entity-fields :mailing-name :street-address :city-state :zip)
  (belongs-to client))

(defentity email
  (database d/db)
  (entity-fields :address :defunct)
  (belongs-to client)
  (prepare (fn [qry]
             (prepr-by-fn qry :defunct bool-enums-prep
                          "Tried to put a non-bool into an email :defunct slot")))
  (transform (fn [res]
               (transfm-by-fn res :defunct bool-enums-tran))))

(defentity phone
  (database d/db)
  (entity-fields :country-code :main-number :extension :notes)
  (belongs-to client))

(defentity complaint
  (database d/db)
  (entity-fields :date :complaint)
  (belongs-to client))

(defentity what-brings
  (database d/db)
  (entity-fields :reason :date)
  (belongs-to client))

(defentity technology
  (database d/db)
  (entity-fields :name)
  (transform identity)
  (many-to-many client :client_technology))

(defentity family-history
  (database d/db)
  (entity-fields :history)
  (belongs-to client))

(defentity health-notes
  (database d/db)
  (entity-fields :notes)
  (belongs-to client))

(defentity perpetual-personal-notes
  (database d/db)
  (entity-fields :notes)
  (belongs-to client))

(defentity perpetual-professional-notes
  (database d/db)
  (entity-fields :notes)
  (belongs-to client))

(defentity session
  (database d/db)
  (entity-fields :date :summary)
  (belongs-to client))

(defentity session-notes
  (database d/db)
  (entity-fields :notes)
  (belongs-to client)
  (belongs-to session))

(defentity exercise
  (database d/db)
  (entity-fields :id :title :pre-notes :post-notes :parent)
  (many-to-many client :client_exercise))

(defentity workbook
  (database d/db)
  (entity-fields :id :title)
  (belongs-to client)
  (has-many workbook-section))

