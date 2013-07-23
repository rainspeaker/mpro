(ns mpro.clients
  (:require [mpro.dbclient :as c]
            [clojure.string :as string])
  (:use [korma.core]
        [mpro.helpers]
        [mpro.helper-macros :only [or=]]))

(def client-status-map
  {:current "current"
   :dormant "dormant"
   :former "former"})

(def rev-client-status-map
  (revmap-shallow client-status-map))

(def bool-map
  {true "true" false "false"})

(def rev-bool-map
  (revmap-shallow bool-map))

(def client-gender-map
  {:female "female"
   :male "male"
   :other "other"})

(def rev-client-gender-map
  (revmap-shallow client-gender-map))

(def client-sex-map
  {:female "female"
   :male "male"
   :intersex "intersex"})

(def rev-client-sex-map
  (revmap-shallow client-gender-map))

(def client-age-group-map
  {:child "child"
   :teenager "teenager"
   :adult "adult"
   :senior "senior"})

(def rev-client-age-group-map
  (revmap-shallow client-age-group-map))

(defn get-client-ids-with-group [[status ispaid]]
  (map :id (select c/client
           (fields :id)
           (where {:status (client-status-map status)
                   :ispaid (bool-map ispaid)}))))

;; (def buildersfns
;;   {:status build-status
;;    :demographic build-demographic
;;    :names build-names
;;    :billing build-billing
;;    :complaint build-complaint
;;    :perpetual-personal-notes build-perpetual-personal-notes
;;    :perpetual-professional-notes build-perpetual-professional-notes
;;    :session-notes build-session-notes
;;    :body-drawings build-body-drawings
;;    :session-history build-session-history})

;; (defn construct-client [id switches]
;;   (reduce
;;    (fn [client [switch switch-value]]
;;      (assoc client switch ((builderfns switch) switch-value)))
;;    {:id id}
;;    switches))

(def client-withs
  (-> (select* c/client)
      (with c/address
            (fields :mailing-name
                    :street-address
                    :city-state
                    :zip))
      (with c/name
            (fields
             :first
             :middle
             :last
             :honorific
             :post-fix
             :preferred))
      (with c/demographic
            (fields
             :birthdate
             :gender
             :sex
             :ethnicity
             :profession
             :age-group))
      (with c/email
            (fields :address :defunct))
      (with c/phone
            (fields
             :country-code
             :main-number
             :extension
             :notes))
      (with c/what-brings
            (fields
             :reason
             :date))
      (with c/billing-info
            (fields
             :preferred-billing-method
             :billing-numbers))
      (with c/billing-notes
            (fields
             :note
             :billing-note-date
             :active))
      (with c/technology)
      (with c/complaint
            (fields
             :complaint
             :complaint-date))
      (with c/family-history
            (fields :family-history))
      (with c/health-notes
            (fields :health-notes))
      (with c/perpetual-personal-notes
            (fields :perpetual-personal-notes))
      (with c/perpetual-professional-notes
            (fields :perpetual-professional-notes))))

(defn client-fields [qry-so-far]
  (-> qry-so-far
      (fields :id :status :ispaid)))



(defn construct-client-by-id [id]
  (-> (client-fields client-withs)
      (where {:id id})
      (select)))

(defn make-singles [res]
  (reduce
   (fn [res propkey]
     (let [propseq (res propkey)]
       (if (not= 1 (count propseq))
         (throw (Exception. (str "number of " propkey " for client (id " (res :id) ") not= 1")))
         (let [prop (first propseq)]
           (assoc res propkey prop)))))
   res [:demographic :family-history :billing-info :health-notes
        :perpetual-professional-notes :perpetual-personal-notes
        :name]))

(defn fix-status-and-ispaid [res]
  (if (and (contains? res :ispaid) (contains? res :status)
           (string? (res :ispaid)) (string? (res :status)))
    (-> res
        ((fn [res]
           (assoc res :ispaid (rev-bool-map (res :ispaid)))))
        ((fn [res]
           (assoc res :status (rev-client-status-map (res :status))))))
    res))

(defn make-any-vectors-into-sequences [res]
  (apply hash-map
         (mapcat 
          (fn [[k v]]
            [k (if (vector? v) (seq v) v)])
          res)))

(defn get-primary-email [email]
  (first email))

(defn get-primary-phone [phone]
  (first phone))

(defn make-convenience-properties [res]
  (-> res
      ((fn [res]
         (-> res
             (assoc-interior
              (string/join
               " "
               (filter (complement nil?)
                       [(-> res :name :first)
                        (-> res :name :middle)
                        (-> res :name :last)]))
              :name :full)
             (assoc-interior
              (string/join
               " "
               ;; filter nil or strings equal to the literal characters
               ;; "". "" may show up if there is no valued for
               ;; :preferred or :preferred = :first. 
               (filter #(not (or (nil? %) (= "\"\"" %)))
                       [(-> res :name :honorific)
                        (-> res :name :first)                    
                        (str "\""
                             (when (not=
                                    (-> res :name :first)
                                    (-> res :name :preferred))
                               (-> res :name :preferred))
                             "\"")
                        (-> res :name :middle)
                        (-> res :name :last)
                        (-> res :name :post-fix)]))
              :name :full-with-trappings)
             ((fn [res]
                (assoc res :email
                       (map (fn [email]
                              (assoc email :mailto-link (str "mailto:" (email :address))))
                            (res :email)))))
             ((fn [res]
                (assoc res :phone
                       (map (fn [phone]
                              (assoc phone :tel-link
                                     (str
                                      "tel:+"
                                      (phone :country-code)
                                      (phone :main-number))))
                            (res :phone)))))
             ((fn [res]
                (assoc res :primary-email
                       (get-primary-email (res :email)))))
             ((fn [res]
                (assoc res :primary-phone
                       (get-primary-phone (res :phone)))))
             )))))

(defn construct-clients-by-group [status ispaid]
  (let
      ;; put together the constraints to pass to korma based on the
      ;; arguments status and ispaid. status can be
      ;; :current/:dormant/:former, or :any. ispaid can be a bool or
      ;; :any. 
      [wheres (-> {}
                  (#(cond
                     (or= status :current :dormant :former)
                     (assoc
                         %
                       :status
                       (client-status-map status))
                     (= status :any) %
                     :else (throw (Exception. "status was other than :current/:dormant/:former or :any"))))
                  (#(cond
                     (or= ispaid true false)
                     (assoc
                         %
                       :ispaid
                       (bool-map ispaid))
                     (= :any ispaid) %
                     :else (throw (Exception. "ispaid was other than bool or :any")))))
       initial-results (-> client-withs
                           (where wheres)
                           (select))]
    
    (map
     (fn [initial-result]
       (-> initial-result
           (make-singles)
           (make-any-vectors-into-sequences)
           (fix-status-and-ispaid)
           (make-convenience-properties)))
     initial-results)))






;;******************************************************************************
;;Inserting / Updating Clients
;;******************************************************************************

()
