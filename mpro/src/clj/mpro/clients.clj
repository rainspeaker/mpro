(ns mpro.clients
  (:require [mpro.dbclient :as c]
            [clojure.string :as string]
            [clojure.set :as set])
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
      (with c/complaint-diagnosis
            (fields
             :complaint
             :complaint-last-modified
             :diagnosis
             :diagnosis-last-modified
             :id))
      (with c/complaint
            (fields
             :complaint
             :complaint-date))
      (with c/history
            (fields
             :history
             :last-modified))
      (with c/diagnosis
            (fields
             :diagnosis
             :last-modified))
      (with c/perpetual-personal-notes
            (fields
             :perpetual-personal-notes
             :last-modified))
      (with c/perpetual-professional-notes
            (fields
             :perpetual-professional-notes
             :last-modified))))

(defn client-fields [qry-so-far]
  (-> qry-so-far
      (fields :id :status :ispaid)))





(defn make-singles [res]
  (reduce
   (fn [res propkey]
     (let [propseq (res propkey)]
       (if (not= 1 (count propseq))
         (throw (Exception. (str "number of " propkey " for client (id " (res :id) ") not= 1")))
         (let [prop (first propseq)]
           (assoc res propkey prop)))))
   res [:demographic :history :billing-info :diagnosis
        :perpetual-professional-notes :perpetual-personal-notes
        :name :address :email :phone]))

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
  (identity email))

(defn get-primary-phone [phone]
  (identity phone))

(defn make-multiples-into-id->prop-maps [res]
  (reduce
   (fn [res key-of-multiple]
     (assoc (dissoc res key-of-multiple)
       key-of-multiple
       (reduce
        (fn [propmap prop]
          (assoc propmap (prop :id) prop))
        {}
        (res key-of-multiple))))
   res
   [:complaint-diagnosis]))

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
                       (assoc (res :email) :mailto-link
                              (str "mailto:" (-> res :email :address))))))
             ((fn [res]
                (assoc res :phone
                       (assoc (res :phone) :tel-link
                              (str
                               "tel:+"
                               (-> res :phone :country-code)
                               (-> res :phone :main-number))))))
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
           (make-multiples-into-id->prop-maps)
           (make-convenience-properties)))
     initial-results)))


(defn construct-client-by-id [id]
  (let [initial-results (-> client-withs
                            (where {:id id})
                            (select))]
    (do (assert (< (count initial-results) 2))
        (first (map
                (fn [initial-result]
                  (-> initial-result
                      (make-singles)
                      (make-any-vectors-into-sequences)
                      (fix-status-and-ispaid)
                      (make-multiples-into-id->prop-maps)
                      (make-convenience-properties)))
                initial-results)))))




;;******************************************************************************
;;Inserting / Updating Clients
;;******************************************************************************



(def property-positions-translator
  {:phone-extension [:phone :extension]
   :phone-country-code [:phone :country-code]
   :phone-main-number [:phone :main-number]

   :email-address [:email :address]
   
   :gender [:demographic :gender]
   :sex [:demographic :sex]
   :age-group [:demographic :age-group]
   :profession [:demographic :profession]
   :ethnicity [:demographic :ethnicity]

   :first-name [:name :first]
   :middle-name [:name :middle]
   :last-name [:name :last]
   :preferred-name [:name :preferred]
   :honorific-name [:name :honorific]
   :post-fix-name [:name :post-fix]

   :zip [:address :zip]
   :street-address [:address :street-address]
   :mailing-name [:address :mailing-name]
   :city-state [:address :city-state]

   :perpetual-professional-notes [:perpetual-professional-notes :perpetual-professional-notes]
   
   :perpetual-personal-notes [:perpetual-personal-notes :perpetual-personal-notes]

   ;;:complaint [:complaint :complaint]

   :history [:history :history]

   ;;:diagnosis [:diagnosis :diagnosis]
   })


(defn form-structured-map [key->coords m]
  (reduce
   (fn [acc [old-key new-coords]]
     (if (contains? m old-key)
       (apply assoc-interior
              (concat [acc (ind m [old-key])] new-coords))
       acc))
   {}
   key->coords))


(defn make-map-keys-keywords [m]
  (reduce (fn [acc [k v]]
            (set/rename-keys acc {k (keyword k)}))
          m
          m))

(defn transfm-by-fn-interior [res f ks]
  (if (seq (rest ks))
    (if (contains? res (first ks))
      (assoc res (first ks)
             (transfm-by-fn-interior
              (res (first ks))
              f
              (rest ks)))
      res)
    (if (contains? res (first ks))
      (assoc res (first ks) (f (res (first ks))))
      res)))

(defn trim-what-must-be-trimmed [partial-client]
  (reduce
   (fn [partclt coords]
     (transfm-by-fn-interior partclt
                              string/trim
                              coords))
   partial-client
   [[:phone :extension]
    [:phone :country-code]
    [:phone :main-number]
    [:email :address]
    [:demographic :profession]
    [:demographic :ethnicity]
    [:name :first]
    [:name :middle]
    [:name :last]
    [:name :preferred]
    [:name :post-fix]
    [:address :zip]
    [:address :street-address]
    [:address :mailing-name]
    [:address :city-state]
    [:perpetual-professional-notes :perpetual-professional-notes]
    [:perpetual-personal-notes :perpetual-personal-notes]
    [:complaint :complaint]
    [:history :history]
    [:diagnosis :diagnosis]]))

(defn make-enum-strings-into-keywords [partial-client]
  (reduce
   (fn [partclt coords]
     (transfm-by-fn-interior partclt
                             keyword
                             coords))
   partial-client
   [[:demographic :sex]
    [:demographic :age-group]
    [:demographic :gender]]))


(defn nilify-some-empty-strings [partial-client]
  (reduce
   (fn [partclt coords]
     (transfm-by-fn-interior partclt
                             #(if (string/blank? %) nil %)
                             coords))
   partial-client
   [[:name :honorific]
    [:name :first]
    [:name :middle]
    [:name :last]
    [:name :post-fix]
    [:name :preferred]
    [:phone :extension]]))


;; HAVE TO MAKE MORE THINGS NULLABLE!!

(defn only-different-properties [reference-values new-values]
  (let [nid (new-values :id)
        oid (reference-values :id)
        diffs
        (reduce
         (fn [acc [k v]]
           (if (map? v)
             (let [prop-diffs (only-different-properties (reference-values k) v)]
               (if (empty? prop-diffs)
                 acc
                 (assoc acc k prop-diffs)))
             (if (= v (reference-values k))
               acc
               (assoc acc k v))))
         {}
         (dissoc new-values :id))]
    (do (assert (= nid oid) "IDS ARE DIFFERENT! SOMETHING IS SERIOUSLY WRONG!")
        (if nid
          (assoc diffs :id nid)))))

(defn post-to-partial-singles [singles]
  (->> singles
      (make-map-keys-keywords)
      (form-structured-map property-positions-translator)
      (trim-what-must-be-trimmed)
      (make-enum-strings-into-keywords)
      (nilify-some-empty-strings)))

(defn grab-by-starting-string [patterns m]
  (apply
   hash-map
   (apply
    concat
    (filter
     (fn [[k v]]
       (reduce #(if (not %1) (re-find %2 k) %1)
               nil
               patterns))
     m))))

(defn make-into-number-keyed-maps [m]
  (-> m
      ((fn [m]
         (reduce
          (fn [acc [k v]]
            (cond
             (re-find #"^complaint" k) (dissoc
                                        (assoc-interior
                                         acc
                                         v
                                         :complaint
                                         (Integer/parseInt
                                          (string/replace k #"complaint" ""))
                                         :complaint)
                                        k)
             (re-find #"^diagnosis" k) (dissoc
                                        (assoc-interior
                                         acc
                                         v
                                         :diagnosis
                                         (Integer/parseInt
                                          (string/replace k #"diagnosis" ""))
                                         :diagnosis)
                                        k)
             :else acc))
          {}
          m)))
      ((fn [m]
         (dissoc
          (assoc
              m
            :complaint-diagnosis
            (merge-with merge (m :complaint) (m :diagnosis)))
          :complaint
          :diagnosis)))))

(def prop-max-lengths
  {[:address :mailing-name] 250
   [:address :street-address] 100
   [:address :city-state] 100
   [:address :zip] 15

   [:complaint-diagnosis :complaint] 10000
   [:complaint-diagnosis :diagnosis] 25000

   [:demographic :ethnicity] 100
   [:demographic :profession] 100

   [:email :address] 300

   [:history :history] 25000

   [:name :first] 40
   [:name :middle] 40
   [:name :last] 40
   [:name :honorific] 40
   [:name :post-fix] 40
   [:name :preferred] 40

   [:perpetual-personal-notes :perpetual-personal-notes] 50000

   [:perpetual-professional-notes :perpetual-professional-notes] 50000

   [:phone :country-code] 3
   [:phone :main-number] 10
   [:phone :extension] 20
   [:phone :notes] 250})

;; (defn validator [partial-client]
;;   (-> [partial-client
;;        {:fields-too-long []}]
;;       ((fn check-and-fix-phone-format [[pc errors]]
;;          (reduce
;;           (fn [err [propcoord max-length]]
;;             ))))
;;       ((fn check-lengths [pc]
;;          ()))
;;       ((fn bad-request-if-bad-enums))))


(defn post-to-partial-multiples [multiples]
  (->> multiples
       (grab-by-starting-string [#"^complaint" #"^diagnosis"])
       (make-into-number-keyed-maps)))

(defn post-to-partial-client [req client]
  (->> (req :form-params)
       (post-to-partial-singles)
       (merge
        (post-to-partial-multiples (req :form-params)))
       (#(assoc % :id (client :id)))
       ;;(validator)
       (only-different-properties client)))

(def propkey-to-table
  {:demographic c/demographic
   :complaint c/complaint
   :billing-info c/billing-info
   :billing-notes c/billing-notes
   :history c/history
   :name c/name
   :perpetual-professional-notes c/perpetual-professional-notes
   :perpetual-personal-notes c/perpetual-personal-notes
   :phone c/phone
   :email c/email
   :address c/address
   :diagnosis c/diagnosis})

(defn update-partial-client [partial-client]
  (do
    (doseq [[propkey prop] (dissoc partial-client
                                   :id
                                   :complaint-diagnosis)
            :let [tbl (propkey-to-table propkey)]]
      (update tbl
              (set-fields prop)
              (where {:client_id (partial-client :id)})))
    (doseq [[cd-id cd] (partial-client :complaint-diagnosis)]
      (update c/complaint-diagnosis
              (set-fields cd)
              (where {:client_id (partial-client :id) :id cd-id})))))
