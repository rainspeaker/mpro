(ns mpro.post-cprofile
  (:require [clojure.string :as string])
  (:use [mpro.helpers]))

(def param-positions-map
  {:phone-extension [:phone :extension]
   :phone-country-code [:phone :country-code]
   :phone-main-number [:phone :main-number]
   
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

   :complaint [:complaint :complaint]

   :history [:history :history]

   :diagnosis [:diagnosis :diagnosis]
   })


(defn form-structured-map [key->coords m]
  (reduce
   (fn [acc [old-key new-coords]]
     (if (contains? m old-key)
       (apply assoc-interior
              (concat [(ind m old-key) acc] new-coords))
       acc))
   {}
   key->coords))


(defn make-map-keys-keywords [m]
  (reduce (fn [acc [k v]]
            rename-keys acc {k (keyword k)})
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
     (tranfm-by-fn-interior partclt
                              string/trim
                              coords))
   partial-client
   [[:phone :extension]
    [:phone :country-code]
    [:phone :main-number]
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


(defn only-different-properties [reference-values new-values]
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
   new-values))


(defn post-to-partial-client [req client]
  (->> (req :form-params)
       (make-map-keys-keywords)
       (structure-map property-positions-translator)
       (trim-what-must-be-trimmed)
       (make-enum-strings-into-keywords)
       (only-different-properties client)
       (#(assoc % :id (client :id)))))
