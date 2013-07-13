(ns mpro.clients)



(def buildersfns
  {:status build-status
   :demographic build-demographic
   :names build-names
   :billing build-billing
   :complaint build-complaint
   :perpetual-personal-notes build-perpetual-personal-notes
   :perpetual-professional-notes build-perpetual-professional-notes
   :session-notes build-session-notes
   :body-drawings build-body-drawings
   :session-history build-session-history})

(defn construct-client [id switches]
  (reduce
   (fn [client [switch switch-value]]
     (assoc client switch ((builderfns switch) switch-value)))
   {:id id}
   switches))

(defn construct-clients [client-group-switches]
  (reduce
   (fn [clients [client-group group-switches]]
     (concat
      clients
      (map
       (fn [client-id]
         (construct-client client-id group-switches))
       (get-client-ids-with-group client-group))))
   [] client-group-switches))

(defn grab-clients [& client-groups]
  (construct-clients
   (reduce (fn [client-group-switches client-group]
             (assoc client-group-switches client-group
                    ({ [:current
                        :pro-bono] {:id true
                                    :status true
                                    :demographic true
                                    :names true
                                    :billing true
                                    :technology true
                                    :complaint true
                                    :health true
                                    :perpetual-personal-notes true
                                    :perpetual-professional-notes true
                                    :session-notes true
                                    :body-drawings true
                                    :session-history true}
                        [:current
                         :paid] {:id true
                                 :status true
                                 :demographic true
                                 :names true
                                 :billing true
                                 :technology true
                                 :complaint true
                                 :health true
                                 :perpetual-personal-notes true
                                 :perpetual-professional-notes true
                                 :session-notes true
                                 :body-drawings true
                                 :session-history true}
                         [:dormant
                          :pro-bono] {:id true
                                      :status true
                                      :demographic true
                                      :names true
                                      :billing true
                                      :technology true
                                      :complaint true
                                      :health true
                                      :perpetual-personal-notes true
                                      :perpetual-professional-notes true
                                      :session-notes true
                                      :body-drawings true
                                      :session-history true}
                          [:dormant
                           :paid] {:id true
                                   :status true
                                   :demographic true
                                   :names true
                                   :billing true
                                   :technology true
                                   :complaint true
                                   :health true
                                   :perpetual-personal-notes true
                                   :perpetual-professional-notes true
                                   :session-notes true
                                   :body-drawings true
                                   :session-history true}
                           [:former
                            :pro-bono] {:id true
                                        :status true
                                        :demographic true
                                        :names true
                                        :billing true
                                        :technology true
                                        :complaint true
                                        :health true
                                        :perpetual-personal-notes true
                                        :perpetual-professional-notes true
                                        :session-notes true
                                        :body-drawings true
                                        :session-history true}
                            
                            [:former
                             :paid] {:id true
                                     :status true
                                     :demographic true
                                     :names true
                                     :billing true
                                     :technology true
                                     :complaint true
                                     :health true
                                     :perpetual-personal-notes true
                                     :perpetual-professional-notes true
                                     :session-notes true
                                     :body-drawings true
                                     :session-history true}}
                     client-group)))
           {} client-groups)))
