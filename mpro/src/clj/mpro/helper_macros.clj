(ns mpro.helper-macros)
      
(defmacro hiccup-for [body]
  (let [make-for-concat
        (fn [body acc]
          (if (seq body)
            (if (= (first body) :for$)
              (recur
               (drop 2 body)
               (conj acc  (list 'for
                                (apply vector (first (second body)))
                                (second (second body)))))
              (recur (drop 1 body) (conj acc [(first body)])))
            acc))
        for-concatting (make-for-concat body [])]
    `(apply vector (concat ~@for-concatting))))(defmacro test-hiccup-for [body]
  ((fn [body acc]
          (if (seq body)
            (if (= (first body) :for$)
              (recur
               (drop 2 body)
               (conj acc  (list 'for
                                (apply vector (first (second body)))
                                (second (second body)))))
              (recur (drop 1 body) (conj acc [(first body)])))
            acc)) body []))
