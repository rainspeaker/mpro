(ns mpro.helpers)

(defn ind [m & ks]
  (if (seq ks)
    (recur (m (first ks)) (rest ks))
    m))
