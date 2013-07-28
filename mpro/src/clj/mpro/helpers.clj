(ns mpro.helpers
  (:require [clojure.string :as string]))

(defn two-or-more? [s]
  (seq (rest s)))

(defn ind [m ks]
  "like using -> to index into a submap, but works with non-keyword indices. tail recursive."
  (if (seq ks)
    (recur (m (first ks)) (rest ks))
    m))



(defn revmap-shallow
  "reverse the direction of a map's binding ... (revmap-shallow {:a 1, :b 2}) => {1 :a, 2 :b}"
  [m]
  (apply hash-map (mapcat reverse m)))

(defn assoc-interior
  "like assoc but lets you associate on submaps directly ... (-> (assoc-interior m v k1 k2) k1 k2) => v ... not tail-recursive. Do not depend on its working on vectors. "
  [m v & ks]
  ((fn associntr [v m ks]
     (if (seq (rest ks))
       (let [res (if (and (contains? m (first ks))
                          (map? (m (first ks))))
                   (associntr v (m (first ks)) (rest ks))
                   (associntr v {} (rest ks)))]
         (assoc m (first ks) res))
       (assoc m (first ks) v)))
  v m ks))

(defn nil-if-blank [str]
  (if-not (string/blank? str)
    str))

