(ns mpro.mysql
  (:require [clojure.java.jdbc :as j]
         [clojure.java.jdbc.sql :as s]))

(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost:3306/mprodb"
         :user "mproj"
         :password "mproj"})

(defn grabtesting1 []
  ())


