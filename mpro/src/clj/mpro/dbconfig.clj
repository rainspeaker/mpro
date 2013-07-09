(ns mpro.dbconfig
  (:require [clojure.java.jdbc :as j]
         [clojure.java.jdbc.sql :as s]))

(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost:8889/mprodb"
         :user "mpro"
         :password "mpro"})



