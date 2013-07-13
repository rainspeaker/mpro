(ns mpro.dbconfig
  (:require [korma.db :as kdb]
            [clojure.java.jdbc :as j]
            [clojure.java.jdbc.sql :as s])
  (:use [korma.core]))

(def dbmap {:classname "com.mysql.jdbc.Driver"
            :subprotocol "mysql"
            :subname "//localhost:8889/mprodb"
            :user "mpro"
            :password "mpro"})

(kdb/defdb db (kdb/mysql dbmap))







