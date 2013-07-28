(ns mpro.app_servlet
  (:gen-class :extends javax.servlet.http.HttpServlet)
  (:use mpro.server)
  (:use [appengine-magic.servlet :only [make-servlet-service-method]]))

(defn -service [this request response]
  ((make-servlet-service-method mpro-app) this request response))

