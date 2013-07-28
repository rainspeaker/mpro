(ns clojurescript-hello
  (:require [jayq.core :as q])
  (:require-macros [jayq.macros :as qm])
  (:use [jayq.core :only [$]]))

;; Equivalent to 
;;
;;   $("#btn").click(function() { alert("Hi!"); });
;; 



;; (qm/ready
;;  (q/bind ($ ".cprofile-form") :submit
;;          (fn [e]
;;            (this-as this
;;                     (do (js/alert (q/attr ($ this) :action))
;;                         (.preventDefault e))))))
  
