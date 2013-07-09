(ns clojurescript-hello
  (:require [jayq.core :as q])
  (:require-macros [jayq.macros :as qm])
  (:use [jayq.core :only [$]]))

;; Equivalent to 
;;
;;   $("#btn").click(function() { alert("Hi!"); });
;; 

(qm/ready (q/bind ($ "#clickable") :click (fn [] (js/alert "Hi!"))))

