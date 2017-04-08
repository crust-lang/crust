(ns crust.compiler
  (:require [clojure.string :as str]))

(defonce namespaces (atom {}))

(defmulti emit-constant class)
(defmethod emit-constant nil [x] "()")
(defmethod emit-constant Long [x] (str x))
(defmethod emit-constant Double [x] (str x))
(defmethod emit-constant String [x] (pr-str x))
(defmethod emit-constant Boolean [x] (if x "true" "false"))

(defmulti emit :op)

(defmethod emit :do [ast]
  (str "{ " (str/join "; " (map emit (conj (:statements ast) (:ret ast)))) " }"))

(defmethod emit :const
  [{:keys [form]}]
  (emit-constant form))

(defmethod emit :if [ast]
  (str "if " (emit (:test ast)) " { "
       (emit (:then ast))
       " } else { "
       (emit (:else ast))
       " }"))
