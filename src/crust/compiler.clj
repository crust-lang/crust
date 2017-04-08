(ns crust.compiler
  (:require [clojure.string :as str]))

(defmulti emit :op)
(defmethod emit :do [ast]
  (str "{ " (str/join "; " (map emit (conj (:statements ast) (:ret ast)))) " }"))

(defmethod emit :const [ast]
  (if (= (:type ast) :nil)
    "()"
    (pr-str (:val ast))))

(defmethod emit :if [ast]
  (str "if " (emit (:test ast)) " { "
       (emit (:then ast))
       " } else { "
       (emit (:else ast))
       " }"))
