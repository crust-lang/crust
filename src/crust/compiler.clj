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

(defmethod emit :const
  [{:keys [form]}]
  (emit-constant form))

(defmethod emit :if [ast]
  (str "if " (emit (:test ast)) " { "
       (emit (:then ast))
       " } else { "
       (emit (:else ast))
       " }"))

(defn emit-body [statements ret]
  (str/join "; " (->> ret
                      (conj statements)
                      (keep identity)
                      (map emit))))

(defn rustify [s]
  (-> s
      name
      (str/replace #"[-.+?!#$%&*]" "")))

(defmethod emit :binding
  [{:keys [name]}]
  (rustify name))

(defmethod emit :local
  [{:keys [name]}]
  (rustify name))

(defmethod emit :fn-method
  [{:keys [params body]}]
  (str
   "|" (str/join "," (map emit params)) "| { "
   (emit body)
   " }"))

(defmethod emit :fn
  [{:keys [methods]}]
  (emit (first methods)))

(defmethod emit :do
  [{:keys [statements ret]}]
  (str "{ " (emit-body statements ret) " }"))
