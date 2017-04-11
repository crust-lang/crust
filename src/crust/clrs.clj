(ns crust.clrs)

(defonce namespaces (atom {}))

(defn- resolve-var [env sym]
  (let [s (str sym)
        lb (-> env :locals sym)
        nm
        (cond
          lb (:name lb)

          ;;todo - resolve ns aliases when we have them
          (namespace sym)
          (symbol (str (namespace sym) "." (name sym)))

          (.contains s ".")
          (let [idx (.indexOf s ".")
                prefix (symbol (subs s 0 idx))
                suffix (subs s idx)
                lb (-> env :locals prefix)]
            (if lb
              (symbol (str (:name lb) suffix))
              sym))

          :else
          (symbol (str (:ns env) "." (name sym))))]
    {:name nm}))

(defmulti emit-constant class)

(defmethod emit-constant nil [x] (print "()"))
(defmethod emit-constant Long [x] (print x))
(defmethod emit-constant Double [x] (print x))
(defmethod emit-constant String [x] (pr x))
(defmethod emit-constant Boolean [x] (print (if x "true" "false")))

(defmulti emit :op)

(defn emits [expr]
  (with-out-str (emit expr)))

(defn emit-block
  [context statements ret]
  (if statements
    (let [body (str "\t" (apply str (interpose "\t" (map emits statements)))
                    "\t" (emits ret))]
      (print body))
    (emit ret)))

(defmacro emit-wrap [env & body]
  `(let [env# ~env]
     (when (= :return (:context env#)) (print "return "))
     ~@body
     (when-not (= :expr (:context env#)) (print ";\n"))))
