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
     (when (= :ctx/return (:context env#)) (print "return "))
     ~@body
     (when-not (= :ctx/expr (:context env#)) (print ";\n"))))

(defmethod emit :var
  [{:keys [info env] :as arg}]
  (emit-wrap env (print (:name info))))

(defmethod emit :constant
  [{:keys [form env]}]
  (emit-wrap env (emit-constant form)))

(defmethod emit :invoke
  [{:keys [f args env]}]
  (emit-wrap env
             (print (str (emits f) "("
                         (apply str (interpose "," (map emits args)))
                         ")"))))


;; Parsing

(def specials
  '#{})

(def ^:dynamic *recur-frame* nil)

(defmacro disallowing-recur [& body]
  `(binding [*recur-frame* nil] ~@body))

(declare analyze)

(defmulti parse (fn [op & rest] op))

(defn analyze-invoke
  [env [f & args]]
  (disallowing-recur
   (let [enve (assoc env :context :ctx/expr)
         fexpr (analyze enve f)
         argexprs (mapv #(analyze enve %) args)]
     {:env env :op :invoke :f fexpr :args argexprs :children [:args :f]})))

(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (let [ret {:env env :form sym}
        lb (-> env :locals sym)]
    (if lb
      (assoc ret :op :var :info lb)
      (assoc ret :op :var :info (resolve-var env sym)))))

(defn get-expander [sym env]
  (when-not (-> env :locals sym)
    ))

(defn analyze-seq
  [env form name]
  (let [op (first form)]
    (assert (not (nil? op)) "Can't call nil")
    (if (specials op)
      (parse op env form name)
      (if-let [mac (and (symbol? op) (get-expander op env))]
        (analyze (apply mac (rest form)))
        (analyze-invoke env form)))))

(def empty-env
  {:ns 'clrs.user
   :context :ctx/return
   :locals {}})

(defn analyze
  "Given an environment, a map containing {:locals (mapping of names to bindings), :context
  (one of :statement, :expr, :return), :ns (a symbol naming the
  compilation ns)}, and form, returns an expression object (a map
  containing at least :form, :op and :env keys). If expr has any (immediately)
  nested exprs, must have :children [exprs...] entry. This will
  facilitate code walking without knowing the details of the op set."
  ([env form] (analyze env form nil))
  ([env form name]
   (let [form (if (instance? clojure.lang.LazySeq form)
                (or (seq form) ())
                form)]
     (cond
       (symbol? form) (analyze-symbol env form)
       (and (seq? form) (seq form)) (analyze-seq env form name)
       :else {:op :constant :env env :form form}))))
