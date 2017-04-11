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

(defmethod emit :if
  [{:keys [test then else env]}]
  (let [context (:context env)]
    (print (str "if " (emits test) " {\n\t"
                      (emits then)
                "} else {\n\t"
                      (emits else)
                "}\n"))))

(defmethod emit :def
  [{:keys [name init env type]}]
  (when init
    (print (str "static " name ": " type " = " (emits init)))
    (when-not (= :expr (:context env)) (print ";\n"))))

(defmethod emit :fn
  [{:keys [name params statements ret env recurs]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :statement (:context env))
    (emit-wrap env
               (print (str "|" (apply str (interpose "," params)) "| {\n\t"))
               (when recurs (print "loop {\n"))
               (emit-block :return statements ret)
               (when recurs (print "break;\n}\n"))
               (print "}\n"))))


;; Parsing

(def specials
  '#{if def fn*})

(def ^:dynamic *recur-frame* nil)

(defmacro disallowing-recur [& body]
  `(binding [*recur-frame* nil] ~@body))

(declare analyze)

(defmulti parse (fn [op & rest] op))

(defmethod parse 'if
  [op env [_ test then else :as form] name]
  (let [test-expr (disallowing-recur (analyze (assoc env :context :ctx/expr) test))
        then-expr (analyze env then)
        else-expr (analyze env else)]
    {:env env :op :if :form form
     :test test-expr :then then-expr :else else-expr
     :children [:test :then :else]}))

(defmethod parse 'def
  [op env form _name]
  (let [pfn (fn
              ([_ sym] {:sym sym})
              ([_ sym init] {:sym sym :init init})
              ([_ sym doc init] {:sym sym :doc doc :init init}))
        args (apply pfn form)
        sym (:sym args)]
    (assert (not (namespace sym)) "Can't def ns-qualified name")
    (assert (contains? (meta sym) :tag) "Must specify type for a static def")
    (let [name (name sym)
          init-expr (when (contains? args :init)
                      (disallowing-recur
                       (analyze (assoc env :context :ctx/expr) (:init args) sym)))]
      (merge {:env env
              :op :def
              :type (:tag (meta sym))
              :form form
              :name name
              :doc (:doc args)
              :init init-expr}
             (when init-expr {:children [:init]})))))

(defn analyze-block
  "returns {:statements .. :ret .. :children ..}"
  [env exprs]
  (let [statements (disallowing-recur
                    (seq (map #(analyze (assoc env :context :ctx/statement) %) (butlast exprs))))
        ret (if (<= (count exprs) 1)
              (analyze env (first exprs))
              (analyze (assoc env :context (if (= :ctx/statement (:context env))
                                             :ctx/statement
                                             :ctx/return))
                       (last exprs)))]
    {:statements statements :ret ret :children [:statements :ret]}))

(defmethod parse 'fn*
  [op env [_ & args] name]
  (let [name (if (symbol? (first args))
               (first args)
               name)
        meths (if (symbol? (first args))
                (next args)
                args)
        ;;turn (fn [] ...) into (fn ([]...))
        meths (if (vector? (first meths)) (list meths) meths)
        ;;todo, merge meths, switch on arguments.length
        meth (first meths)
        params (first meth)
        ;;todo, variadics
        params (remove '#{&} params)
        body (next meth)
        locals (reduce (fn [m name] (assoc m name {:name name})) (:locals env) params)
        recur-frame {:names (vec params) :flag (atom nil)}
        block (binding [*recur-frame* recur-frame]
                (analyze-block (assoc env :context :ctx/return :locals locals) body))]
    (assert (= 1 (count meths)) "Arity overloading not yet supported")
    (merge {:env env :op :fn :name name :meths meths :params params :recurs @(:flag recur-frame)} block)))


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
