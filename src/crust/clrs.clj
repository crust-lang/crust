(ns crust.clrs
  (:require [clojure.string :as str]))

(defonce namespaces (atom {}))

(defn- resolve-var [env sym]
  (let [s (str sym)
        lb (-> env :locals sym)
        nm
        (cond
          lb (:name lb)

          (= "rs" (namespace sym)) (subs s 3)

          ;;todo - resolve ns aliases when we have them
          (namespace sym)
          (symbol (str (namespace sym) "." (name sym)))

          (str/includes? s ".")
          (let [idx (str/index-of s ".")
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
     ~@body
     (when (= :ctx/statement (:context env#)) (print ";"))
     (when-not (= :ctx/expr (:context env#)) (print "\n"))))

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

(defn- format-param [{:keys [name type]}]
  (str name (when type (str ": " type))))

(defmethod emit :fn
  [{:keys [name params body env recurs]}]
  ;;fn statements get erased, serve no purpose and can pollute scope if named
  (when-not (= :ctx/statement (:context env))
    (emit-wrap env
               (print (str "|" (apply str (interpose "," (map format-param params))) "| {\n\t"))
               (when recurs (print "loop {\n"))
               (emit body)
               (when recurs (print "break;\n}\n"))
               (print "}\n"))))

(defmethod emit :do
  [{:keys [statements ret env]}]
  (if statements
    (do
      (print "{\n")
      (let [body (str "\t" (apply str (interpose "\t" (map emits statements)))
                      "\t" (emits ret))]
        (print body))
      (print "}\n"))
    (emit ret)))

(defmethod emit :let
  [{:keys [bindings body env loop]}]
  (let [context (:context env)
        bs (map (fn [{:keys [name init type mutable?]}]
                  (str "\tlet "
                       (when mutable? "mut ")
                       name
                       (when type (str ": " type))
                       " = " (emits init) ";\n"))
                bindings)]
    (print (str "{\n" (apply str bs) "\n"))
    (when loop (print "loop {\n"))
    (emit body)
    (when loop (print "break;\n}\n"))
    (print "}")))

(defmethod emit :recur
  [{:keys [frame exprs env]}]
  (let [temps (vec (take (count exprs) (repeatedly gensym)))
        names (:names frame)]
    (print "{\n")
    (dotimes [i (count exprs)]
      (print (str "\tlet " (temps i) " = " (emits (exprs i)) ";\n")))
    (dotimes [i (count exprs)]
      (print (str (:name (names i)) " = " (temps i) ";\n")))
    (print "continue;\n")
    (print "}\n")))

(defmethod emit :new
  [{:keys [ctor args env]}]
  (emit-wrap env
             (print (str (emits ctor) " {"
                         (apply str
                                (interpose ","
                                           (map #(str (first %) ": " (emits (second %)))
                                                args)))
                         "}"))))

(defmethod emit :set!
  [{:keys [target val env]}]
  (emit-wrap env (print (str (emits target) " = "(emits val)))))

(defmethod emit :ns
  [{:keys [name requires macros env]}]
  (print "mod" name "{\n\t")
  (doseq [lib (map #(str/replace % #"\." "::")
                   (vals requires))]
    (println (str "use " lib ";")))
  (println "\n}"))

(defmethod emit :defn*
  [{:keys [env name body params recurs]}]
  (emit-wrap env
             (print (str "fn " name "("
                         (apply str (interpose "," (map format-param params)))
                         ") {\n\t"))
             (when recurs (print "loop {\n"))
             (emit body)
             (when recurs (print "break;\n}\n"))
             (print "}\n")))

(defmethod emit :defstruct*
  [{:keys [env name fields]}]
  (emit-wrap env
    (println "pub struct" name "{")
    (print "\t")
    (doseq [{:keys [private label type]} fields]
      (println (str (when private "priv ") label ": " type ",")))
    (println "}")))

(defmethod emit :defenum*
  [{:keys [env name variants]}]
  (emit-wrap env
             (println "pub enum" name "{")
             (doseq [{:keys [value]} variants]
               (println (str "\t" value ",")))
             (println "}")))

;; Parsing

(def specials
  '#{if def fn* do let*
     loop recur new set!
     ns defn* defstruct*
     defenum*})

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

(defn analyze-fn [op env meths name]
  (let [;;turn (fn [] ...) into (fn ([]...))
        meths (if (vector? (first meths)) (list meths) meths)
        ;;todo, merge meths, switch on arguments.length
        meth (first meths)
        params (first meth)
        ;;todo, variadics
        params (map (fn [sym] {:name sym :type (:tag (meta sym))}) (remove '#{&} params))
        body (next meth)
        append-arg (fn [m param] (assoc m (:name param) param))
        locals (reduce append-arg (:locals env) params)
        recur-frame {:names (vec params) :flag (atom nil)}
        body (binding [*recur-frame* recur-frame]
               (analyze (assoc env :context :ctx/return :locals locals)
                        (apply list 'do body)))]
    (assert (= 1 (count meths)) "Arity overloading not yet supported")
    {:env env
     :op op
     :name name
     :meths meths
     :params params
     :body body
     :children [:body]
     :recurs @(:flag recur-frame)}))

(defmethod parse 'fn*
  [op env [_ & args] name]
  (let [name (if (symbol? (first args))
               (first args)
               name)
        meths (if (symbol? (first args))
                (next args)
                args)]
    (analyze-fn :fn env meths name)))

(defmethod parse 'defn*
  [op env [_ name & args] name*]
  (analyze-fn :defn* env args name))

(defmethod parse 'do
  [op env [_ & exprs] _]
  (merge {:env env :op :do}
         (analyze-block env exprs)))

(defn analyze-let
  [encl-env [_ bindings & exprs :as form] is-loop]
  (assert (and (vector? bindings) (even? (count bindings)))
          "bindings must be vector of even number of elements")
  (let [context (:context encl-env)
        [bes env]
        (disallowing-recur
         (loop [bes []
                env (assoc encl-env :context :ctx/expr)
                bindings (seq (partition 2 bindings))]
           (if-let [[name init] (first bindings)]
             (do
               (assert (not (or (namespace name) (str/includes? (str name) ".")))
                       (str "Invalid local name: " name))
               (let [init-expr (analyze env init)
                     m (meta name)
                     be {:name (gensym (str name "__"))
                         :init init-expr
                         :type (:tag m)
                         :mutable? (:mut m)}]
                 (recur (conj bes be)
                        (assoc-in env [:locals name] be)
                        (next bindings))))
             [bes env])))
        recur-frame (when is-loop {:names (vec (map :name bes)) :flag (atom nil)})
        body
        (binding [*recur-frame* (or recur-frame *recur-frame*)]
          (analyze (assoc env :context (if (= :ctx/expr context)
                                         :ctx/return
                                         context))
                   ;; Wrap the body exprs in a synthetic do-block
                   (apply list 'do exprs)))]
    {:env encl-env
     :op :let
     :loop is-loop
     :bindings bes
     :body body
     :form form
     :children [:bindings :body]}))

(defmethod parse 'let*
  [op encl-env form _]
  (analyze-let encl-env form false))

(defmethod parse 'loop
  [op encl-env form _]
  (analyze-let encl-env form true))

(defmethod parse 'recur
  [op env [_ & exprs] _]
  (assert *recur-frame* "Can't recur here")
  (assert (= (count exprs) (count (:names *recur-frame*))) "recur argument count mismatch")
  (reset! (:flag *recur-frame*) true)
  (assoc {:env env :op :recur}
         :frame *recur-frame*
         :exprs (disallowing-recur
                 (vec
                  (map #(analyze (assoc env :context :ctx/expr) %)
                       exprs)))))

(defn analyze-symbol
  "Finds the var associated with sym"
  [env sym]
  (let [ret {:env env :form sym}
        lb (-> env :locals sym)]
    (if lb
      (assoc ret :op :var :info lb)
      (assoc ret :op :var :info (resolve-var env sym)))))

(defmethod parse 'new
  [_ env [_ ctor & {:as args}] _]
  (disallowing-recur
   (let [enve (assoc env :context :ctx/expr)
         ctorexpr (analyze enve ctor)
         argexprs (zipmap
                   (keys args)
                   (map #(analyze enve %) (vals args)))]
     {:env env :op :new :ctor ctorexpr :args argexprs :children [:args :ctor]})))

(defmethod parse 'set!
  [_ env [_ target val] _]
  (assert (symbol? target) "set! target must be a symbol naming var")
  (let [lb (-> env :locals target)]
    (assert (:mutable? lb) "Can't set! a non-mutable binding"))
  (disallowing-recur
   (let [enve (assoc env :context :ctx/expr)
         targetexpr (analyze-symbol enve target)
         valexpr (analyze enve val)]
     {:env env
      :op :set!
      :target targetexpr
      :val valexpr
      :children [:target :val]})))

(defmethod parse 'ns
  [_ env [_ ns-name & {:keys [requires macros] :as params}] _]
  (doseq [nsym (vals macros)]
    (require nsym))
  (let [deps (into requires
                   (map (fn [[alias nsym]]
                          [alias (find-ns nsym)])
                        macros))]
    (swap! namespaces #(-> %
                           (assoc-in [ns-name :name] ns-name)
                           (assoc-in [ns-name :deps] deps))))
  (merge {:env env :op :ns :name ns-name} params))

(defn- analyze-struct-field [[label type]]
  (let [private (:private (meta label))]
    {:label label
     :type type
     :private private}))

(defmethod parse 'defstruct*
  [_ env [_ name fields] _]
  (let [fields (map analyze-struct-field (partition 2 fields))]
    {:env env
     :op :defstruct*
     :name name
     :fields fields}))

(defmethod parse 'defenum*
  [_ env [_ name & variants] _]
  (let [variants (map (fn [variant] {:value variant}) variants)]
    {:env env
     :op :defenum*
     :name name
     :variants variants}))

(defn analyze-invoke
  [env [f & args]]
  (disallowing-recur
   (let [enve (assoc env :context :ctx/expr)
         fexpr (analyze enve f)
         argexprs (mapv #(analyze enve %) args)]
     {:env env :op :invoke :f fexpr :args argexprs :children [:args :f]})))

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
