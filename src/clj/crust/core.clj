(ns crust.core
  (:refer-clojure :exclude [-> ->> .. amap and areduce assert binding bound-fn case comment cond condp
                            declare definline definterface defmethod defmulti defn defn- defonce
                            defprotocol defrecord defstruct deftype delay doseq dosync dotimes doto
                            extend-protocol extend-type fn for future gen-class gen-interface
                            if-let if-not import io! lazy-cat lazy-seq let letfn locking loop
                            memfn ns or proxy proxy-super pvalues refer-clojure reify sync time
                            when when-first when-let when-not while with-bindings with-in-str
                            with-loading-context with-local-vars with-open with-out-str with-precision with-redefs]))

(alias 'core 'clojure.core)

(defmacro import-macros [ns [& vars]]
  (core/let [ns (find-ns ns)
             vars (map #(ns-resolve ns %) vars)
             syms (map #(core/-> % .sym (with-meta {:macro true})) vars)
             defs (map (core/fn [sym var]
                         `(def ~sym (deref ~var))) syms vars)]
    `(do ~@defs)
    :imported))

(import-macros clojure.core
               [-> ->> ..  and assert comment cond condp
                declare defmacro defn defn-
                doto
                extend-protocol extend-type fn for
                if-let if-not let letfn loop
                or
                when when-first when-let when-not while])
