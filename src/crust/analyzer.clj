(ns crust.analyzer
  (:refer-clojure :exclude [macroexpand-1 macroexpand])
  (:require [clojure.tools.analyzer :as ana]

            [clojure.tools.analyzer
             [env :as env]
             [passes :refer [schedule]]
             [utils :as utils]]

            [clojure.tools.analyzer.passes
             [source-info :refer [source-info]]
             [trim :refer [trim]]
             [elide-meta :refer [elide-meta elides]]
             [warn-earmuff :refer [warn-earmuff]]
             [uniquify :refer [uniquify-locals]]]))

(def specials
  "Set of the special forms for clojure in Rust."
  (into ana/specials
        '#{clojure.core/ns clojure.core/import* deftype* mod*}))

(defn global-env []
  (atom {:namespaces (atom {})}))

(defn empty-env
  "Returns an empty env map"
  []
  {:context    :ctx/expr
   :locals     {}
   :ns         (ns-name *ns*)})

(defmulti parse
  "Extension to tools.analyzer/-parse for Rust special forms"
  (fn [[op & rest] env] op))

(defmethod parse :default
  [form env]
  (ana/-parse form env))

(defmethod parse 'ns
  [form env]
  {:op   :ns
   :env  env
   :form form})

(defmethod parse 'clojure.core/import*
  [[_ class :as form] env]
  {:op    :import
   :env   env
   :form  form
   :class class})

(defmethod parse 'deftype*
  [form env]
  {:op :deftype
   :env env
   :form form})

(defn macroexpand-1
  "If form represents a macro form or an inlineable function,returns its expansion,
   else returns form."
  ([form] (macroexpand-1 form (empty-env)))
  ([form env]))

(defn create-var [sym env])

(defn var? [obj])

(def default-passes
  "Set of passes that will be run by default on the AST by #'run-passes"
  #{#'warn-earmuff

    #'uniquify-locals

    #'source-info
    #'elide-meta

    #'trim

    ;; #'analyze-host-expr
    })

(def scheduled-default-passes
  (schedule default-passes))

(defn ^:dynamic run-passes
  "Function that will be invoked on the AST tree immediately after it has been constructed,
   by default runs the passes declared in #'default-passes, should be rebound if a different
   set of passes is required.

   Use #'clojure.tools.analyzer.passes/schedule to get a function from a set of passes that
   run-passes can be bound to."
  [ast]
  (scheduled-default-passes ast))

(def default-passes-opts
  "Default :passes-opts for `analyze`"
  {:collect/what                    #{:constants :callsites}
   :collect/where                   #{:deftype :fn}
   :collect/top-level?              false
   :collect-closed-overs/where      #{:deftype :fn :loop}
   :collect-closed-overs/top-level? false})

(defn analyze
  "Analyzes a clojure form using tools.analyzer augmented with the Rust specific special ops
   and returns its AST, after running #'run-passes on it.

   If no configuration option is provides, analyze will setup tools.analyzer using the extension
   points declared in this namespace.

   If provided, opts should be a map of options to analyze, currently the only valid
   options are :bindings and :passes-opts (if not provided, :passes-opts defaults to the
   value of `default-passes-opts`).
   If provided, :bindings should be a map of Var->value pairs that will be merged into the
   default bindings for tools.analyzer, useful to provide custom extension points.
   If provided, :passes-opts should be a map of pass-name-kw->pass-config-map pairs that
   can be used to configure the behaviour of each pass.

   E.g.
   (analyze form env {:bindings  {#'ana/macroexpand-1 my-mexpand-1}})"
  ([form] (analyze form (empty-env) {}))
  ([form env] (analyze form env {}))
  ([form env opts]
     (with-bindings (merge {#'ana/macroexpand-1 macroexpand-1
                            #'ana/create-var    create-var
                            #'ana/parse         parse
                            #'ana/var?          var?
                            #'elides            (merge {:fn    #{:line :column :end-line :end-column :file :source}
                                                        :reify #{:line :column :end-line :end-column :file :source}}
                                                       elides)
                            ;; TODO: this used to return a "real"
                            ;; clojure ns, unclear what this should be
                            ;; now
                            #'*ns*              (:ns env)}
                           (:bindings opts))
       (env/ensure (global-env)
         (env/with-env (utils/mmerge (env/deref-env)
                                     {:passes-opts (get opts :passes-opts default-passes-opts)})
           (run-passes (ana/analyze form env)))))))
