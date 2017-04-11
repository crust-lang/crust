(ns crust.clrs-test
  (:require [crust.clrs :as sut]
            [clojure.test :refer :all]))

(def test-env
  {:ns 'clrs.test
   :locals {}})

(def test-expr-env
  (assoc test-env :context :ctx/expr))

(def test-stmnt-env
  (assoc test-env :context :ctx/statement))

(def test-ret-env
  (assoc test-env :context :ctx/return))

(def test-expr-env
  (merge sut/empty-env
         {:ns 'clrs.test
          :context :ctx/expr}))
