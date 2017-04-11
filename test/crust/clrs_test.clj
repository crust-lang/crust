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

(deftest emit-var-test
  (testing "vars"
    (is (= "clrs.test.x"
           (sut/emits (sut/analyze test-expr-env 'x))))
    (is (= "clrs.test.y;\n"
           (sut/emits (sut/analyze test-stmnt-env 'y))))
    (is (= "return clrs.test.z;\n"
           (sut/emits (sut/analyze test-ret-env 'z))))

    (is (= "clrs.user.foo"
           (sut/emits (sut/analyze test-expr-env 'clrs.user/foo))))))

(deftest emit-constant-test
  (testing "nil"
    (is (= "()"
           (sut/emits (sut/analyze test-expr-env nil)))))

  (testing "integers"
    (is (= "1"
           (sut/emits (sut/analyze test-expr-env 1))))
    (is (= "13"
           (sut/emits (sut/analyze test-expr-env 13)))))

  (testing "floating-point"
    (is (= "1.0"
           (sut/emits (sut/analyze test-expr-env 1.0))))
    (is (= "13.5"
           (sut/emits (sut/analyze test-expr-env 13.5)))))

  (testing "strings"
    (is (= "\"foo\""
           (sut/emits (sut/analyze test-expr-env "foo"))))
    (is (= "\"bar\""
           (sut/emits (sut/analyze test-expr-env "bar")))))

  (testing "booleans"
    (is (= "true"
           (sut/emits (sut/analyze test-expr-env true))))
    (is (= "false"
           (sut/emits (sut/analyze test-expr-env false))))))

(deftest emit-invoke-test
  (testing "invocations"
    (is (= "clrs.test.foo(clrs.test.x)"
           (sut/emits (sut/analyze test-expr-env '(foo x)))))
    (is (= "clrs.test.bar(clrs.test.y)"
           (sut/emits (sut/analyze test-expr-env '(bar y)))))))

(deftest emit-if-test
  (testing "if"
    (is (= "if true {\n\t1} else {\n\t2}\n"
           (sut/emits (sut/analyze test-expr-env '(if true 1 2)))))
    (is (= "if false {\n\tclrs.test.x} else {\n\tclrs.test.y}\n"
           (sut/emits (sut/analyze test-expr-env '(if false x y)))))))

(deftest emit-def-test
  (testing "def"
    (is (= "static x: u8 = 10;\n"
           (sut/emits (sut/analyze test-expr-env '(def ^u8 x 10)))))
    (is (= "static baz: i32 = -42;\n"
           (sut/emits (sut/analyze test-expr-env '(def ^i32 baz -42)))))))
