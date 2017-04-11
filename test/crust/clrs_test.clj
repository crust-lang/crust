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
    (is (= "clrs.test.z\n"
           (sut/emits (sut/analyze test-ret-env 'z))))

    (is (= "clrs.user.foo"
           (sut/emits (sut/analyze test-expr-env 'clrs.user/foo))))))

(defmacro emits-expr [form]
  `(sut/emits (sut/analyze test-expr-env ~form)))

(deftest emit-constant-test
  (testing "nil"
    (is (= "()"
           (emits-expr nil))))

  (testing "integers"
    (is (= "1"
           (emits-expr 1)))
    (is (= "13"
           (emits-expr 13))))

  (testing "floating-point"
    (is (= "1.0"
           (emits-expr 1.0)))
    (is (= "13.5"
           (emits-expr 13.5))))

  (testing "strings"
    (is (= "\"foo\""
           (emits-expr "foo")))
    (is (= "\"bar\""
           (emits-expr "bar"))))

  (testing "booleans"
    (is (= "true"
           (emits-expr true)))
    (is (= "false"
           (emits-expr false)))))

(deftest emit-invoke-test
  (testing "invocations"
    (is (= "clrs.test.foo(clrs.test.x)"
           (emits-expr '(foo x))))
    (is (= "clrs.test.bar(clrs.test.y)"
           (emits-expr '(bar y))))))

(deftest emit-if-test
  (testing "if"
    (is (= "if true {\n\t1} else {\n\t2}\n"
           (emits-expr '(if true 1 2))))
    (is (= "if false {\n\tclrs.test.x} else {\n\tclrs.test.y}\n"
           (emits-expr '(if false x y))))))

(deftest emit-def-test
  (testing "def"
    (is (= "static x: u8 = 10;\n"
           (emits-expr '(def ^u8 x 10))))
    (is (= "static baz: i32 = -42;\n"
           (emits-expr '(def ^i32 baz -42))))))

(deftest emit-fn-test
  (testing "fn"
    (is (= "|| {\n\t1\n}\n"
           (emits-expr '(fn* [] 1))))
    (is (= "|pay_attention| {\n\tpay_attention\n}\n"
           (emits-expr '(fn* [pay_attention] pay_attention)))))

  (testing "fn statements are elided"
    (is (= "{\n\t\t1\n}\n"
           (emits-expr '(do
                          (fn* [x] x)
                          1))))))

(deftest emit-do-test
  (testing "do"
    (is (= "{\n\tclrs.test.foo(10);\n\t1\n}\n"
           (emits-expr '(do (foo 10) 1))))
    (is (= "{\n\tclrs.test.foo(10);\n\t1\n}\n"
           (emits-expr '(do (foo 10) 1))))

    (testing "do statements disappear with only one body element"
      (is (= "1"
             (emits-expr '(do 1)))))))

(defmethod clojure.test/assert-expr 'matches [msg re-and-form]
  (let [re   (nth re-and-form 1)
        form (nth re-and-form 2)]
    `(let [re# ~re
           result# ~form
           matches?# (re-matches re# result#)]
       (do-report (merge {:message ~msg
                          :expected (list '~'to-match re# '~form)
                          :actual (list '~'does-not-match re# result#)}
                         (if matches?#
                           {:type :pass}
                           {:type :fail})))
       matches?#)))
