(ns crust.compiler-test
  (:require [crust.compiler :as sut]
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
         {:ns {:name 'clrs.test}
          :context :ctx/expr}))

(deftest emit-var-test
  (testing "vars"
    (is (= "x"
           (sut/emits (sut/analyze test-expr-env 'x))))
    (is (= "y;\n"
           (sut/emits (sut/analyze test-stmnt-env 'y))))
    (is (= "z\n"
           (sut/emits (sut/analyze test-ret-env 'z))))

    (is (= "clrs::user::foo"
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
    (is (= "foo(x)"
           (emits-expr '(foo x))))
    (is (= "bar(y)"
           (emits-expr '(bar y))))))

(deftest emit-if-test
  (testing "if"
    (is (= "if true {\n\t1} else {\n\t2}\n"
           (emits-expr '(if true 1 2))))
    (is (= "if false {\n\tx} else {\n\ty}\n"
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
    (is (= "{\n\tfoo(10);\n\t1\n}\n"
           (emits-expr '(do (foo 10) 1))))
    (is (= "{\n\tfoo(10);\n\t1\n}\n"
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

(deftest emit-let-test
  (testing "let"
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\n\1\n\}"
                 (emits-expr '(let* [x 1] x))))
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\tlet (y__\d+) = 20;\n\n\1\n\}"
                 (emits-expr '(let* [x 1 y 20] x))))
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\tlet (y__\d+) = 20;\n\n\{\n\tfoo\(\1,\2\);\n\t\1\n\}\n\}"
                 (emits-expr '(let* [x 1 y 20]
                                (foo x y)
                                x))))

    (testing "prints types when they're hinted"
      (is (matches #"\{\n\tlet (x__\d+): u8 = 1;\n\n\1\n\}"
                   (emits-expr '(let* [^u8 x 1] x)))))))

(deftest emit-loop-test
  (testing "loop"
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\nloop \{\n\1\nbreak;\n\}\n\}"
                 (emits-expr '(loop* [x 1] x))))))

(deftest emit-recur-test
  (testing "fn-recur"
    (is (matches #"\|x\| \{\n\tloop \{\n\{\n\tlet ([\w\d_]+) = 1;\nx = \1;\ncontinue;\n\}\nbreak;\n\}\n\}\n"
                 (emits-expr '(fn* [x] (recur 1))))))

  (testing "loop-recur"
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\nloop \{\n\1\nbreak;\n\}\n\}"
                 (emits-expr '(loop* [x 1] x))))))

(deftest emit-new-test
  (testing "new"
    (is (= "Foo {}"
           (emits-expr '(new Foo))))

    (is (= "Foo {x: 1}"
           (emits-expr '(new Foo x 1))))))

(deftest emit-set!-test
  (testing "set!"
    (is (matches #"\{\n\tlet mut (x__\d+) = 10;\n\n\1 = 20\n\}"
                 (emits-expr '(let* [^:mut x 10]
                                (set! x 20)))))))

(deftest emit-ns-test
  (testing "ns"
    (is (= ""
           (emits-expr '(ns foo.bar))))

    (is (= "use baz::bar;\n"
           (emits-expr '(ns foo.bar
                          (:require [baz.bar :as baz.bar])))))))

(deftest emit-deftype*-test
  (testing "deftype*"
    (is (= "struct Foo {\n}\n"
           (emits-expr '(deftype* Foo []))))

    (is (= "struct Foo {\n\ta: u8,\n}\n"
           (emits-expr '(deftype* Foo [^u8 a]))))

    (is (= "struct Foo {\n\ta: u8,\n\tb: i32,\n}\n"
           (emits-expr '(deftype* Foo [^u8 a ^i32 b]))))))

(deftest macro-test
  (testing "fn"
    (is (= "|x| {\n\tx\n}\n"
           (emits-expr '(fn [x] x)))))

  (testing "cond"
    (is (= "if true {\n\t1} else {\n\tif false {\n\t2} else {\n\t()}\n}\n"
           (emits-expr '(cond true 1
                              false 2))))))
