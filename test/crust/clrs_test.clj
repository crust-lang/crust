(ns crust.clrs-test
  (:require [crust.clrs :as sut]
            [clojure.java.io :as io]
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

(defmacro emits-expr [form]
  `(sut/emits (sut/analyze test-expr-env ~form)))

(deftest emit-var-test
  (testing "vars"
    (is (= "clrs.test.x"
           (sut/emits (sut/analyze test-expr-env 'x))))
    (is (= "clrs.test.y;\n"
           (sut/emits (sut/analyze test-stmnt-env 'y))))
    (is (= "clrs.test.z\n"
           (sut/emits (sut/analyze test-ret-env 'z))))

    (is (= "clrs.user.foo"
           (sut/emits (sut/analyze test-expr-env 'clrs.user/foo)))))

  (testing "rust native ns vars"
    (is (= "foo"
           (emits-expr 'rs/foo)))))

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
           (emits-expr '(if false x y))))
    (is (= "if true {\n\t1} else {\n\t()}\n"
           (emits-expr '(if true 1))))))

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
           (emits-expr '(fn* [pay_attention] pay_attention))))
    (is (= "|pay_attention: u8| {\n\tpay_attention\n}\n"
           (emits-expr '(fn* [^u8 pay_attention] pay_attention)))))

  (testing "fn statements are elided"
    (is (= "{\n\t\t1\n}\n"
           (emits-expr '(do
                          (fn* [x] x)
                          1))))))

(deftest emit-defn*-test
  (testing "defn*"
    (is (= "fn main() {\n\t()\n}\n"
           (emits-expr '(defn* main []))))
    (is (= "fn main() {\n\tprintln!(\"Hello World!\")\n}\n"
           (emits-expr '(defn* main []
                          (rs/println! "Hello World!")))))
    (is (= "fn one(x: u8) {\n\tx\n}\n"
           (emits-expr '(defn* one [^u8 x] x))))))

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

(deftest emit-let-test
  (testing "let"
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\n\1\n\}"
                 (emits-expr '(let* [x 1] x))))
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\tlet (y__\d+) = 20;\n\n\1\n\}"
                 (emits-expr '(let* [x 1 y 20] x))))
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\tlet (y__\d+) = 20;\n\n\{\n\tclrs.test.foo\(\1,\2\);\n\t\1\n\}\n\}"
                 (emits-expr '(let* [x 1 y 20]
                                (foo x y)
                                x))))

    (testing "prints types when they're hinted"
      (is (matches #"\{\n\tlet (x__\d+): u8 = 1;\n\n\1\n\}"
                   (emits-expr '(let* [^u8 x 1] x)))))))

(deftest emit-loop-test
  (testing "loop"
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\nloop \{\n\1\nbreak;\n\}\n\}"
                 (emits-expr '(loop [x 1] x))))))

(deftest emit-recur-test
  (testing "fn-recur"
    (is (matches #"\|x\| \{\n\tloop \{\n\{\n\tlet ([\w\d_]+) = 1;\nx = \1;\ncontinue;\n\}\nbreak;\n\}\n\}\n"
                 (emits-expr '(fn* [x] (recur 1))))))

  (testing "loop-recur"
    (is (matches #"\{\n\tlet (x__\d+) = 1;\n\nloop \{\n\1\nbreak;\n\}\n\}"
                 (emits-expr '(loop [x 1] x))))))

(deftest emit-new-test
  (testing "new"
    (is (= "clrs.test.Foo {\n}\n"
           (emits-expr '(new Foo))))
    (is (= "clrs.test.Foo {\n\tx: 1,\n}\n"
           (emits-expr '(new Foo x 1))))
    (is (= "clrs.test.Foo {\n\tx: 1,\n\ty: 2,\n}\n"
           (emits-expr '(new Foo x 1 y 2))))))

(deftest emit-set!-test
  (testing "set!"
    (is (matches #"\{\n\tlet mut (x__\d+) = 10;\n\n\1 = 20\n\}"
                 (emits-expr '(let* [^:mut x 10]
                                (set! x 20)))))))

(deftest emit-ns-test
  (testing "ns"
    (is (= "mod foo {\n\t\n}\n"
           (emits-expr '(ns foo))))
    (is (= "mod foo.bar {\n\t\n}\n"
           (emits-expr '(ns foo.bar))))
    (is (= "mod foo.bar {\n\tuse baz::bar;\n\n}\n"
           (emits-expr '(ns foo.bar
                          :requires {baz.bar baz.bar}))))))

(deftest emit-defstruct*-test
  (testing "defstruct*"
    (is (= "pub struct Foo {\n}\n"
           (emits-expr '(defstruct* Foo))))
    (is (= "struct Foo {\n}\n"
           (emits-expr '(defstruct* ^:private Foo))))
    (is (= "pub struct Foo {\n\tx: uint,\n}\n"
           (emits-expr '(defstruct* Foo [x uint]))))
    (is (= "pub struct Foo {\n\tx: uint,\n\ty: Box<foo>,\n}\n"
           (emits-expr '(defstruct* Foo [x uint y Box<foo>]))))
    (is (= "pub struct Foo {\n\tpriv x: uint,\n}\n"
           (emits-expr '(defstruct* Foo [^:private x uint]))))))

(deftest emit-defenum*-test
  (testing "defenum*"
    (is (= "pub enum Foo {\n}\n"
           (emits-expr '(defenum* Foo))))
    (is (= "enum Foo {\n}\n"
           (emits-expr '(defenum* ^:private Foo))))
    (is (= "pub enum Foo {\n\tV1,\n\tV2,\n}\n"
           (emits-expr '(defenum* Foo V1 V2))))
    (is (= "pub enum Foo {\n\tV1(uint),\n}\n"
           (emits-expr '(defenum* Foo [V1 uint]))))))

(deftest emit-core
  (with-open [core (java.io.PushbackReader. (io/reader "src/crust/core.clrs"))
              out-file  (io/writer "target/core.rs")]
    (binding [*out* out-file]
      (let [forms (->> #(read core false nil)
                       repeatedly
                       (take-while identity))]
        (doseq [form forms]
          (sut/emit (sut/analyze sut/empty-env form)))))))
