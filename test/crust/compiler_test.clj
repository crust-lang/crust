(ns crust.compiler-test
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer.jvm :as jvm]
            [crust.compiler :refer :all]))

(deftest rs-output-test
  (testing "constants"
    (is (= "()" (emit (jvm/analyze nil))))
    (is (= "\"hello\"" (emit (jvm/analyze "hello"))))
    (is (= "1" (emit (jvm/analyze 1)))))

  (testing "special forms"
    (testing "if"
      (is (= "if true { 1 } else { () }" (emit (jvm/analyze '(if true 1)))))
      (is (= "if true { 1 } else { 2 }" (emit (jvm/analyze '(if true 1 2))))))

    (testing "do"
      (is (= "{ if true { 1 } else { 2 }; if false { 1 } else { 2 } }"
             (emit (jvm/analyze '(do (if true 1 2) (if false 1 2)))))))

    (testing "fn"
      (is (= "|x__0| { x__0 }"
             (emit (jvm/analyze '(fn [x] x)))))

      (is (= "|x__0,y__0| { x__0 }"
             (emit (jvm/analyze '(fn [x y] x))))))

    (testing "let"
      (is (= "{ let x__0 = 10; x__0 }"
             (emit (jvm/analyze '(let [x 10] x))))))

    (testing "invoke"
      (is (= "{ let id__0 = |x__0| { x__0 }; id__0(()) }"
             (emit (jvm/analyze '(let [id (fn [x] x)]
                                  (id nil)))))))

    (testing "set!"
      #_(is (= "{ let mut x__0 = 10; x = 5; }"
             (emit (jvm/analyze '(let [^:mut x 10]
                                  (set! x 5)))))))))
