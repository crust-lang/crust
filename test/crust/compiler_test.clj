(ns crust.compiler-test
  (:require [clojure.test :refer :all]
            [clojure.tools.analyzer.jvm :as jv]
            [crust.compiler :refer :all]))

(deftest rs-output-test
  (testing "constants"
    (is (= "()" (emit (jv/analyze nil))))
    (is (= "\"hello\"" (emit (jv/analyze "hello"))))
    (is (= "1" (emit (jv/analyze 1)))))

  (testing "special forms"
    (testing "if"
      (is (= "if true { 1 } else { () }" (emit (jv/analyze '(if true 1)))))
      (is (= "if true { 1 } else { 2 }" (emit (jv/analyze '(if true 1 2))))))

    (testing "do"
      (is (= "{ if true { 1 } else { 2 }; if false { 1 } else { 2 } }"
             (emit (jv/analyze '(do (if true 1 2) (if false 1 2)))))))

    (testing "fn"
      (is (= "|x__0| { x__0 }"
             (emit (jv/analyze '(fn [x] x)))))

      (is (= "|x__0,y__0| { x__0 }"
             (emit (jv/analyze '(fn [x y] x))))))

    (testing "let"
      (is (= "{ let x__0 = 10; x__0 }"
             (emit (jv/analyze '(let [x 10] x))))))

    (testing "invoke"
      (is (= "{ let id__0 = |x__0| { x__0 }; id__0(()) }"
             (emit (jv/analyze '(let [id (fn [x] x)]
                                  (id nil)))))))))
