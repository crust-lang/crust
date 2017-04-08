(ns crust.core-test
	(:require [clojure.test :refer :all]
            [clojure.tools.analyzer.jvm :as jv]
						[crust.core :refer :all]))

(deftest rs-output-test
	(testing "constants"
		(is (= "()" (emit (jv/analyze nil)))) 
		(is (= "\"hello\"" (emit (jv/analyze "hello"))))
		(is (= "1" (emit (jv/analyze 1)))))

	(testing "special forms"
		(is (= "if true { 1 } else { () }" (emit (jv/analyze '(if true 1)))))
		(is (= "if true { 1 } else { 2 }" (emit (jv/analyze '(if true 1 2)))))
    (is (= "{ if true { 1 } else { 2 }; if false { 1 } else { 2 } }" 
           (emit (jv/analyze '(do (if true 1 2) (if false 1 2))))))))
