(ns crust.core-test
	(:require [clojure.test :refer :all]
						[crust.core :refer :all]))

(deftest rs-output-test
	(testing "constants"
		(is (= "\"another string\""
					 (emit '{:op :const,
									 :env
									 {:context :ctx/expr,
										:locals {},
										:ns user,
										:file
										"/private/var/folders/b5/8mgcmpl14rv6np5f6h1ygtjw0000gn/T/form-init5910749599388819816.clj"},
									 :type :string,
									 :literal? true,
									 :val "another string",
									 :form "another string",
									 :top-level true,
									 :o-tag java.lang.String,
									 :tag java.lang.String})))
		(is (= "\"hello\""
					 (emit '{:op :const,
									 :env
									 {:context :ctx/expr,
										:locals {},
										:ns user,
										:file
										"/private/var/folders/b5/8mgcmpl14rv6np5f6h1ygtjw0000gn/T/form-init5910749599388819816.clj"},
									 :type :string,
									 :literal? true,
									 :val "hello",
									 :form "hello",
									 :top-level true,
									 :o-tag java.lang.String,
									 :tag java.lang.String})))
		(is (= "1"
					 (emit '{:op :const,
									 :env
									 {:context :ctx/expr,
										:locals {},
										:ns user,
										:file "/private/var/folders/b5/8mgcmpl14rv6np5f6h1ygtjw0000gn/T/form-init5910749599388819816.clj"},
									 :type :number,
									 :literal? true,
									 :val 1,
									 :form 1,
									 :top-level true,
									 :o-tag long,
									 :tag long}))))

	(testing "special forms"
		(is (= "if true { 1 }"
					 (emit '{:children [:test :then],
									 :op :if,
									 :o-tag long,
									 :top-level true,
									 :then
									 {:op :const,
										:type :number,
										:literal? true,
										:val 1,
										:form 1,
										:o-tag long,
										:tag long},
									 :form (if true 1 2),
									 :tag long,
									 :test
									 {:op :const, 
										:type :bool,
										:literal? true,
										:val true,
										:form true,
										:o-tag java.lang.Boolean,
										:tag java.lang.Boolean}})))
		(is (= "if true { 1 } else { 2 }"
					 (emit '{:children [:test :then :else],
									 :else
									 {:op :const,
										:type :number,
										:literal? true,
										:val 2,
										:form 2,
										:o-tag long,
										:tag long},
									 :op :if,
									 :o-tag long,
									 :top-level true,
									 :then
									 {:op :const,
										:type :number,
										:literal? true,
										:val 1,
										:form 1,
										:o-tag long,
										:tag long},
									 :form (if true 1 2),
									 :tag long,
									 :test
									 {:op :const, 
										:type :bool,
										:literal? true,
										:val true,
										:form true,
										:o-tag java.lang.Boolean,
										:tag java.lang.Boolean}})))))
