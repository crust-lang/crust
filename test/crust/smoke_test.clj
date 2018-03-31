(ns crust.smoke-test
  (:require [clojure.test :refer [deftest is testing]]
            [crust.clrs :as crust]
            [clojure.java.shell :refer [sh]]
            [clojure.java.io :refer [writer]]
            ))

(deftest end-to-end
  (testing "basic compilation"
    (let [program '(defn* main [] (rs/println! "Hello World!"))
          file-name "target/main.rs"]
      (with-open [f (writer file-name)]
        (binding [*out* f]
          (crust/emit (crust/analyze crust/empty-env program))))
      (let [result (sh "rustc" "-o" "target/main" file-name)]
        (assert (= (:exit result) 0) (str "Did not compile correctly" (:err result)))
        (let [execution (sh "./target/main")]
          (is (= 0 (:exit execution)))
          (is (= "Hello World!\n" (:out execution))))))))
