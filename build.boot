(def project 'crust)
(def version "0.1.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src/clj" "src/cljt"}
          :source-paths   #{"test/clj" "test/cljt"}
          :dependencies   (template
                           [[org.clojure/clojure ~(clojure-version)]]))

(task-options!
 pom {:project     project
      :version     version
      :description "FIXME: write description"
      :url         "http://example/FIXME"
      :scm         {:url "https://github.com/crust-lange/crust"}
      :license     {"Eclipse Public License"
                    "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask build
  "Build and install the project locally."
  []
  (comp (pom) (jar) (install)))
