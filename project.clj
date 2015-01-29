(defproject fhir "0.1.0-SNAPSHOT"
  :description "FHIR client in clojure"
  :url "https://github.com/fhirbase/fhir.clj"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins  [[lein-cljfmt "0.1.3"]
             [jonase/eastwood "0.2.1"]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [clj-time "0.9.0"]
                 ;;[org.apache.commons/commons-lang3 "3.0"]
                 [me.raynes/fs "1.4.6"]
                 [org.clojure/data.xml "0.0.8"]
                 [cheshire "5.4.0"]])
