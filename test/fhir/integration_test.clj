(ns fhir.integration-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [clojure.data :as cd]
            [clojure.string :as cstr]
            [clojure.set :as cs]
            [fhir.utils :as fu]
            [fhir.format :as ff]))


(deftest problems-with-name-refs
  (is (=
       (dissoc (ff/from-json (slurp "test/fixtures/json/dateTime.profile.json")) :text)
       (dissoc (ff/from-xml (slurp "test/fixtures/xml/dateTime.profile.xml")) :text))))


(deftest integration-test
  (doseq  [f (fs/glob "test/fixtures/xml/*.xml")]
    (let [json-file (str "test/fixtures/json/" (fs/base-name f ".xml") ".json")]
      (println json-file)
      (if (fs/exists? json-file)
        (let [from-xml (-> (ff/from-xml (slurp (.getAbsolutePath f)))
                           (dissoc :text))
              from-json (-> (ff/from-json (slurp json-file))
                            (dissoc :text))]
          (if-not (= from-xml from-json)
            (println json-file)
            (fu/resources-diff from-xml from-json)))))))
