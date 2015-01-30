(ns fhir.integration-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [clojure.data :as cd]
            [fhir.utils :as fu]
            [fhir.profiles :as fp]
            [fhir.format :as ff]))

(def idx
  (fp/index-profiles
    (fu/read-json "profiles/profiles-resources.json")
    (fu/read-json "profiles/profiles-types.json")))


(deftest problems-with-name-refs
  (is (=
       (ff/from-json idx (slurp "test/fixtures/json/dateTime.profile.json"))
       (ff/from-xml idx (slurp "test/fixtures/xml/dateTime.profile.xml")))))


(deftest integration-test
  (doseq  [f (fs/glob "test/fixtures/xml/*.xml")]
    (let [json-file (str "test/fixtures/json/" (fs/base-name f ".xml") ".json")]
      (if (fs/exists? json-file)
        (let [from-xml (-> (ff/from-xml idx (slurp (.getAbsolutePath f)))
                           (dissoc :text))
              from-json (-> (ff/from-json  idx (slurp json-file))
                            (dissoc :text))]
          (when-not (= from-xml from-json)
            (println "json/xml are not equal" json-file)
            #_(fu/resources-diff from-xml from-json)))))))
