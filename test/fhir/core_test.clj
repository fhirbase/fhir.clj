(ns fhir.core-test
  (:require [clojure.test :refer :all]
            [fhir.core :as fc]))
(def idx
  (fc/index
     "profiles/profiles-resources.json"
     "profiles/profiles-types.json"))

(deftest core-test
  (def pt-json (fc/parse idx "{\"resourceType\": \"Patient\", \"name\":[{\"text\":\"Goga\"}]}"))
  (def pt-xml (fc/parse idx "<Patient><name><text value=\"Goga\"/></name></Patient>"))
  (def pt-clj (fc/resource idx {:resourceType "Patient" :name {:text "Goga"}}))
  (is
    (= pt-json pt-xml pt-clj))

  (is
    (= pt-json
       (fc/parse idx (fc/generate idx :json pt-json)))))
