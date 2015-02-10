(ns fhir.client-test
  (:require [clojure.test :refer :all]
            [fhir.core :as fc]
            [fhir.client :as fcl]))

(comment
  (def idx
    (fc/index
      "profiles/profiles-resources.json"
      "profiles/profiles-types.json"))
  (def pt-bndl (fcl/GET idx (fcl/url "http://fhirtest.uhn.ca/baseDstu2" "Patient?_format=xml&name=john" )))
  (def pt (get-in pt-bndl [:entry 0 :resource]))

  (println (get-in pt [:name 0 :family 0]))
  (fc/validate idx pt))
