(ns fhir.format-test
  (:require [clojure.test :refer :all]
            [fhir.utils :as fu]
            [fhir.core :as fc]
            [clojure.data :as cd]
            [fhir.format :as ff]))

(def pt {:resourceType "Patient"
         :id "myid"
         :active true
         :identifier [{:use "usual"
                       :system "local"
                       :value "777"
                       :period {:start "2011-0101"}}]
         :name [{:text "Goga"
                 :family ["shuba" "duba"]
                 :given ["giv1" "giv2"]}]})

(def pt-xml (slurp "test/fhir/pt.xml"))

(deftest text-to-xml
  (is (= (ff/parse-xml (ff/to-xml pt))
         (ff/parse-xml pt-xml))))

(def pt2 (ff/from-json (slurp "test/fhir/patient-example-f001-pieter.json")))
(def pt2-xml (slurp "test/fhir/patient-example-f001-pieter.xml"))

(deftest to-xml
  (is (= (ff/parse-xml (ff/to-xml pt2))
         (ff/parse-xml pt2-xml))))

(deftest from-xml
  (is (= pt2 (ff/from-xml pt2-xml))))

;; just test for &nbsp

(deftest test-npsp-problem
  (is (try-expr "ups"
                (ff/parse-xml "<div>&amp; &nbsp;</div>"))))

(def bndl (ff/from-xml "<Bundle xmlns=\"http://hl7.org/fhir\"> <entry> <resource> <MedicationPrescription> <id value=\"3123\"/> </MedicationPrescription> </resource> </entry> </Bundle>"))

(deftest bundle-from-xml
  (is (= (get-in bndl [:entry 0 :resource :resourceType])
         "MedicationPrescription")))


(deftest xml-parse-gen-test
  (let [s "<div>&amp;</div>"]
    (is (= (ff/emit-xml (ff/parse-xml s))
           s))))


(def bndl-json (ff/from-json (slurp "test/fhir/bundle-example.json")))
(def bndl-xml  (ff/from-xml  (slurp "test/fhir/bundle-example.xml")))

(fu/resources-diff bndl-json bndl-xml)

#_(println bndl-xml)

(deftest bundle-parsing
  (is (= bndl-json bndl-xml)))

(comment
  (cd/diff
    (ff/parse-xml (ff/to-xml pt2))
    (ff/parse-xml pt2-xml))
  (cd/diff
    pt2 (ff/from-xml pt2-xml))
  (ff/from-xml pt2-xml)
  (spit "/tmp/ups.xml" (ff/to-xml pt2))
  (get-in (fc/zip-meta pt2) [:Patient :maritalStatus])
  (get-in (fc/zip-meta pt2) [:Patient :gender])
  (println (ff/to-xml pt)))
