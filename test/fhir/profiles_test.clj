(ns fhir.profiles-test
  (:require [clojure.test :refer :all]
            [fhir.utils :as fu]
            [fhir.profiles :as fp]))

(def idx
  (fp/index-profiles
    (fu/read-json "profiles/profiles-resources.json")
    (fu/read-json "profiles/profiles-types.json")))

(deftest meta-test
  (testing "expansion of *"
    (is (not (nil?
               (get-in idx [:idx :ElementDefinition :defaultValueString])))))
  (testing "find-meta"
    (is (= [:HumanName :use]
           (->
             (fp/find-meta idx [:Patient :name :use])
             (get-in [:$attrs :path]))))

    (is (= [:HumanName]
           (-> (fp/find-meta idx [:Patient :name])
               (get-in [:$attrs :type])))))


  (testing "name refs"
    (is
      (= [:Profile :snapshot :element]
         (-> (fp/find-meta idx [:Profile :differential :element])
             (get-in [:$attrs :path])
             )))))
