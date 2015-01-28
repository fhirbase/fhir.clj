(ns fhir.profiles-test
  (:require [clojure.test :refer :all]
            [fhir.utils :as fu]
            [fhir.profiles :as fp]))


(deftest meta-test
  (testing "expansion of *"
    (is (not (nil?
               (get-in fp/idx [:ElementDefinition :defaultValueString])))))
  (testing "find-meta"
    (is (= [:HumanName :use]
           (->
             (fp/find-meta [:Patient :name :use])
             (get-in [:$attrs :path]))))

    (is (= [:HumanName]
           (-> (fp/find-meta [:Patient :name])
               (get-in [:$attrs :type])))))


  (testing "name refs"
    (is
      (= [:Profile :snapshot :element]
         (-> (fp/find-meta [:Profile :differential :element])
             (get-in [:$attrs :path])
             )))))
