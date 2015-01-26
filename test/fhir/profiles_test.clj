(ns fhir.profiles-test
  (:require [clojure.test :refer :all]
            [fhir.utils :as fu]
            [fhir.profiles :as fp]))

(deftest meta-test
  (testing "find-meta"
    (is (= (fp/find-meta [:Patient :name :use])
           {:$attrs {:path [:HumanName :use] :ord 3 :type [:code] :max "1" :min 0}}))

    (is (= (:$attrs (fp/find-meta [:Patient :name]))
           {:path [:Patient :name], :ord 18, :type [:HumanName], :max "*", :min 0}))))
