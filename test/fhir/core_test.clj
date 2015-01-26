(ns fhir.core-test
  (:require [clojure.test :refer :all]
            [fhir.utils :as fu]
            [fhir.core :as fc]))

(defn required? [mt]
  (= (get-in mt [:$attrs :min]) 1))

(def pt {:resourceType "Patient"
         :name [{:text "Ups"}]
         :unexpected "Hi"
         :link [{}] })

(defn validate [acc pth v mt]
  (cond (nil? mt)             (conj acc [:unexpected-element pth v])
        (and (nil? v)
             (required? mt))  (conj acc [:missed-element pth])
        :else  acc))

(deftest meta-test
  (testing "find-meta"
    (is (= (fc/find-meta [:Patient :name :use])
           {:$attrs {:path [:HumanName :use] :ord 3 :type [:code] :max "1" :min 0}}))

    (is (= (:$attrs (fc/find-meta [:Patient :name]))
           {:path [:Patient :name], :ord 49, :type [:HumanName], :max "*", :min 0})))

  (testing "resource-reduce"
    (is
      (= 3 (count (fc/reduce-resource pt validate))))))

(comment
  ;;example
  (def pt (fu/read-json "examples/pt.json"))
  (fc/reduce-resource pt validate))
