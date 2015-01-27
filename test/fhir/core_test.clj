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

(deftest reduce-resource
  (testing "resource-reduce"
    (is
      (= 3 (count (fc/reduce-resource pt validate))))))

(def ptm (fc/zip-meta {:resourceType "Patient" :name {:text "My name" :family ["a" "b"]}}))

(deftest zip-meta
  (testing "zip-meta"
    (is
      (is (= 27 (count (get-in ptm [:Patient])))))))

(comment
  ;;example
  (def pt (fu/read-json "examples/pt.json"))
  (fc/reduce-resource pt validate)
  (def ptm (fc/zip-meta {:resourceType "Patient" :name {:text "My name" :family ["a" "b"]}}))
  (sort-by #(:ord (first %))
    (map (fn [[k [m v]]] [m v] ) (second (:Patient ptm)))))
