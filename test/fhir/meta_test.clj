(ns fhir.meta-test
  (:require [clojure.test :refer :all]
            [fhir.utils :as fu]
            [fhir.profiles :as fp]
            [fhir.meta :as fm]))

(def idx
  (fp/index-profiles
    (fu/read-json "profiles/profiles-resources.json")
    (fu/read-json "profiles/profiles-types.json")))

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
      (= 3 (count (fm/reduce-resource idx pt validate))))))

(def ptm (fm/zip-meta idx {:resourceType "Patient" :name {:text "My name" :family ["a" "b"]}}))

(deftest zip-meta
  (testing "zip-meta"
    (is (= 27 (count (get-in ptm [:Patient]))))))

(comment
  ;;example
  (def pt (fu/read-json "examples/pt.json"))
  (fm/reduce-resource pt validate)
  (def ptm (fm/zip-meta {:resourceType "Patient" :name {:text "My name" :family ["a" "b"]}}))
  (sort-by #(:ord (first %))
           (map (fn [[k [m v]]] [m v] ) (second (:Patient ptm)))))
