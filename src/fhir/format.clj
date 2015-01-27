(ns fhir.format
  (:require [fhir.core :as fc]
            [clojure.xml :as cx]
            [clojure.stacktrace :as cs]
            [cheshire.core :as json]
            [clojure.zip :as cz]))

(defn parse [st])


(defn stream [s]
  (java.io.ByteArrayInputStream. (.getBytes s)))

(defn xml-resource-tag [res cnt]
  (let [res-nm (:resourceType res)]
    {:tag res-nm :attrs {:xmlns "http://hl7.org/fhir"} :content cnt}))

(declare xml-resource-content)

(defn xml-element-tag [-key -meta -val]
  (cond
    (map? -val) {:tag -key
                 :content (xml-resource-content -val)}
    :else {:tag -key
           :attrs {:value -val}}))

(defn sort-by-ord [[k [m v]]]
  (or (get-in m [:$attrs :ord]) (:ord m)))

(defn remove-empty [[k [m v]]]
  (not (nil? v)))

(defn xml-resource-content [el]
  (->>
    (filter remove-empty el)
    (sort-by sort-by-ord )
    (reduce (fn [acc [k [m v]]]
              (if (vector? v)
                (into
                  acc (mapv (fn [v]  (xml-element-tag k m v)) v))
                (conj
                  acc (xml-element-tag k m v)))) [])
    (filter identity)))


(defn parse-xml [s]
  (cx/parse (stream s)))

(defn from-json [s]
  (json/parse-string s keyword))

(defn to-json [res] res)

(defn to-xml [res]
  (let [res-nm (:resourceType res)
        zres (fc/zip-meta res)
        content (xml-resource-content (get-in zres [(keyword res-nm)]))
        xml-data  (xml-resource-tag res content)]
    (with-out-str (cx/emit-element xml-data))))
