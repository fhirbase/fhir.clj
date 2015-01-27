(ns fhir.format
  (:require [fhir.core :as fc]
            [clojure.xml :as cx]
            [fhir.profiles :as fp]
            [clojure.edn :as ce]
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


(defn coerce-primitive [-meta value]
  (if-let [tp (get-in -meta [:$attrs :type 0])]
    (if (string? value)
      (cond
        (= tp :boolean) (ce/read-string value)
        :else value)
      value)
    value))

(defn coerce-resource [res pth]
  (cond
    (map? res) (reduce (fn [acc [k v]]
                         (let [-meta (fp/find-meta (conj pth k))
                               mult (= "*" (get-in -meta [:$attrs :max]))]
                           (assoc acc k
                                  (if (and mult (not (vector? v)))
                                    [(coerce-resource v (conj pth k))]
                                    (coerce-resource v (conj pth k)))))
                         ) {} res)
    (vector? res) (mapv #(coerce-resource % pth) res)
    :else (coerce-primitive (fp/find-meta pth) res)))

(defn from-xml-recur [content]
  (reduce
    (fn [acc node]
      (let [attr (:tag node)
            prev-value (get acc attr)
            value (if (:content node)
                    (from-xml-recur (:content node))
                    (get-in node [:attrs :value]))]
        (update-in acc [attr]
                   (fn [v]
                     (cond
                       (nil? v) value
                       (vector? v) (conj v value)
                       :else [v value])))))
    {} content))


;;; PUBLIC API

(defn to-json [res] res)

(defn from-json [s]
  (let [res (json/parse-string s keyword)
        res-nm (keyword (:resourceType res)) ]
    (assoc (coerce-resource res [res-nm]) :resourceType (name res-nm))))

(defn from-xml [s]
  (let [xml (parse-xml s)
        res-nm (:tag xml)
        res    (from-xml-recur (:content xml))]
    (assoc (coerce-resource res [res-nm]) :resourceType (name res-nm))))


(defn to-xml [res]
  (let [res-nm (:resourceType res)
        zres (fc/zip-meta res)
        content (xml-resource-content (get-in zres [(keyword res-nm)]))
        xml-data  (xml-resource-tag res content)]
    (with-out-str (cx/emit-element xml-data))))
