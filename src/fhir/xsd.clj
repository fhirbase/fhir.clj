(ns fhir.xsd
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]))

;; convert datathypes from xsd to profiles json

(defn zip-str  [s]
  (zip/xml-zip
    (xml/parse  (java.io.ByteArrayInputStream.  (.getBytes s)))))

(def base (zip-str (slurp "xml/fhir-base.xsd")))


(defn attr [x nm] (get-in x [:attrs nm]))

(defn attr-eq? [nm v]
  (fn [x]
    (= (attr x nm) v)))

(defn attr-re? [nm re]
  (fn [x]
    (re-matches re (attr x nm))))

(defn mk-filter [pth-item]
  (cond
    (vector? pth-item) (let [[tg pred] pth-item]
                         (fn [x] (and (= (:tag x) tg) (pred x))))
    (keyword? pth-item) (fn [x]
                          (= (:tag x) pth-item))))

(defn select [[p & px] xs]
  (if p
    (->> (mapcat (fn [x] (:content x)) xs)
         (filter (mk-filter p))
         (select px))
    xs))

;;[:xs:enumeration (attr-re? :value #"^[A-Z].*")]
(def types-list
  (->> base
       (select
         [[:xs:simpleType (attr-eq? :name "DataType-list")]
          :xs:restriction
          :xs:enumeration])
       (map #(attr % :value))))

(def complex-types-list
  (filter #(re-matches #"^[A-Z].*" %) types))

(def types-idx
  (->>
    base
    (select [[:xs:complexType
              (fn [x] (contains? (set complex-types-list) (attr x :name)))]])
    (reduce
      (fn [acc x]
        (assoc acc (attr x :name) x)))))

(defn xsd-el-to-fhir-el [root y]
  {:path [root (attr y :name)]
   :type [{:code (attr y :type)}]
   :min (attr y :minOccurs)
   :max (attr y :maxOccurs)
   :short (->>
            (select [:xs:annotation :xs:documentation] [y])
            (mapcat :content)
            (apply str))})

(defn collect-els [x]
  (->> [x]
       (select
         [:xs:complexContent :xs:extension :xs:sequence :xs:element])
       (mapv (fn [y] (xsd-el-to-fhir-el (attr x :name) y)))))

(defn collect [x]
  {:type (attr x :name)
   :snapshot {:element (collect-els x)}})

(doseq [k complex-types-list]
  (println (collect (get types-idx k))))
