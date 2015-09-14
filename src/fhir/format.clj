(ns fhir.format
  (:require [fhir.meta :as fc]
            [clojure.xml :as cx]
            [clojure.data.xml :as xml]
            [fhir.profiles :as fp]
            [fhir.utils :as fu]
            [clojure.edn :as ce]
            [clojure.string :as cs]
            [cheshire.core :as json]
            [clojure.zip :as cz]))


(defn by-ord [[k [m v]]]
  (or (get-in m [:$attrs :ord]) (:ord m)))

(defn remove-empty [[k [m v]]]
  (not (nil? v)))

;;TODO: remove used?
(defn normalize-string [s]
  (cs/replace s #"\s+" " "))

(defn coerce-primitive [-meta value]
  (if-let [tp (get-in -meta [:$attrs :type 0])]
    (if (string? value)
      (cond
        (= tp :boolean) (ce/read-string value)
        (= tp :integer) (int (ce/read-string value))
        (= tp :decimal) (ce/read-string value)
        (= tp :string)  (normalize-string value)
        :else value)
      value)
    value))

(defn is-collection? [-meta]
  (= "*" (get-in -meta [:$attrs :max])))

(declare coerce-resource)

(defn- coerse-resource-element [idx pth v]
  (let [-meta (fp/find-meta idx pth)
        mult  (is-collection? -meta)]
    (cond
      ;; normalize xml string for equality
      (= (last pth) :div) (fu/normalize-xml-str v)
      ;; fix collections
      (and mult (nil? v)) []
      (and mult (not (vector? v))) [(coerce-resource idx v pth)]
      :else (coerce-resource idx v pth))))

(defn coerce-resource
  ([idx res]
   (if-let [res-nm (:resourceType res)]
     (coerce-resource idx res [(keyword res-nm)])
     (throw (Exception. "WARN: Ups enter coerce-resource without resourceType"))))
  ([idx res pth]
   (if (and (:resourceType res) (not= pth [(keyword (:resourceType res))]))
     (coerce-resource idx res)
     (cond
       (map? res)    (fu/mapmap #(coerse-resource-element idx (conj pth %1) %2) res)
       (vector? res) (mapv #(coerce-resource idx % pth) res)
       :else (coerce-primitive (fp/find-meta idx pth) res)))))

(fu/TODO "check in resources list")
(defn is-resource-key? [res-nm]
  (let [fl (subs (name res-nm) 0 1)]
    (= fl (.toUpperCase fl))))

(fu/TODO "fix convertion of extensins into xml")
(defn from-xml-recur [content]
  (-> (fn [acc node]
        (cond
          ;; fix text html
          (= (:tag node) :div)
          (assoc acc :div (fu/emit-html node))

          ;; handle bundle nested resources
          (is-resource-key? (:tag node))
          (let [res-nm (name (:tag node))
                res (from-xml-recur (get-in node [:content]))]
            (assoc res :resourceType res-nm))

          :else
          (let [attr (:tag node)
                ;; fix diff in extension representation
                attr-key (if (= attr :extension)
                           (keyword (get-in node [:attrs :url]))
                           attr)
                prev-value (get acc attr)
                value (if (seq (:content node))
                        (from-xml-recur (:content node))
                        (get-in node [:attrs :value]))]
            (update-in acc [attr-key]
                       (fn [v]
                         (cond
                           ;; fix diff in extension representation
                           (= attr :extension) [value]
                           (nil? v) value
                           (vector? v) (conj v value)
                           :else [v value]))))))
      (reduce {} content)))

(defn xml-resource-tag [res cnt]
  (let [res-nm (:resourceType res)]
    (apply xml/element res-nm
           {:xmlns "http://hl7.org/fhir"}
           cnt)))

(declare xml-resource-content)

(defn xml-element-tag [-key -meta -val]
  (cond
    (map? -val) (apply xml/element -key {}
                       (xml-resource-content -val))
    :else       (xml/element -key {:value -val})))

(defn xml-resource-content [el]
  (->>
    (filter remove-empty el)
    (sort-by by-ord )
    (reduce (fn [acc [k [m v]]]
              (if (vector? v)
                (into
                  acc (mapv (fn [v]  (xml-element-tag k m v)) v))
                (conj
                  acc (xml-element-tag k m v)))) [])
    (filter identity)))

;;; PUBLIC API

(defn from-json [idx s]
  {:pre (string? s)}
  (->> (fu/from-json s)
       (coerce-resource idx)))

(defn from-xml [idx s]
  {:pre (string? s)}
  (let [xml (fu/parse-xml s)
        res-nm (:tag xml)
        res    (from-xml-recur (:content xml))]
    (coerce-resource idx (assoc res :resourceType (name res-nm)))))


(defn to-json [idx res]
  (fu/to-json res))

;; could this be done without zip meta
(defn to-xml [idx res]
  (let [res-nm (:resourceType res)
        zres (fc/zip-meta idx res)
        content (xml-resource-content (get-in zres [(keyword res-nm)]))
        xml-data  (xml-resource-tag res content)]
    (fu/emit-xml xml-data)))
