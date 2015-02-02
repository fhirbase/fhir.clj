(ns fhir.core
  (:require
    [fhir.format :as ff]
    [fhir.utils :as fu]
    [fhir.meta :as fm]
    [fhir.profiles :as fp]))

(def re-xml #"(?m)^<.*>")
(def re-json #"(?m)^[{].*")

(defn index? [x]
  (map? x))

(defn index
  "prs - list of pathes to json profiles"
  [& prs]
  (->> (map (fn [pth] (fu/read-json pth)) prs)
       (apply fp/index-profiles)))

(defn parse
  ([idx x]
   {:pre (fp/index? idx)}
   (cond
     (re-seq re-xml x) (parse idx :xml x)
     (re-seq re-json x) (parse idx :json x)
     :else (throw (Exception. "Don't know how to parse: " (pr-str x)))))
  ([idx fmt s]
   {:pre [(fp/index? idx)
          (contains? #{:json :xml} fmt)]}
   (cond
     (= fmt :xml) (ff/from-xml idx s)
     (= fmt :json) (ff/from-json idx s))))

(defn validate
  "validate resource and return issues for OperationOutcomme"
  ([idx res]
   {:pre [(fp/index? idx) (map? res)]}
   (fm/validate idx res)))

(defn resource [idx res]
  {:pre [(fp/index? idx)
         (map? res)]}
  (ff/coerce-resource idx res))

(defn generate [idx fmt res]
  {:pre [(fp/index? idx)
         (contains? #{:json :xml} fmt)
         (map? res)]}
  (cond
    (= fmt :xml) (ff/to-xml idx res)
    (= fmt :json) (ff/to-json idx res)))
