(ns fhir.client
  (:require [org.httpkit.client :as cc]
            [fhir.core :as fc]
            [fhir.format :as ff]))

(defn url [base-url & parts]
  (apply str base-url "/" (interpose "/" parts)))

(defn GET [idx url]
  (->>
    (cc/get url {:throw-exceptions false})
    (deref)
    (:body)
    (fc/parse idx)))
