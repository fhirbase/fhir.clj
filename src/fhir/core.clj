(ns fhir.core
  (:require
    [clojure.string :as cs]
    [cheshire.core :as json]))

(def pt-json (slurp "profiles/patient.json"))

(def pt-prof (json/parse-string pt-json keyword))
(def els (get-in pt-prof [:snapshot :element]))
(def el (nth els 5))

(defn get-path [x] (->> (cs/split (:path x) #"\.")
                        (mapv keyword)))
(def els-idx
  (reduce #(assoc %1 (get-path %2) %2) {} els))

(println (keys els-idx))

(defn walk [m path profile]
  (cond
    (map? m)    (doseq [[k,v] m] (walk v (conj path k) profile))
    (vector? m) (doseq [v m] (walk v path profile))
    :else (do
            (println path (get profile path))
            (when-not (get profile path)
              (println "Extra key" path)))))

(defn validate-structure [m profile]
   (walk m [(keyword (:resourceType m))] profile))

(validate-structure {:resourceType "Patient"
           :extra "ups"
           :name "ups"
           :name [{:given  ["name"]
                   :family ["name"]}]}
          els-idx)

;;(build pt-profile)
