(ns fhir.meta
  (:require
    [clojure.set :as cset]
    [fhir.utils :as fu]
    [fhir.profiles :as fp]))

(defn- reduce-recur
  "walk resource recursively and collect accumulator by (f acc path value meta-info)"
  [idx obj pth f acc]
  (let [-meta (fp/find-meta idx pth)
        acc (f acc pth obj -meta)
        reduce-map #(reduce-recur idx (get obj %2) (conj pth %2) f %1)
        reduce-vec #(reduce-recur idx %2 pth f %1)]
    (cond
      (map? obj)    (reduce reduce-map acc (fu/all-keys -meta obj))
      (vector? obj) (reduce reduce-vec acc obj)
      :else acc)))

(defn reduce-resource [idx obj f]
  (let [init-pth [(keyword (:resourceType obj))]]
    (reduce-recur idx (dissoc obj :resourceType) init-pth  f [])))

(defn zip-meta-recur [idx obj pth]
  (let [-meta (fp/find-meta idx pth)
        -attrs (:$attrs -meta)
        reduce-map (fn [acc k]
                     (let [epth (conj pth k)
                           v (get obj k)
                           emeta (fp/find-meta idx epth)]
                       (assoc acc k [emeta (zip-meta-recur idx v epth)])))]
    (cond
      (map? obj) (let [all-keys (fu/all-keys -meta obj)]
                   (reduce reduce-map {} all-keys))
      (vector? obj) (mapv #(zip-meta-recur idx % pth) obj)
      :else obj)))

(defn zip-meta [idx obj]
  (let [res-nm (keyword (:resourceType obj))
        init-path [res-nm]
        obj (dissoc obj :resourceType)]
    {res-nm (zip-meta-recur idx obj init-path)}))
