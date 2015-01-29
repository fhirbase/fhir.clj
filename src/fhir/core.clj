(ns fhir.core
  (:require
    [clojure.set :as cset]
    [fhir.utils :as fu]
    [fhir.profiles :as fp]))

(defn- reduce-recur
  "walk resource recursively and collect accumulator by (f acc path value meta-info)"
  [obj pth f acc]
  (let [-meta (fp/find-meta pth)
        acc (f acc pth obj -meta)
        reduce-map #(reduce-recur (get obj %2) (conj pth %2) f %1)
        reduce-vec #(reduce-recur %2 pth f %1)]
    (cond
      (map? obj)    (reduce reduce-map acc (fu/all-keys -meta obj))
      (vector? obj) (reduce reduce-vec acc obj)
      :else acc)))

(defn reduce-resource [obj f]
  (let [init-pth [(keyword (:resourceType obj))]]
    (reduce-recur (dissoc obj :resourceType) init-pth  f [])))

(defn zip-meta-recur [obj pth]
  (let [-meta (fp/find-meta pth)
        -attrs (:$attrs -meta)
        reduce-map (fn [acc k]
                     (let [epth (conj pth k)
                           v (get obj k)
                           emeta (fp/find-meta epth)]
                       (assoc acc k [emeta (zip-meta-recur v epth)])))]
    (cond
      (map? obj) (let [all-keys (fu/all-keys -meta obj)]
                   (reduce reduce-map {} all-keys))
      (vector? obj) (mapv #(zip-meta-recur % pth) obj)
      :else obj)))

(defn zip-meta [obj]
  (let [res-nm (keyword (:resourceType obj))
        init-path [res-nm]
        obj (dissoc obj :resourceType)]
    {res-nm (zip-meta-recur obj init-path)}))
