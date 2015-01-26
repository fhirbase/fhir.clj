(ns fhir.core
  (:require
    [clojure.set :as cset]
    [fhir.profiles :as fp]))


(defn all-keys [mt obj]
  (disj (cset/union (set (keys mt))
                    (set (keys obj)))
        :$attrs))

(defn- reduce-recur
  "walk resource recursively and collect accumulator by (f acc path value meta-info)"
  [obj pth f acc]
  (let [-meta (fp/find-meta pth)
        acc (f acc pth obj -meta)
        reduce-map #(reduce-recur (get obj %2) (conj pth %2) f %1)
        reduce-vec #(reduce-recur %2 pth f %1)]
    (cond
      (map? obj)    (reduce reduce-map acc (all-keys -meta obj))
      (vector? obj) (reduce reduce-vec acc obj)
      :else acc)))

(defn reduce-resource [obj f]
  (let [init-pth [(keyword (:resourceType obj))]]
    (reduce-recur (dissoc obj :resourceType) init-pth  f [])))


(defn zip-meta-recur [obj pth]
  (let [-meta (fp/find-meta pth)
        -attrs (:$attrs -meta)
        reduce-map (fn [acc k]
                     (assoc acc k (zip-meta-recur (get obj k) (conj pth k))))]
    (cond
      (map? obj) (let [all-keys (all-keys -meta obj)]
                   [-attrs (reduce reduce-map {} all-keys)])
      (vector? obj) [-attrs (mapv #(zip-meta-recur % pth) obj)]
      :else [-attrs obj])))

(defn zip-meta [obj]
  (let [res-nm (keyword (:resourceType obj))
        init-path [res-nm]
        obj (dissoc obj :resourceType)]
    {res-nm (zip-meta-recur obj init-path)}))
