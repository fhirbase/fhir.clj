(ns fhir.core
  (:require
    [clojure.set :as cset]
    [fhir.profiles :as fp]))

(defn- get-type [m]
  (get-in m [:$attrs :type 0]))

(defn find-meta
  "Looking meta information from profiles for path"
  [[y & ys]]
  (loop [[x & xs] ys obj (get fp/idx y)]
    (cond
      ;; found
      (nil? x) obj
      (and (map? obj) (contains? obj x)) (recur xs (get obj x))
      ;; if no next key look for type and switch to complex type search
      (get-type obj) (find-meta (concat [(get-type obj) x] xs))
      ;; meta information not found
      :else nil)))

(defn all-keys [mt obj]
  (cset/union (set (keys mt))
              (set (keys obj))))

(defn- walk-resource
  "walk resource recursively and collect accumulator by (f acc path value meta-info)"
  [obj pth f acc]
  (let [-meta (find-meta pth)
        acc (f acc pth obj -meta)
        reduce-map #(walk-resource (get obj %2) (conj pth %2) f %1)
        reduce-vec #(walk-resource %2 pth f %1)]
    (cond
      (map? obj)    (reduce reduce-map acc (all-keys -meta obj))
      (vector? obj) (reduce reduce-vec acc obj)
      :else acc)))

(defn reduce-resource [obj f]
  (let [init-pth [(keyword (:resourceType obj))]]
    (walk-resource (dissoc obj :resourceType) init-pth  f [])))
