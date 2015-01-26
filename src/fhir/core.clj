(ns fhir.core
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]
    [fhir.types :as ft]
    [cheshire.core :as json]
    [fhir.profiles :as fp]))

(defn find-meta
  "Looking meta information from profiles for path"
  [pth]
  (let [obj (get fp/idx (first pth))
        pth (rest pth)]
    (loop [[x & xs] pth obj obj]
      (if (nil? x) obj
        (if (and (map? obj) (contains? obj x))
          (recur xs (get obj x))
          (if-let [tp (get-in obj [:$attrs :type 0])]
            (find-meta (concat [tp x] xs))
            nil))))))

(defn all-keys [mt obj]
  (cset/union (set (keys mt)) (set (keys obj))))

(defn walk-resource
  "walk resource recursively and collect accumulator by f"
  [obj pth f acc]
  (let [-meta (find-meta pth)
        accc (f acc pth obj -meta)]
    (cond
      (map? obj)  (reduce
                    (fn [a k] (walk-resource (get obj k) (conj pth k) f a))
                    accc
                    (all-keys -meta obj))
      (vector? obj) (reduce (fn [a v] (walk-resource v pth f a)) accc obj)
      :else accc)))

(defn reduce-resource [obj f]
  (walk-resource obj [(keyword (:resourceType obj))] f []))


(comment
  (def pt (fp/read-json "examples/pt.json"))

  (reduce-resource pt (fn [acc pth v mt]
                        (cond (nil? mt) (conj acc (str "Unexpected element: " pth " value " v))
                              (and (nil? v) (= (get-in mt [:$attrs :min]) 1))  (conj acc (str "Missed key " pth))
                              :else  acc)))

  (println "meta:" (find-meta [:Patient :address :use]) "End")
  (println "meta:" (find-meta [:Patient :deceasedBoolean]) "End"))
