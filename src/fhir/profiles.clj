(ns fhir.profiles
  (:require
    [clojure.string :as cs]
    [fhir.utils :as fu]))

(defn get-path [x]
  (->> (cs/split (:path x) #"\.")
       (mapv keyword)))


(defn get-res-name [entry]
  (keyword (get-in entry [:resource :snapshot :element 0 :path])))


(defn get-elems [prof] (get-in prof [:snapshot :element]))

(defn get-path [s]
  (mapv keyword (cs/split s #"\.")))

(defn get-types [el]
  (mapv #(keyword (:code %)) (:type el)))

(defn process-elems [els]
  (mapv
    (fn [e]
      (merge e {:path (get-path (:path e))
                :type (get-types e)}))
    els))

(defn poly-type? [e]
  (re-seq #"\[x]" (name (last (:path e)))))

(defn poly-fixed-path [e tp]
  (let [pth (:path e)
        nm (name (last pth))
        prefix (first (cs/split nm #"\[x]" ))]
    (conj (apply vector (butlast (:path e))) (keyword (str prefix (fu/camelize (name tp)))))))

(defn expand-types [e]
  (mapv
    (fn [tp] (merge e {:path (poly-fixed-path e tp) :type [tp]}))
    (:type e)))

(defn expand-poly-types [els]
  (reduce (fn [a e]
            (if (poly-type? e)
              (concat a (expand-types e))
              (conj a e)))
          [] els))


(defn make-nested [els]
  (loop [[x & xs] els
         ord 0
         acc {}]
    (if x
      (let [pt  (:path x)
            mt  (merge (select-keys x [:min :max :type]) {:ord ord :path pt})
            acc (update-in acc pt (fn [a]  (merge (or a {}) {:$attrs mt})))]
        (recur xs (inc ord)  acc))
      acc)))

(defn mreduce [f m]
  (reduce (fn [a [k v]] (assoc a k (f k v))) {} m))

(def resources (fu/read-json "profiles/profiles-resources.json"))
(def types     (fu/read-json "profiles/profiles-types.json"))

(def profiles
  (-> (let [prfs (concat
                   (:entry types)
                   (:entry resources))]
        (reduce
          #(assoc %1 (get-res-name %2) (:resource %2))
          {} prfs))
      (dissoc nil)))

(def idx
  (mreduce
    (fn [k v]
      (-> v
          get-elems
          process-elems
          expand-poly-types
          make-nested
          (get k)))
    profiles))

(defn- get-type [m]
  (get-in m [:$attrs :type 0]))

(defn find-meta
  "Looking meta information from profiles for path"
  [[y & ys]]
  (loop [[x & xs] ys obj (get idx y)]
    (cond
      ;; found
      (nil? x) obj
      (and (map? obj) (contains? obj x)) (recur xs (get obj x))
      ;; if no next key look for type and switch to complex type search
      (get-type obj) (find-meta (concat [(get-type obj) x] xs))
      ;; meta information not found
      :else nil)))
