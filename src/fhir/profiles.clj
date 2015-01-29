(ns fhir.profiles
  (:require
    [clojure.string :as cs]
    [fhir.utils :as fu]))

(def resources (fu/read-json "profiles/profiles-resources.json"))
(def types     (fu/read-json "profiles/profiles-types.json"))

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
  (into [] (map-indexed
             (fn [idx e]
               (merge e {:path (get-path (:path e))
                         :type (get-types e)
                         :name (:name e)
                         :nameReference (:nameReference e)
                         :ord idx }))
             els)))

(defn poly-type? [e]
  (re-seq #"\[x]" (name (last (:path e)))))

(defn poly-fixed-path [e tp]
  (let [pth (:path e)
        nm (name (last pth))
        prefix (first (cs/split nm #"\[x]" ))]
    (conj (apply vector (butlast (:path e))) (keyword (str prefix (fu/camelize (name tp)))))))

(def all-datatype-codes
  (mapv
    (fn [x]
      (keyword (get-in x [:resource :snapshot :element 0 :path])))
    (:entry types)))

(defn expand-types [e]
  (let
    [types (:type e)
     types (if (= types [:*])
             all-datatype-codes
             types)]
    (mapv
      (fn [tp] (merge e {:path (poly-fixed-path e tp) :type [tp]}))
      types)))

(defn expand-poly-types [els]
  (reduce (fn [a e]
            (if (poly-type? e)
              (concat a (expand-types e))
              (conj a e)))
          [] els))


(defn make-nested [els]
  (loop [[x & xs] els
         acc {}]
    (if x
      (let [pt  (:path x)
            mt  (merge (select-keys x [:min :max :type :ord :nameReference]) {:path pt})
            acc (update-in acc pt (fn [a]  (merge (or a {}) {:$attrs mt})))]
        (recur xs acc))
      acc)))

(defn mreduce [f m]
  (reduce (fn [a [k v]] (assoc a k (f k v))) {} m))


(def profiles
  (-> (let [prfs (concat
                   (:entry types)
                   (:entry resources))]
        (reduce
          #(assoc %1 (get-res-name %2) (:resource %2))
          {} prfs))
      (dissoc nil)))

(defn find-names [els]
  (reduce
    (fn [acc el]
      (if-let [nm (:name el)]
        (assoc acc nm (:path el))
        acc))
    {}
    els))

(def nameRefs
  (mreduce
    (fn [k v]
      (-> v
          get-elems
          process-elems
          expand-poly-types
          find-names
          ))
    profiles))

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

(fu/TODO "Handle bundles")
(defn find-meta
  "Looking meta information from profiles for path"
  [[y & ys]]
  (let [res-nm y]
    (loop [[x & xs] ys obj (get idx y)]
      (cond
        ;;handle name ref
        (get-in obj [:$attrs :nameReference])
        (let [nm-ref  (get-in obj [:$attrs :nameReference])
              ref-pth (get-in nameRefs [res-nm nm-ref])]
          (find-meta (concat ref-pth [x] (or xs []))))
        ;; found
        (nil? x)  obj
        (and (map? obj) (contains? obj x)) (recur xs (get obj x))
        ;; if no next key look for type and switch to complex type search
        (get-type obj) (find-meta (concat [(get-type obj) x] xs))
        ;; meta information not found
        :else nil))))

(comment
  (find-meta [:Bundle :entry :resource])
  (find-meta [:Resource]))
