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

(defn expand-types [types-list e]
  (let
    [types (:type e)
     types (if (= types [:*])
             types-list
             types)]
    (mapv
      (fn [tp] (merge e {:path (poly-fixed-path e tp) :type [tp]}))
      types)))

(defn expand-poly-types [types-list els]
  (reduce (fn [a e]
            (if (poly-type? e)
              (concat a (expand-types types-list e))
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
      (first (vals acc)))))

(defn mreduce [f m]
  (reduce (fn [a [k v]] (assoc a k (f k v))) {} m))

(defn find-names [els]
  (reduce
    (fn [acc el]
      (if-let [nm (:name el)]
        (assoc acc nm (:path el))
        acc))
    {} els))

(defn concat-prfs [profiles]
  (mapcat :entry profiles))

(defn dd [xs] (println xs) xs)

(defn index? [x]
  (= (set (keys x)) #{:idx :nameRefs :types}))

(defn index-profiles [& profiles]
  (let [prfs (-> (reduce #(assoc %1 (get-res-name %2) (:resource %2)) {} (concat-prfs profiles))
                 (dissoc nil))
        ;;TODO get types from metadata
        types-list    [:integer :dateTime :code :date :decimal :uri :id :base64Binary :time :oid :string :boolean :uuid :instant :Period :Coding :Range :Quantity :Attachment :Ratio
                       :SampledData :Reference :CodeableConcept :Identifier :Extension :BackboneElement :Element :Narrative :ElementDefinition :Timing :Address :HumanName :ContactPoint :Quantity :Quantity :Quantity :Quantity :Quantity]]
    {:idx      (mreduce
                 (fn [k v]
                   (->> v get-elems process-elems (expand-poly-types types-list) make-nested))
                 prfs)

     :nameRefs (mreduce
                 (fn [k v]
                   (->> v get-elems process-elems (expand-poly-types types-list) find-names))
                 prfs)
     :types types-list}))


(defn- get-type [m]
  (get-in m [:$attrs :type 0]))

(fu/TODO "Handle bundles")
(defn find-meta
  "Looking meta information from profiles for path
  firt parameter is index built with (index-profiles profiles)"
  [idx [y & ys]]
  (let [res-nm y]
    (loop [[x & xs] ys obj (get-in idx [:idx y])]
      (cond
        ;;handle name ref
        (get-in obj [:$attrs :nameReference])
        (let [nm-ref  (get-in obj [:$attrs :nameReference])
              ref-pth (get-in idx [:nameRefs res-nm nm-ref])]
          (find-meta idx (concat ref-pth [x] (or xs []))))
        ;; found
        (nil? x)  obj
        (and (map? obj) (contains? obj x)) (recur xs (get obj x))
        ;; if no next key look for type and switch to complex type search
        (get-type obj) (find-meta idx (concat [(get-type obj) x] xs))
        ;; meta information not found
        :else nil))))

(comment
  (def fidx
    (index-profiles
      (fu/read-json "profiles/profiles-resources.json")
      (fu/read-json "profiles/profiles-types.json")))
  (require '[clojure.tools.namespace.repl :as nst])
  (nst/refresh)

  (find-meta fidx [:Bundle :entry :resource])
  (find-meta fidx [:Patient :name]))
