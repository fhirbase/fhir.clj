(ns fhir.profiles
  (:require
    [clojure.string :as cs]
    [fhir.types :as ft]
    [cheshire.core :as json]))

(defn get-path [x]
  (->> (cs/split (:path x) #"\.")
       (mapv keyword)))


(defn read-json [pth]
  (-> (slurp pth)
      (json/parse-string  keyword)))

(def resources (read-json "profiles/profiles-resources.json"))
(def types (read-json "profiles/profiles-types.json"))

(defn get-res-name [entry]
  (keyword (get-in entry [:resource :snapshot :element 0 :path])))

(def profiles
  (-> (let [all (concat (:entry types) (:entry resources))]
        (reduce #(assoc %1 (get-res-name %2) (:resource %2)) {} all))
      (dissoc nil)))

(defn get-elems [prof] (get-in prof [:snapshot :element]))

(defn get-path [s]
  (mapv keyword (cs/split s #"\.")))

(defn get-types [el]
  (mapv #(keyword (:code %)) (:type el)))

(defn process-elems [els]
  (mapv
    (fn [e] (merge e {:path (get-path (:path e)) :type (get-types e)}))
    els))

(defn poly-type? [e]
  (re-seq #"\[x]" (name (last (:path e)))))

(defn cap-first-letter [s]
  (str (cs/upper-case (subs s 0 1)) (subs s 1 (.length s))))

(defn poly-fixed-path [e tp]
  (let [pth (:path e)
        nm (name (last pth))
        prefix (first (cs/split nm #"\[x]" ))]
    (conj (apply vector (butlast (:path e))) (keyword (str prefix (cap-first-letter (name tp)))))))

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


(defn hashify [els]
  (loop [[x & xs] els
         ord 0
         acc {}]
    (if x
      (let [pt (:path x)
            mt (merge (select-keys x [:min :max :type]) {:ord ord :path pt})
            acc (update-in acc pt (fn [a]  (merge (or a {}) {:$attrs mt})))]
        (recur xs (inc ord)  acc))
      acc)))

(defn mreduce [f m]
  (reduce (fn [a [k v]] (assoc a k (f k v))) {} m))

(def idx
  (mreduce
    (fn [k x] (-> (get-elems x)
                process-elems
                expand-poly-types
                hashify
                (get k)))
    profiles))

(comment
  (sort (keys (:Patient idx)))
  (:$attrs (:Address idx))
  (:Patient idx)
  (:code idx))
