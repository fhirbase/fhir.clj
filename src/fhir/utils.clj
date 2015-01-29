(ns fhir.utils
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]
    [cheshire.core :as json]))

(defn read-json [pth]
  (-> (slurp pth)
      (json/parse-string  keyword)))

(defn camelize [^String s]
  (str (cs/upper-case (subs s 0 1)) (subs s 1 (.length s))))

(def todos (atom #{}))

(defmacro TODO [s]
  `(swap! todos conj {:ns *ns* :text ~s :line ~(:line (meta &form))}))

(defn normalize-string [s]
  (cs/replace s #"\s" ""))

(defn mapmap
  "just map hashmap into hashmap (assoc acc k (f k v))"
  [f m]
  (reduce
    (fn [acc [k v]]
      (assoc acc k (f k v)))
    {} m))

(comment "example"
         (= (mapmap str {:a 1 :b 2})
            {:a "a1" :b "b2"}))

(defn resources-diff
  ([a b] (resources-diff a b []))
  ([a b pth]
   (when (not= a b)
     (cond
       ;; maps
       (every? map? [a b])
       (let [ks (cset/union (set (keys a)) (set (keys b)))]
         (doseq [k ks] (resources-diff (get a k) (get b k) (conj pth k))))

       ;; strings
       (and (every? string? [a b])
            (not= (normalize-string a) (normalize-string b)))
       (do
         (println "\nSTRING DIFF:" pth)
         (println "\t" (pr-str a))
         (println "\t" (pr-str b)))

       ;; vectors
       (every? vector? [a b])
       (if (= (count a) (count b))
         (doseq [[aa bb] (map list a b)] (resources-diff aa bb pth))
         (do
           (println "\nDIFF VECTOR:" pth "; counts " (count a) (count b))
           (println "\tin first" (pr-str (cset/difference (set a) (set b))))
           (println "\tin second" (pr-str (cset/difference (set b) (set a))))))

       ;; primitives
       :else (do
               (println "\nDIFF:" pth)
               (println "\t" (pr-str a))
               (println "\t" (pr-str b)))))))
