(ns fhir.integration-test
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [clojure.data :as cd]
            [clojure.string :as cstr]
            [clojure.set :as cs]
            [fhir.format :as ff]))


(defn normalize-string [s]
  (cstr/replace s #"\s" ""))

(defn diffo
  ([a b] (diffo a b []))
  ([a b pth]
   (when (not= a b)
     (cond
       ;; maps
       (every? map? [a b])
       (let [ks (cs/union (set (keys a)) (set (keys b)))]
         (doseq [k ks] (diffo (get a k) (get b k) (conj pth k))))

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
         (doseq [[aa bb] (map list a b)] (diffo aa bb pth))
         (do
           (println "\nDIFF VECTOR:" pth "; counts " (count a) (count b))
           (println "\tin first" (pr-str (cs/difference (set a) (set b))))
           (println "\tin second" (pr-str (cs/difference (set b) (set a))))))

       ;; primitives
       :else (do
               (println "\nDIFF:" pth)
               (println "\t" (pr-str a))
               (println "\t" (pr-str b)))))))

(deftest problems-with-name-refs
  (is (=
       (dissoc (ff/from-json (slurp "test/fixtures/json/dateTime.profile.json")) :text)
       (dissoc (ff/from-xml (slurp "test/fixtures/xml/dateTime.profile.xml")) :text))))

;; TODO turn into tests
(doseq  [f (take 100 (fs/glob "test/fixtures/xml/*.xml"))]
  (let [json-file (str "test/fixtures/json/" (fs/base-name f ".xml") ".json")]
    (if (fs/exists? json-file)
      (let [from-xml (ff/from-xml (slurp (.getAbsolutePath f)))
            from-json (ff/from-json (slurp json-file))]
        (println json-file)
        (diffo
          (dissoc from-xml :text)
          (dissoc from-json :text))))))
