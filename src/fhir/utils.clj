(ns fhir.utils
  (:require
    [clojure.string :as cs]
    [clojure.set :as cset]
    [clojure.data.xml :as xml]
    [cheshire.core :as json]))

(def todos (atom #{}))
(defmacro TODO [s]
  `(swap! todos conj {:ns *ns* :text ~s :line ~(:line (meta &form))}))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; json

(defn read-json [pth]
  (-> (slurp pth)
      (json/parse-string  keyword)))

(defn to-json [m]
  (json/generate-string m))

(defn from-json [s]
  (json/parse-string s keyword))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; string

(defn camelize [^String s]
  (str (cs/upper-case (subs s 0 1)) (subs s 1 (.length s))))

(defn unencode-html-entities [s]
  (-> (cs/replace s #"&nbsp;" "&#160;")
      (cs/replace #"&(trade|copy|sect|reg);" "$1")))


(defn normalize-string [s]
  (cs/replace s #"\s" ""))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; xml


;; hack to emit element without xml header
(defn emit-xml [e & {:as opts}]
  (let  [^java.io.StringWriter sw (java.io.StringWriter.)
         ^javax.xml.stream.XMLStreamWriter writer  (-> (javax.xml.stream.XMLOutputFactory/newInstance)
                                                       (.createXMLStreamWriter sw))]
    (when  (instance? java.io.OutputStreamWriter sw)
      (xml/check-stream-encoding sw  (or  (:encoding opts) "UTF-8")))
    (doseq  [event  (xml/flatten-elements  [e])]
      (xml/emit-event event writer))
    (.writeEndDocument writer)
    (.toString sw)))

(defn parse-xml [s]
  (-> (unencode-html-entities s)
      (xml/parse-str)))

(defn emit-html [xml]
  (emit-xml
    (assoc-in xml [:attrs :xmlns]
              "http://www.w3.org/1999/xhtml")))

(TODO "too much trnasformations")
(defn normalize-xml-str [s]
  (when s
    (-> (parse-xml s)
        (emit-html))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; collections

(defn mapmap
  "just map hashmap into hashmap (assoc acc k (f k v))"
  [f m]
  (reduce
    (fn [acc [k v]]
      (assoc acc k (f k v)))
    {} m))

(defn all-keys [& xs]
  (-> (set (mapcat keys xs))
      (disj :$attrs)))

(comment "example"
         (= (mapmap str {:a 1 :b 2})
            {:a "a1" :b "b2"}))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; debug

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
