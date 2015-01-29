(ns fhir.utils
  (:require
    [clojure.string :as cs]
    [cheshire.core :as json]))

(defn read-json [pth]
  (-> (slurp pth)
      (json/parse-string  keyword)))

(defn camelize [^String s]
  (str (cs/upper-case (subs s 0 1)) (subs s 1 (.length s))))

(def todos (atom #{}))

(defmacro TODO [s]
  `(swap! todos conj {:ns *ns* :text ~s :line ~(:line (meta &form))}))
