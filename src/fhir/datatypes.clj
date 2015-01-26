(ns fhir.datatypes
  (:require
    [cheshire.core :as json]
    [clj-time.format :as cf]
    ))

(import java.math.BigDecimal)
(import org.ietf.jgss.Oid)

(def date-formatters
  (cf/formatter "YYYY-MM-dd"))

(defn parse-datetime [st]
  (cf/parse date-formatters st))

(parse-datetime "2011-01-01")

(def Address
  {:use   {:$attrs {:min 1 :max 1 :type [{:code "code"}]}}
   :text
   :line
   :city
   :state
   :postalCode {:type :code}
   :country {:type :code}
   :period {:type :Period}})

(def types
  {:boolean      {:native true :type  java.lang.Boolean}
   :integer      {:native true :type  java.lang.Integer}
   :decimal      {:native true :type  java.math.BigDecimal}
   :string       {:native true :type  java.lang.String}
   :instant      {:native true :type  java.util.Date}
   :dateTime     {:native true :type  java.util.Date :parse parse-datetime}
   :date         {:native true :type  java.util.Date}
   :uri          {:native true :type  java.net.URI}
   :base64Binary {:native true :type  java.lang.String}
   :code         {:native true :type  java.lang.String}
   :id           {:native true :type  java.lang.String}
   :uuid         {:native true :type  java.util.UUID}
   :oid          {:native true :type  org.ietf.jgss.Oid}
   :Period       {:start {:type :dateTime}
                  :end   {:type :dateTime}}
   :Address    Address
   :HumanName    {:use   {:type :code}
                  :family  {:type :string}
                  :given  {:type :string}}
   :Reference {:display   {:type :string}
               :reference {:type :uri}}
   :Narrative {:status {:type :code}
               :div {:type :string}}
   :Coding   {:system  {:type :string}
              :version  {:type :string}
              :code  {:type :code}
              :display  {:type :string}
              :primary  {:type :boolean}
              :valueSet {:type :Reference}}
   :CodeableConcept   {:coding  {:type :Coding}
                       :display {:type :string}
                       }
   :Identifier   {:use   {:type :code}
                  :value  {:type :string}
                  :label  {:type :string}
                  :assigner {:type :Reference}
                  :period  {:type :Period}
                  :system  {:type :string}}
   :ContactPoint {:use   {:type :code}
                  :value  {:type :string}
                  :period  {:type :Period}
                  :system  {:type :string}}
   })

(defn reduce-map [acc t tp [attr value]]
  (if-let [attr-type (get t attr)]
    (assoc acc attr (parse-as (:type attr-type) value))
    (println "Unexpected key " [tp attr] value)))

(defn parse-as [tp data]
  (let [t (get types tp)]
    (cond
      (:native t)       ((or (:parse t) identity) data)
      (vector? data)    (reduce #(conj %1 (parse-as tp %2)) [] data)
      (map? data)       (reduce #(reduce-map %1 t tp %2) {} data))))
