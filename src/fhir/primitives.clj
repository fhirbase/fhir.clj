(ns fhir.primitives
  (:require
    [cheshire.core :as json]
    [clj-time.format :as cf]))

(import java.math.BigDecimal)
(import org.ietf.jgss.Oid)

(def date-formatters
  (cf/formatter "YYYY-MM-dd"))

(defn parse-datetime [st]
  (cf/parse date-formatters st))

(parse-datetime "2011-01-01")


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
   :oid          {:native true :type  org.ietf.jgss.Oid}})
