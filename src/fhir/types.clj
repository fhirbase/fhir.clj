(ns fhir.types
  (:require
    [cheshire.core :as json]
    [clj-time.format :as cf]))

(import java.math.BigDecimal)
(import org.ietf.jgss.Oid)

(def Period
  {:resourceType :Period
   :snapshot
   {:element [{:path "Period" :min 1 :max "1"}
              {:path "Period.end" :min 0 :max "1" :type [{:code "dateTime"}]}
              {:path "Period.start" :min 0 :max "1" :type [{:code "dateTime"}]}]}})
(def Address
  {:resourceType :Address
   :snapshot
   {:element [{:path "Address" :min 1 :max "1"}
              {:path "Address.use" :min 0 :max "1" :type [{:code "string"}]}
              {:path "Address.line" :min 0 :max "*" :type [{:code "string"}]}
              ]}})
(def types
  {:boolean      {:native true :type  java.lang.Boolean}
   :integer      {:native true :type  java.lang.Integer}
   :decimal      {:native true :type  java.math.BigDecimal}
   :string       {:native true :type  java.lang.String}
   :instant      {:native true :type  java.util.Date}
   :dateTime     {:native true :type  java.util.Date}
   :date         {:native true :type  java.util.Date}
   :uri          {:native true :type  java.net.URI}
   :base64Binary {:native true :type  java.lang.String}
   :code         {:native true :type  java.lang.String}
   :id           {:native true :type  java.lang.String}
   :uuid         {:native true :type  java.util.UUID}
   :oid          {:native true :type  org.ietf.jgss.Oid}
   :Period       Period
   :Address      Address
   })
