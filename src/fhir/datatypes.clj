(ns fhir.datatypes)
(import java.math.BigDecimal)

(type true)
(println Boolean)

(def primitives
  {:boolean java.lang.Boolean
   :integer java.lang.Integer
   :decimal java.math.BigDecimal
   :instant java.util.Date
   :address
   [{:path [:use]  :type :code :min 0 :max 1000}
    {:path [:line] :type :code :min 0 :max 1000}
    {:path [:city] :type :code :min 0 :max 1}
    {:path [:period] :type :period :min 0 :max 1}]})



(comment
  (comment
    (def-type Address
      (code :use ["home"])
      (string :line {:min 0 :many true})
      (string :city)
      (enum :state)
      (code :counttry)
      (filed :period Period)))


  (address {:use  "home"
            :line ["abc"]
            :city "NY"
            :period {:start #inst"2005-01-01"}})


  (comment
    => VALIDATION ERRORS/WARNINGS
    => element does not exists
    => wrong arity i.e. [] for vector
    => check enums
    => optional check value sets

    )

  (conversion
    (to-xml)
    (to-json)
    (parse-xml)
    (parse-json))

  )
