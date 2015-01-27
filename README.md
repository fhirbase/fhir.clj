# fhir.clj

[![Build Status](https://travis-ci.org/fhirbase/fhir.clj.svg)](https://travis-ci.org/fhirbase/fhir.clj) [![Dependency Status](https://www.versioneye.com/user/projects/54c823486c63105469000026/badge.svg?style=flat)](https://www.versioneye.com/user/projects/54c823486c63105469000026)

[![Clojars Project](http://clojars.org/fhir/latest-version.svg)](http://clojars.org/fhir)


FHIR client implementation in clojure.

## Roadmap

* parse & serialize FHIR formats
* validate resources against profiles
* datatypes coersing (clj-time, ooid)
* rest client api
* support oauth

## Usage

```clj

(require '[fhir.core :as fhir])

(def pt
  (fhir/parse "
   {\"resourceType\": \"Patient\",
    \"name\": [{\"text\":\"Smith\"}],
    \"active\": true}
  "))

(fhir/validate pt)
;;=> [errors]

(fhir/serialize pt :xml)
;;=> <Patient> <name><text value="Smith"/></name><active value="true"></Patient>

(fhir/resource {:resourceType "Patient" :name {:text "Smith" :family "Eric"}})
;;=> {:resourceType "Patient" :name [{:text "Smith" :family ["Eric"]}]}

```

## Notes

fhir.clj is not very strict about arity of attributes,
so you can assign single value where collection is expected
and this will be fixed using metadata from profile.

## License

Copyright © 2014 HealthSamurai

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
