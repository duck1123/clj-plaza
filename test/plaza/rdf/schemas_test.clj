(ns plaza.rdf.schemas-test
  (:use plaza.rdf.core
        plaza.rdf.implementations.sesame
        plaza.rdf.schemas
        plaza.rdf.sparql
        midje.sweet)
  (:require [clojure.tools.logging :as log]))

(init-sesame-framework)
(use 'plaza.rdf.vocabularies.foaf)
(init-vocabularies)

(defonce ^:dynamic *test-model*
  (make-rdfs-schema ["http://something/" "Good"]
                    :name   {:uri "http://test.com/name"      :range :string}
                    :price  {:uri ["http://test.com/" :price] :range :float}
                    :number {:uri :number                     :range :int}))

(fact "extend-schemas"
  (let [extended (extend-rdfs-schemas "http://test.com/Foo" [*test-model*])]
    (str (type-uri extended)) => "http://test.com/Foo"
    (some #{"http://test.com/Foo"} (map str (super-type-uris extended))) => truthy
    (some #{"http://something/Good"} (map str (super-type-uris extended))) => truthy
    (str (property-uri extended :name)) => "http://test.com/name"))

(fact "props"
  (str (type-uri *test-model*)) => "http://something/Good")

(fact "add-remove-prop"
  (do (let [modelp (-> *test-model*
                       (add-property :wadus "http://test.com/wadus" :float)
                       (add-property :foo "http://test.com/foo" "http://test.com/ranges/foo"))
            modelpp (-> modelp
                        (remove-property-by-uri "http://test.com/foo")
                        (remove-property-by-alias :wadus))]
        (property-alias modelp "http://test.com/foo") => :foo
        (property-alias modelp "http://test.com/wadus") => :wadus
        (property-alias modelpp :wadus) => nil?
        (property-alias modelpp :foo) => nil?)))

(fact "to-map"
  (let [m (to-map *test-model* [[:test ["http://test.com/" :name] "name"]
                                [:test ["http://test.com/" :price] (d 120)]
                                [:test :number (d 10)]])]
    m => {:name (rdf-resource "name") :price (d 120) :number (d 10)}))

(fact "to-pattern"
  (let [p (to-pattern *test-model* [:name :price])]
    (count p) => 4
    (count (filter #(:optional (meta %1)) p)) => 2))

(fact "to-pattern-2"
  (let [p (to-pattern *test-model* "http://test.com/Test" [:name :price])]
    (count p) => 4
    (count (filter #(:optional (meta %1)) p)) => 2
    (doseq [[s _p _o] p]
      (resource-id s) => "http://test.com/Test")))

(fact "property-uri"
  (str (property-uri *test-model* :name)) => "http://test.com/name")

(fact "property-alias"
  (property-alias *test-model* "http://test.com/name") => :name)

(fact "property-parse-value"
  (parse-prop-value *test-model* :number "2") => 2)

(fact "schema->triples"
  (let [ts (to-rdf-triples foaf:Agent-schema)]
    (count ts) => 37))

(fact "parse-from-rdf"
  (let [ts (to-rdf-triples foaf:Agent-schema)
        *m* (build-model)
        _tmp (with-model *m* (model-add-triples ts))
        parsed (parse-rdfs-schemas-from-model *m*)]
    (sort (aliases (first parsed))) =>  (sort (aliases foaf:Agent-schema))))
