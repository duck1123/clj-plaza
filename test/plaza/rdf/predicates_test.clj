(ns plaza.rdf.predicates-test
  (:use clojure.test
        midje.sweet
        [plaza.rdf.core :only [l make-triples
                               d triple-subject
                               rdf:Property
                               rdfs:Class b]]
        plaza.rdf.implementations.jena
        plaza.rdf.predicates)
  (:require [plaza.rdf.core :as rdf]))

(init-jena-framework)

(fact "check"
  (triple-check-apply (is-literal?) (l "test")) =not=> empty?)

(fact "filters-1"
  (let [tps (make-triples [[:a :b (d 2)] [:d :e (l "hola")] [:g [:h :i :j :k]]])
        result (filter (triple-or?
                        (not? (object-and? (is-literal?)))
                        (object-and? (is-literal?)))
                       tps)]
    (count result) => 4))

(fact "filters-2"
  (let [tps (make-triples [[:a :b (d 2)] [:d :e (l "hola")] [:g [:h :i :j :k]]])
        result (filter (triple-and?
                        (not? (object-and? (is-literal?)))
                        (object-and? (is-literal?)))
                       tps)]
    (count result) => 0))

(fact "filters-3"
  (let [tps (make-triples [[:a :b (d 2)]
                           [:d :e (l "hola")]
                           [:g [:h :i :j :k]]])
        result (filter (triple-check (not? (object-and? (is-literal?))))
                       tps)]
    (count result) => 2))

(fact "filters-4"
  (let [tps (make-triples [[:a :b (d 2)]
                           [:d :e (l "hola")]
                           [:g [:h :i :j :k]]])
        result (filter (triple-check (not? (object? (is-literal?)))) tps)]
    (count result) => 2))

(fact "filters-5"
  (let [tps (make-triples [[:a :b (d 2)]
                           [:d :e (l "hola")]
                           [:g [:h :i :j :k]]])
        result (filter (triple-check (object? (is-resource?))) tps)]
    (count result) => 2))

(fact "filters-6"
  (let [tps (make-triples [[:a :b (d 2)]
                           [:d :e (l "hola")]
                           [:g [:h :i :j :k]]])
        result-1 (filter (triple-check (object? (matches-literal? "hola"))) tps)
        result-2 (filter (triple-check (object? (matches-literal? "adios"))) tps)]
    (count result-1) => 1
    (count result-2) => 0))

(fact "filters-7"
  (let [tps (make-triples [[:a :b (d 2)] [:d :e (l "hola" "es")] [:g [:h :i :j :k]]])
        result-1 (filter (triple-check (object? (matches-literal? "hola" "es")))
                         tps)
        result-2 (filter (triple-check (object? (matches-literal? "hola" "en")))
                         tps)]
    (count result-1) => 1
    (count result-2) => 0))

(fact "filters-8"
  (triple-check-apply (matches-qname-prefix? :rdf) (triple-subject rdf:Property)) => truthy
  (triple-check-apply (matches-qname-prefix? :rdf) (triple-subject rdfs:Class)) =not=> truthy)

(fact "filters-9"
  (let [tps (make-triples [[(b :a) :p (b) ] [:d :e (l "hola" "es")]])
        result-1 (filter (triple-check (subject? (matches-bnode? :a)))
                         tps)
        result-2 (filter (triple-check (object? (is-bnode?)))
                         tps)]
    (count result-1) => 1
    (count result-2) => 1))

(deftest test-tc-1
  (let [tps (make-triples [[(b :a) :p (b) ] [:d :e (l "hola" "es")]])
        result-1 (filter (tc (subject? (matches-bnode? :a)))
                         tps)
        result-2 (filter (tc (object? (is-bnode?)))
                         tps)]
    (is (= 1 (count result-1)))
    (is (= 1 (count result-2)))))


(deftest test-predicate-2
  (init-jena-framework)
  (is (triple-check-apply (has-datatype? :int) (d (Integer. 1))))
  (is (not (triple-check-apply (has-datatype? :int) (d 2.0))))
  (is (triple-check-apply (has-datatype? "http://www.w3.org/2001/XMLSchema#int") (d (Integer. 1)))))

(fact "predicate-3"
  (triple-check-apply (literal-fn? (fn [l] true )) (l "cat")) => true
  (triple-check-apply (literal-fn? (fn [l] false)) (l "cat")) => false)
