(ns plaza.rdf.sparql-test
  (:use [plaza.rdf.core :only [d rdf:type optional opt l make-triples
                               defmodel model-add-triples to-string
                               triple-predicate triple-subject triples-abstraction]]
        plaza.rdf.implementations.jena
        plaza.rdf.predicates
        plaza.rdf.sparql
        plaza.utils
        midje.sweet)
  (:require [clojure.tools.logging :as log])
  (:import plaza.rdf.implementations.jena.JenaSparqlFramework))

;; we'll test with jena
(init-jena-framework)

(fact "framework-sparql->pattern-1"
  (let [framework (JenaSparqlFramework.)
        query "SELECT ?v WHERE { ?v ?p 2 . optional {?v ?q 3 . ?v ?q 4 } }"
        res (parse-sparql->pattern framework query)]
    (count res) => 3
    (count (filter #(:optional (meta %1)) res)) => 2))

(fact "framework-sparql->pattern-2"
  (let [framework (JenaSparqlFramework.)
        res (parse-sparql->pattern framework "SELECT ?v WHERE { ?v ?p 2 . ?v ?q 3 . ?v ?q 4 }")]
    (count res) => 3
    (count (filter #(:optional (meta %1)) res)) => 0))

(fact "sparql->pattern-1"
  (let [res (sparql->pattern "SELECT ?v WHERE { ?v ?p 2 . optional {?v ?q 3 . ?v ?q 4 } }")]
    (count res) => 3
    (count (filter #(:optional (meta %1)) res)) => 2))

(fact "sparql->pattern-2"
  (let [res (sparql->pattern "SELECT ?v WHERE { ?v ?p 2 . ?v ?q 3 . ?v ?q 4 }")]
    (count res) => 3
    (count (filter #(:optional (meta %1)) res)) => 0))

(fact "build-query-framework-1"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        query (parse-sparql->query framework "PREFIX rdf: <http://test.com>
SELECT ?v
WHERE {
  ?v ?p 2 .
  ?v rdf:algo 3 .
  ?v ?q 4
}")
        built-query (build-query framework query)]
    (.isEmpty (first (.getElements (.getQueryPattern built-query)))) => false
    (.getResultVars built-query) => ["v"]
    (.getQueryType built-query) => com.hp.hpl.jena.query.Query/QueryTypeSelect))

(fact "build-query-1"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        pattern (str "PREFIX rdf: <http://test.com> "
                     "SELECT ?v WHERE { ?v ?p 2 .?v rdf:algo 3 . ?v ?q 4  }")
        query (sparql->query pattern)
        built-query (build-query framework query)]
    (.isEmpty (first (.getElements (.getQueryPattern built-query)))) => false
    (.getResultVars built-query) => ["v"]
    (.getQueryType built-query) => com.hp.hpl.jena.query.Query/QueryTypeSelect))

(fact "build-query-2"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        query (parse-sparql->query
               framework
               "PREFIX  dc:   <http://purl.org/dc/elements/1.1/>
PREFIX  a:    <http://www.w3.org/2000/10/annotation-ns#>
PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>

SELECT  ?annot
WHERE {
  ?annot  a:annotates  <http://www.w3.org/TR/rdf-sparql-query/> ;
  dc:date      ?date .
  FILTER (?date < \"2005-01-01T00:00:00Z\"^^xsd:dateTime)
}")]
    (count (:filters query)) => 1
    (count (:pattern query)) => 2))

(fact "build-query-3"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        query (build-query framework
                           (defquery
                             (query-set-vars [:?y])
                             (query-set-type :select)
                             (query-set-pattern
                              (make-pattern [[:?y :?x :?p]]))
                             (query-set-filters [(make-filter :> :?y (d (int 2)))])))
        res (.toString query)]
    res => "SELECT  ?y\nWHERE\n  { ?y  ?x  ?p .\n    FILTER ( ?y > \"2\"^^<http://www.w3.org/2001/XMLSchema#int> )\n  }\n"))

(fact "build-query-4"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        query-str (str (build-query framework
                                    (defquery
                                      (query-set-vars [?a])
                                      (query-set-pattern (make-pattern [[:a :b :c]]))
                                      (query-set-type :select)
                                      (query-set-limit 2)
                                      (query-set-distinct)
                                      (query-set-offset 5)
                                      (query-set-reduced))))]
    query-str  => #"REDUCED"
    query-str  => #"DISTINCT"
    query-str  => #"OFFSET"
    query-str  => #"LIMIT"))

(fact "make-pattern-build-1"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        pattern (make-pattern [[:?x rdf:type :http://test.com/Test]
                               (optional [:?y :?z (d 2)])])
        query (defquery
                (query-set-vars [:?y])
                (query-set-type :select)
                (query-set-pattern pattern))
        query-str (.toString (build-query framework query))
        parsed-pattern (parse-sparql->pattern framework query-str)]
    (count (filter #(:optional (meta %1)) parsed-pattern)) => 1))

(fact "#'pattern-bind"
  (let [s (triple-subject "foo")
        pattern (make-pattern [[:?x rdf:type :http://test.com/Test]])
        binding-map {:?x s}
        res (pattern-bind pattern binding-map)]
    (first (first res)) => s))

(fact "#'pattern-bind*"
  (let [s (triple-subject "foo")
        pattern (make-pattern [[:?x rdf:type :http://test.com/Test]])
        binding-map {:?x s}
        t (first pattern)
        res (pattern-bind* t binding-map)]
    (first res) => s))

(fact "#'pattern-bind"
  (let [s (triple-subject "foo")
        pattern (make-pattern [[:?x rdf:type :http://test.com/Test]])
        binding-map {:?x s}
        res (pattern-bind pattern binding-map)]
    (first (first res)) => s))

(fact "make-pattern-build-2"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        pattern (make-pattern [[:?x rdf:type :http://test.com/Test]
                               (opt [:?y :?z (d 2)]
                                    [:?l :?m (l "cat")])])
        query (defquery
                (query-set-vars [:?y])
                (query-set-type :select)
                (query-set-pattern pattern))
        query-str (.toString (build-query *sparql-framework* query))
        parsed-pattern (parse-sparql->pattern framework query-str)]
    (count (filter #(:optional (meta %1)) parsed-pattern)) => 2))

(fact "abstraction-1"
  (let [triples (make-triples [[:a :b :c]])
        pattern (make-pattern [[:?x :b :c]])]
    (triples-abstraction triples
                         (subject?
                          (matches-uri? "http://plaza.org/ontologies/a"))
                         {:subject :?x}) => pattern))

(fact "build-filters-2"
  (let [framework (plaza.rdf.implementations.jena.JenaSparqlFramework.)
        gt (.toString (build-filter framework (make-filter :> :?x (d (int 3)))))
        gt-2 (.toString (build-filter framework (make-filter :> :?x (make-filter :bound :?y))))]
    gt => "( ?x > \"3\"^^xsd:int )"
    gt-2 => "( ?x > bound(?y) )"))

(fact "build-filters-3"
  (let [*tq* "PREFIX  dc:   <http://purl.org/dc/elements/1.1/>
PREFIX  a:    <http://www.w3.org/2000/10/annotation-ns#>
PREFIX  xsd:  <http://www.w3.org/2001/XMLSchema#>

SELECT  ?annot
WHERE {
  ?annot  a:annotates  <http://www.w3.org/TR/rdf-sparql-query/> ;
  dc:date      ?date .
  FILTER (bound(?date) < \"2005-01-01T00:00:00Z\"^^xsd:dateTime)
}"
        res (->> (parse-sparql->query *sparql-framework* *tq*)
                 :filters
                 first
                 (build-filter *sparql-framework*)
                 .toString)]
    (is (= res "( bound(?date) < \"2005-01-01T00:00:00Z\"^^xsd:dateTime )"))))

(fact "go-back-query"
  (let [query (defquery
                (query-set-type :select)
                (query-set-vars [:?x])
                (query-set-pattern
                 (make-pattern [[:?x "a" (d 2)]])))

        triples (make-triples [[:m :a (d 2)]
                               [:n :b (d 2)]
                               [:o :a (d 2)]
                               [:p :a (d 3)]])

        model (defmodel
                (model-add-triples triples))

        results (model-query-triples model query)]
    (count results) => 2))

(fact "collect-vars"
  (set (pattern-collect-vars (make-pattern [[:?a :?b (l "test")] [:a :?b (d 2)]]))) => #{:?a :?b}
  (set (pattern-collect-vars (make-pattern [[?s ?p ?o]]))) => #{:?s :?p :?o})

(fact "has-meta-pattern"
  (:pattern (meta (make-pattern [[?a ?b ?c]]))) => true)

(fact "model-pattern-apply-checks-meta"
  (let [m (defmodel (model-add-triples [[:a :b :c] [:a :b :e]]))
        result (model-pattern-apply m [[?s ?p :c]])]
    (count result) => 1
    (count (first result)) => 1
    (nth (first (first result)) 2) =not=> keyword?))

(fact "model-pattern-apply-non-free-vars"
  (let [m (defmodel (model-add-triples [[:a :b :c] [:a :b :e]]))
        result (model-pattern-apply m [[:a :b :c]])]
    (count result) => 1
    (count (first result)) => 1
    (nth (first (first result)) 2) =not=> keyword?))

(fact "pattern-pply"
  (let [res (pattern-apply [[:ba rdf:type :Post]]
                           [[?a rdf:type :Post]])
        [s p o] (first (first res))]
    (to-string s) => #"ba"
    (to-string p) => #"type"
    (to-string o) => #"Post"))

(fact "model-pattern-apply-limit-offset"
  (let [m (defmodel (model-add-triples [[:a :b :c] [:a :b :e] [:a :b :f]]))
        result (flatten-1 (model-pattern-apply m [[:a :b ?p]] (f :limit 2) (f :offset 0)))
        result2 (flatten-1 (model-pattern-apply m [[:a :b ?p]] (f :limit 2) (f :offset 2)))]
    (count result) => 2
    (count result2) => 1))
