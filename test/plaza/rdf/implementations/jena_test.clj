(ns plaza.rdf.implementations.jena-test
  (:use clojure.test
        midje.sweet
        plaza.rdf.core
        plaza.rdf.implementations.common
        plaza.rdf.implementations.jena))

;; we'll test with jena
(init-jena-framework)

(fact "jena-resource"
  (let [model (build-model :jena)
        res (create-resource model "http://test.com/test")
        https-res (create-property model "https://test.com/test")]

    res                =not=> bnode?
    res                =not=> property?
    (to-string res)        => "http://test.com/test"
    res                    => resource?
    (resource-id res)      => "http://test.com/test"
    (qname-prefix res)     => "http://test.com/"
    (qname-local res)      => "test"
    (to-string https-res)  => "https://test.com/test"))

(fact "jena properties"
  (let [model (build-model :jena)
        res (create-property model "http://test.com/test")
        https-res (create-property model "https://test.com/test")]
    (to-string res)       => "http://test.com/test"
    res               =not=> bnode?
    res                   => resource?
    res                   => property?
    (resource-id res)     => "http://test.com/test"
    (qname-prefix res)    => "http://test.com/"
    (qname-local res)     => "test"
    (to-string https-res) => "https://test.com/test"))

(fact "blank nodes"
  (let [model (build-model :jena)
        res (create-blank-node model "a")]
    (to-string res)        => "_:a"
    res                    => bnode?
    res                =not=> resource?
    res                =not=> property?
    (resource-id res)      => "a"
    (qname-prefix res)     => "_"
    (qname-local res)      => "a"))

(deftest test-jena-literal
  (let [model (build-model :jena)
        res (create-literal model "a" "es")]
    (is (= "a@es" (to-string res)))
    (is (not (bnode? res)))
    (is (not (resource? res)))
    (is (not (property? res)))
    (is (literal? res))
    (is (= (resource-id res) "a@es"))
    (is (= (literal-value res) "a"))
    (is (= (literal-language res) "es"))
    (is (= (literal-datatype-uri res) "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))))

(deftest test-jena-typed-literal
  (let [model (build-model :jena)
        res (create-typed-literal model (Integer. 2))]
    (is (= "\"2\"^^<http://www.w3.org/2001/XMLSchema#int>" (to-string res)))
    (is (not (bnode? res)))
    (is (not (resource? res)))
    (is (not (property? res)))
    (is (literal? res))
    (is (= (resource-id res) "\"2\"^^<http://www.w3.org/2001/XMLSchema#int>"))
    (is (= (literal-value res) 2))
    (is (= (literal-language res) ""))
    (is (= (literal-datatype-uri res) "http://www.w3.org/2001/XMLSchema#int"))))

(deftest test-locks-1
  (let [*m* (build-model :jena)
        counter (ref 0)
        sync (promise)
        sync-lock (promise)]
    (future
      (model-critical-write *m*
                            (deliver sync-lock :continue)
                            (dosync
                             (alter counter (fn [x] :a)))
                            (deliver sync :continue)))
    @sync-lock
    (model-critical-write *m*
                          (dosync
                           (is (= @counter :a))
                           (alter counter (fn [x] :b))))
    @sync
    (is (= (dosync @counter) :b))))

(fact "query-string"
  (let [*m* (build-model :jena)
        query-str "SELECT ?s ?p ?o WHERE { ?s ?p ?o .}"]
    (with-model *m* (model-add-triples [[:a :b :c]]))
    (count (query-triples *m* query-str))          => 1
    (count (ffirst (query-triples *m* query-str))) => 3))

(fact "datatype-symbol"
  (datatype-symbol "http://www.w3.org/2001/XMLSchema#string")  => :string
  (datatype-symbol "http://www.w3.org/anything/not/a/atatype") => nil)
