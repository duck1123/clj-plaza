(ns plaza.rdf.core-test-sesame
  (:use plaza.rdf.core
        plaza.rdf.implementations.sesame
        midje.sweet))

;; rdf/xml used in the tests
(def ^:dynamic *test-xml*
  "<rdf:RDF
    xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
    xmlns:test=\"http://plaza.org/ontologies/\" >
  <rdf:Description rdf:about=\"http://plaza.org/ontologies/a\">
    <test:c rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\">3</test:c>
    <test:b rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\">2</test:b>
  </rdf:Description>
  <rdf:Description rdf:about=\"http://plaza.org/ontologies/d\">
    <test:e rdf:datatype=\"http://www.w3.org/2001/XMLSchema#int\">3</test:e>
  </rdf:Description>
</rdf:RDF>")

(def ^:dynamic *test-xml-blanks*
  "<?xml version=\"1.0\"?>
<rdf:RDF xmlns:csf=\"http://schemas.microsoft.com/connectedservices/pm#\" xmlns:owl=\"http://www.w3.org/2002/07/owl#\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:rdfs=\"http://www.w3.org/2000/01/rdf-schema#\">
    <rdf:Description rdf:about=\"urn:upn_abc\">
        <csf:Phone>
            <rdf:Description>
                <csf:Phone-Home-Primary>425-555-0111</csf:Phone-Home-Primary>
                <csf:Phone-Mobile-Other>425-555-0114</csf:Phone-Mobile-Other>
                <csf:Phone-Office-Other>425-555-0115</csf:Phone-Office-Other>
              </rdf:Description>
        </csf:Phone>
     </rdf:Description>
</rdf:RDF>")

;; we'll test with Sesame
(init-sesame-framework)

(fact "create-model-sesame"
  (build-model :sesame) => model?)


(fact "with-rdf-ns-sesame"
  (let [before *rdf-ns*
        new-ns "hello"
        result (with-rdf-ns new-ns
                 *rdf-ns*)]
    result => new-ns
    before => *rdf-ns*))

(fact "with-model-sesame"
  (let [before-ns *rdf-ns*
        before-model *rdf-model*
        new-ns "hello"
        new-model "bye"
        result (with-rdf-ns new-ns
                 (with-model new-model
                   [*rdf-ns* *rdf-model*]))]
    result       => [new-ns new-model]
    before-ns    => *rdf-ns*
    before-model => *rdf-model*))

(fact "make-property-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (rdf-property rdf :hola))
        p2 (with-model m
             (rdf-property rdf:type))]
    (to-string p1) => "http://www.w3.org/1999/02/22-rdf-syntax-ns#hola"
    (to-string p2) => "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))

(fact "make-resource-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (rdf-resource rdf :Mundo))
        p2 (with-model m
             (rdf-property rdfs:Class))]
    (to-string p1) => "http://www.w3.org/1999/02/22-rdf-syntax-ns#Mundo"
    (to-string p2) => "http://www.w3.org/2000/01/rdf-schema#Class"))


(fact "make-literal-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (rdf-literal "test"))
        p2 (with-model m
             (rdf-literal "test" "es"))]
    (to-string p1) => "test"
    (to-string p2) => "test@es"))

(fact "make-typed-literal-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (rdf-typed-literal (int 2)))
        p2 (with-model m
             (rdf-typed-literal 2 :anyuri))]
    (to-string p1) => "\"2\"^^<http://www.w3.org/2001/XMLSchema#int>"
    (to-string p2) => "\"2\"^^<http://www.w3.org/2001/XMLSchema#anyURI>"))

(fact "triple-subject-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-subject :A)))
        p2 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-subject [rdf :A])))]
    (to-string p1) => "http://test.com/A"
    (to-string p2) => "http://www.w3.org/1999/02/22-rdf-syntax-ns#A"))

(fact "triple-predicate-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-predicate :p)))
        p2 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-subject [rdf :p])))]
    (to-string p1) => "http://test.com/p"
    (to-string p2) => "http://www.w3.org/1999/02/22-rdf-syntax-ns#p"))

(fact "triple-object-sesame"
  (let [m (build-model :sesame)
        p1 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-object :p)))
        p2 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-object [rdf :p])))
        p3 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-object (l "test"))))
        p4 (with-model m
             (with-rdf-ns "http://test.com/"
               (triple-object (d (int 2)))))]
    (to-string p1) => "http://test.com/p"
    (to-string p2) => "http://www.w3.org/1999/02/22-rdf-syntax-ns#p"
    (to-string p3) => "test"
    (to-string p4) => "\"2\"^^<http://www.w3.org/2001/XMLSchema#int>"))

(fact "rdf-triple-a-sesame"
  (let [m (build-model :sesame)
        ts (with-model m
             (with-rdf-ns "http://test.com/"
               (rdf-triple [:a :b :c])))]
    (count ts)             => 3
    (to-string (nth ts 0)) => "http://test.com/a"
    (to-string (nth ts 1)) => "http://test.com/b"
    (to-string (nth ts 2)) => "http://test.com/c"))

(fact "rdf-triple-b-sesame"
  (let [m (build-model :sesame)
        ts (with-model m
             (with-rdf-ns "http://test.com/"
               (rdf-triple [:a  [:b :c
                                 :d :e]])))]
    (count ts) => 2

    (let [fts (nth ts 0)
          sts (nth ts 1)]
      (to-string (nth fts 0)) => "http://test.com/a"
      (to-string (nth fts 1)) => "http://test.com/b"
      (to-string (nth fts 2)) => "http://test.com/c"
      (to-string (nth sts 0)) => "http://test.com/a"
      (to-string (nth sts 1)) => "http://test.com/d"
      (to-string (nth sts 2)) => "http://test.com/e")))

(fact "add-triples-sesame"
  (let [m (build-model :sesame)]
    (with-model m (model-add-triples [[:a :b :c] [:d :e :f] [:g [:h :i :j :k]]]))
    (count (walk-triples m (fn [s p o] [s p o]))) => 4))

(fact "add-triples-2-sesame"
  (let [m (build-model :sesame)]
    (with-model m (model-add-triples (make-triples [[:a :b :c] [:d :e :f] [:g [:h :i :j :k]]])))
    (count (walk-triples m (fn [s p o] [s p o]))) => 4))

(fact "remove-triples-1-sesame"
  (let [m (defmodel
             (model-add-triples (make-triples [[:a :b (d 2)]]))
             (model-add-triples (make-triples [[:e :f (l "test")]])))]
    (with-model m (model-remove-triples (make-triples [[:a :b (d 2)]])))
    (count (model->triples m)) => 1))

(fact "optional-sesame"
  (let [optional? (optional [:foo])]
    (:optional (meta (first optional?))) => truthy))

(fact "optional-2-sesame"
  (let [optional? (optional [:foo :bar])
        opt? (opt [:foo :bar])]
    optional? => opt?))

(fact "document->model-1-sesame"
  (let [m (build-model :sesame)
        _m (with-model m
             (document->model
              (java.io.ByteArrayInputStream. (.getBytes *test-xml*)) :xml))]
    (count (model->triples m)) => 3))

(fact "document->model-2-sesame"
  (let [m (build-model :sesame)
        _m (with-model m
             (document->model
              (java.io.ByteArrayInputStream. (.getBytes *test-xml-blanks*)) :xml))]
    (count (model->triples m))          => 4
    (o (first (model->triples m)))      => blank-node?
    (o (second (model->triples m))) =not=> blank-node?
    (o (nth (model->triples m) 2))  =not=> blank-node?
    (o (nth (model->triples m) 3))  =not=> blank-node?))

(fact "find-resources-sesame"
  (let [m (build-model :sesame)
        _m (with-model m
             (document->model
              (java.io.ByteArrayInputStream. (.getBytes *test-xml*)) :xml))
        res (find-resources m)]
    (count res) => 2))

(fact "find-resource-uris-sesame"
  (let [m (build-model :sesame)
        _m (with-model m
             (document->model
              (java.io.ByteArrayInputStream. (.getBytes *test-xml*)) :xml))
        res (find-resource-uris m)]
    (count res) => 2))

(fact "blank-node-sesame"
  (let [b1 (blank-node)
        b2 (b)
        b3 (blank-node :a)
        b4 (b :a)]
    b1 => blank-node?
    b2 => blank-node?
    b3 => blank-node?
    b4 => blank-node?
    (keyword (blank-node-id b3)) => :a
    (keyword (blank-node-id b4)) => :a))

(fact "test-blank-node-is-sesame"
  :?a                                   =not=> blank-node?
  (d 2)                                 =not=> blank-node?
  (l "test")                            =not=> blank-node?
  (rdf-resource "http://test.com/Test") =not=> blank-node?)

(fact "has-meta-sesame"
  (:triples (meta (make-triples [[:a :b :c]]))) => truthy)
