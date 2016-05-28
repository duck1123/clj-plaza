;; @author Antonio Garote
;; @email antoniogarrote@gmail.com
;; @date 11.05.2010

(ns plaza.rdf.implementations.sesame
  (:use [plaza.utils]
        [plaza.rdf core sparql]
        [plaza.rdf.implementations.common])
  (:import (org.openrdf.repository Repository RepositoryException)
           (org.openrdf.repository.sail SailRepository)
           (org.openrdf.rio RDFFormat)
           (org.openrdf.sail.memory MemoryStore)
           (org.openrdf.model.impl URIImpl BNodeImpl LiteralImpl ValueFactoryImpl)
           (org.openrdf.model URI BNode Literal ValueFactory)
           (org.openrdf.sail.inferencer.fc ForwardChainingRDFSInferencer)))

;; Loading RDFa java
;; (Class/forName "net.rootdev.javardfa.RDFaReader")

;; declaration of symbols
(declare parse-sesame-object)

;; Shared functions

(defn find-sesame-datatype
  "Finds the right datatype object from the string representation"
  [literal]
  (let [lit (let [literal-str (keyword->string literal)]
              (if (.startsWith literal-str "http://")
                (aget (.split literal-str "#") 1)
                literal))]
    (cond
     (= "xmlliteral" (.toLowerCase (keyword->string lit))) "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"
     (= "anyuri" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#anyURI"
     (= "boolean" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#boolean"
     (= "byte" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#byte"
     (= "date" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#date"
     (= "datetime" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#dateTime"
     (= "decimal" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#decimal"
     (= "double" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#double"
     (= "float" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#float"
     (= "int" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#int"
     (= "integer" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#integer"
     (= "long" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#long"
     (= "string" (.toLowerCase (keyword->string lit))) "http://www.w3.org/2001/XMLSchema#string"
     :else literal)))

(defn sesame-typed-literal-tojava
  "Transforms a sesame typed literal into the equivalente Java object"
  [lit]
  (cond
   (= "http://www.w3.org/2001/XMLSchema#boolean" (str (.getDatatype lit))) (.booleanValue lit)
   (= "http://www.w3.org/2001/XMLSchema#byte" (str (.getDatatype lit))) (.byteValue lit)
   (= "http://www.w3.org/2001/XMLSchema#date" (str (.getDatatype lit))) (.calendarValue lit)
   (= "http://www.w3.org/2001/XMLSchema#dateTime" (str (.getDatatype lit))) (.calendarValue lit)
   (= "http://www.w3.org/2001/XMLSchema#decimal" (str (.getDatatype lit))) (.decimalValue lit)

   (= "http://www.w3.org/2001/XMLSchema#double" (str (.getDatatype lit))) (.doubleValue lit)
   (= "http://www.w3.org/2001/XMLSchema#float" (str (.getDatatype lit))) (.floatValue lit)
   (= "http://www.w3.org/2001/XMLSchema#int" (str (.getDatatype lit))) (.intValue lit)
   (= "http://www.w3.org/2001/XMLSchema#integer" (str (.getDatatype lit))) (.integerValue lit)
   (= "http://www.w3.org/2001/XMLSchema#long" (str (.getDatatype lit))) (.longValue lit)
   (= "http://www.w3.org/2001/XMLSchema#string" (str (.getDatatype lit))) (.stringValue lit)
   true (.stringValue lit)))

(defn- translate-plaza-format
  "Translates an string representation of a RDF format to the sesame object"
  [format]
  (cond (= format "RDF/XML")  RDFFormat/RDFXML
        (= format "N-TRIPLE")  RDFFormat/NTRIPLES
        (= format "N3")  RDFFormat/N3
        (= format "TURTLE")  RDFFormat/TURTLE
        (= format "TTL")  RDFFormat/TURTLE
        (= format "XHTML")  :todo
        (= format "HTML")  :todo
        (= format "TRIG")  RDFFormat/TRIG
        (= format "TRIX")  RDFFormat/TRIX
        true      RDFFormat/RDFXML))

;; SPARQL

(defn- process-model-query-result
  "Transforms a query result into a dicitionary of bindings"
  [model result]
  (let [vars (iterator-seq (.iterator result))]
    (reduce
     (fn [acum item]
       (assoc acum (keyword (str "?" (.getName item)))
              (parse-sesame-object model (.getValue item))))
     {} vars)))

(defn- model-query-fn
  "Queries a model and returns a map of bindings"
  [model connection query]
  (let [query-string (if (string? query)
                       query
                       (str (build-query *sparql-framework* query)))]
    (let [tuple-query (.prepareTupleQuery connection org.openrdf.query.QueryLanguage/SPARQL
                                          query-string)
          result (.evaluate tuple-query)]
      (loop [acum []
             should-continue (.hasNext result)]
        (if should-continue
          (recur (conj acum (process-model-query-result model (.next result)))
                 (.hasNext result))
          acum)))))

(defn- model-query-triples-fn
  "Queries a model and returns a list of triple sets with results binding variables in que query pattern"
  [model connection query-or-string]
  (let [query (if (string? query-or-string) (sparql->query query-or-string) query-or-string)
        query-string (if (string? query-or-string)
                       query-or-string
                       (str (build-query *sparql-framework* query-or-string)))
        results (model-query-fn model connection query-string)]
    (map #(pattern-bind (:pattern query) %1) results)))

;; Sesame implementation

(deftype SesameResource [res]
  RDFNode

  (bnode?    [_] false)
  (resource? [_] true)
  (property? [_] false)
  (literal?  [_] false)

  RDFResource

  (resource-id [resource]
    (to-string resource))

  (qname-prefix [_]
    (.getNamespace res))

  (qname-local  [_]
    (.getLocalName res))

  (literal-value [resource]
    (throw (Exception. "Cannot retrieve literal value for a blank node")))

  (literal-language [resource]
    (throw (Exception. "Cannot retrieve lang for a blank node")))

  (literal-datatype-uri [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a blank node")))

  (literal-datatype-obj [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a blank node")))

  (literal-lexical-form [resource]
    (str res))

  JavaObjectWrapper

  (to-java [_] res)

  RDFPrintable

  (to-string [_] (str res))

  Object

  (toString [resource]
    (str res))

  (equals [resource other-resource]
    (and (= (class resource) (class other-resource))
         (= (resource-id resource) (resource-id other-resource))))

  (hashCode [resource]
    (.hashCode (resource-id resource))))

(deftype SesameBlank [res]
  RDFNode

  (bnode?    [_] true)
  (resource? [_] false)
  (property? [_] false)
  (literal?  [_] false)

  RDFResource

  (resource-id [resource]
    (.getID res))

  (qname-prefix [resource]
    "_")

  (qname-local [resource]
    (.getID res))

  (literal-value [resource]
    (throw (Exception. "Cannot retrieve literal value for a blank node")))

  (literal-language [resource]
    (throw (Exception. "Cannot retrieve lang for a blank node")))

  (literal-datatype-uri [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a blank node")))

  (literal-datatype-obj [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a resource")))

  (literal-lexical-form [resource]
    (.getId res))

  JavaObjectWrapper

  (to-java [_] res)

  RDFPrintable
  (to-string [resource]
    (str "_:" (resource-id resource)))

  Object

  (toString [resource]
    (to-string resource))

  (hashCode [resource]
    (.hashCode (resource-id resource)))

  (equals [resource other-resource]
    (= (resource-id resource) (resource-id other-resource))))

(deftype SesameLiteral [res]
  RDFNode

  (bnode? [_] false)
  (resource? [_] false)
  (property? [_] false)
  (literal? [_] true)

  RDFResource

  (resource-id [resource]
    (if (= (.getLanguage res) "")
      (.getLabel res)
      (str (.getLabel res) "@" (.getLanguage res))))

  (qname-prefix [_]
    (throw (Exception. "Cannot retrieve qname-prefix value for a literal")))

  (qname-local [_]
    (throw (Exception. "Cannot retrieve qname-local value for a literal")))

  (literal-value [_] (.stringValue res))

  (literal-language [_] (if-let [lang (.getLanguage res)] lang ""))

  (literal-datatype-uri [_] "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")

  (literal-datatype-obj [_] (find-sesame-datatype :xmlliteral))

  (literal-lexical-form [_] (.stringValue res))

  RDFDatatypeMapper

  (find-datatype [_ literal] (find-sesame-datatype literal))

  JavaObjectWrapper

  (to-java [_] res)

  RDFPrintable

  (to-string [resource]
    (let [lang (literal-language resource)]
      (if (= "" lang)
        (literal-lexical-form resource)
        (str  (literal-lexical-form resource) "@" lang))))

  Object

  (toString [resource]
    (to-string resource))
  (hashCode [resource]
    (.hashCode (resource-id resource)))
  (equals [resource other-resource]
    (= (resource-id resource) (resource-id other-resource))))

(deftype SesameTypedLiteral [res]
  RDFNode

  (bnode?    [_] false)
  (resource? [_] false)
  (property? [_] false)
  (literal?  [_] true)

  RDFResource

  (resource-id [resource]
    (to-string resource))

  (qname-prefix [resource]
    (throw (Exception. "Cannot retrieve qname-prefix value for a literal")))

  (qname-local [resource]
    (throw (Exception. "Cannot retrieve qname-local value for a literal")))

  (literal-value [resource]
    (sesame-typed-literal-tojava res))

  (literal-language [resource] "")

  (literal-datatype-uri [resource]
    (str (.getDatatype res)))

  (literal-datatype-obj [resource]
    (find-sesame-datatype (str (.getDatatype res))))

  (literal-lexical-form [resource]
    (str (literal-value resource)))

  RDFDatatypeMapper

  (find-datatype [resource literal]
    (find-sesame-datatype literal))

  JavaObjectWrapper

  (to-java [resource] res)

  RDFPrintable
  (to-string [resource]
    (str res))

  Object

  (toString [resource]
    (str res))
  (hashCode [resource]
    (.hashCode (resource-id resource)))
  (equals [resource other-resource]
    (if (satisfies? RDFResource other-resource)
      (= (resource-id resource) (resource-id other-resource))
      false)))

(deftype SesameProperty [res]
  RDFResource RDFNode RDFDatatypeMapper JavaObjectWrapper RDFPrintable
  (to-java [resource]
    res)
  (to-string [resource]
    (str res))
  (bnode? [resource]
    false)
  (resource? [resource]
    true)
  (property? [resource]
    true)
  (literal? [resource]
    false)
  (resource-id [resource]
    (to-string resource))
  (qname-prefix [resource]
    (.getNamespace res))
  (qname-local [resource]
    (.getLocalName res))
  (literal-value [resource]
    (throw (Exception. "Cannot retrieve literal value for a blank node")))
  (literal-language [resource]
    (throw (Exception. "Cannot retrieve lang for a blank node")))
  (literal-datatype-uri [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a blank node")))
  (literal-datatype-obj [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a blank node")))
  (literal-lexical-form [resource]
    (str res))
  (toString [resource]
    (str res))
  (hashCode [resource]
    (.hashCode (resource-id resource)))
  (equals [resource other-resource]
    (and (= (class resource) (class other-resource))
         (= (resource-id resource) (resource-id other-resource)))))

(deftype SesameModel [mod]
  RDFModel RDFDatatypeMapper JavaObjectWrapper RDFPrintable
  (to-java [model]
    mod)
  (create-resource [model ns local]
    (plaza.rdf.implementations.sesame.SesameResource.
     (.createURI (ValueFactoryImpl/getInstance) (expand-ns ns local))))
  (create-resource [model uri]
    (if (instance? plaza.rdf.core.RDFResource uri)
      uri
      (if (or (.startsWith (keyword->string uri) "http://")
              (.startsWith (keyword->string uri) "https://"))
        (plaza.rdf.implementations.sesame.SesameResource.
         (.createURI (ValueFactoryImpl/getInstance) (keyword->string uri)))
        (plaza.rdf.implementations.sesame.SesameResource.
         (.createURI (ValueFactoryImpl/getInstance)
                     (expand-ns *rdf-ns* (keyword->string uri)))))))
  (create-property [model ns local]
    (plaza.rdf.implementations.sesame.SesameProperty.
     (.createURI (ValueFactoryImpl/getInstance) (expand-ns ns local))))
  (create-property [model uri]
    (if (or (instance? plaza.rdf.implementations.sesame.SesameResource uri)
            (instance? plaza.rdf.implementations.sesame.SesameProperty uri))
      (plaza.rdf.implementations.sesame.SesameProperty.
       (.createURI (ValueFactoryImpl/getInstance) (to-string uri)))
      (if (or (.startsWith (keyword->string uri) "http://")
              (.startsWith (keyword->string uri) "https://"))
        (plaza.rdf.implementations.sesame.SesameProperty.
         (.createURI (ValueFactoryImpl/getInstance) (keyword->string uri)))
        (plaza.rdf.implementations.sesame.SesameProperty.
         (.createURI (ValueFactoryImpl/getInstance)
                     (expand-ns *rdf-ns* (keyword->string uri)))))))
  (create-blank-node [model]
    (plaza.rdf.implementations.sesame.SesameBlank.
     (.createBNode (ValueFactoryImpl/getInstance) (str (.getTime (java.util.Date.))))))
  (create-blank-node [model id]
    (plaza.rdf.implementations.sesame.SesameBlank.
     (.createBNode (ValueFactoryImpl/getInstance) (keyword->string id))))
  (create-literal [model lit]
    (plaza.rdf.implementations.sesame.SesameLiteral.
     (.createLiteral (ValueFactoryImpl/getInstance) lit)))
  (create-literal [model lit lang]
    (plaza.rdf.implementations.sesame.SesameLiteral.
     (.createLiteral (ValueFactoryImpl/getInstance) lit (keyword->string lang))))
  (create-typed-literal [model lit]
    (plaza.rdf.implementations.sesame.SesameTypedLiteral.
     (.createLiteral (ValueFactoryImpl/getInstance) lit)))
  (create-typed-literal [model lit type]
    (plaza.rdf.implementations.sesame.SesameTypedLiteral.
     (.createLiteral (ValueFactoryImpl/getInstance)
                     (str lit)
                     (.createURI (ValueFactoryImpl/getInstance)
                                 (str (find-sesame-datatype type))))))
  (critical-write [model f]
    (let [connection (.getConnection mod)]
      (try
        (do
          (.setAutoCommit connection false)
          (let [res (f)]
            (.commit connection)
            res))
        (catch RepositoryException e (.rollback connection))
        (finally (.close connection)))))
  (critical-read [model f]
    (critical-write model f)) ;; is reading thread-safe in Sesame?
  (add-triples [model triples]
    (let [connection (.getConnection mod)
          graph (let [g (org.openrdf.model.impl.GraphImpl.)]
                  (doseq [[ms mp mo] triples]
                    (.add g (to-java ms)
                          (to-java mp)
                          (to-java mo)
                          (into-array org.openrdf.model.Resource [])))
                  g)]
      (try
        (doto connection
          (.setAutoCommit false)
          (.add graph (into-array org.openrdf.model.Resource []))
          (.commit))
        (catch RepositoryException e
          (.rollback connection))
        (finally (.close connection)))
      model))
  (remove-triples [model triples]
    (let [connection (.getConnection mod)]
      (try
        (do
          (.setAutoCommit connection false)
          (loop [acum triples]
            (when (seq acum)
              (let [[ms mp mo] (first acum)]
                (.remove connection (to-java ms) (to-java (create-property model mp)) (to-java mo)
                         (into-array org.openrdf.model.Resource [])) ;; varargs!
                (recur (rest acum)))))
          (.commit connection))
        (catch RepositoryException e (.rollback connection))
        (finally (.close connection)))
      model))
  (walk-triples [model f]
    (let [connection (.getConnection mod)]
      (try
        (do
          (let [stmts (.asList (.getStatements connection nil nil nil true
                                               (into-array org.openrdf.model.Resource [])))
                res (map
                     (fn [st]
                       (let [s (let [subj (.getSubject st)]
                                 (if (instance? org.openrdf.model.Resource subj)
                                   (if (instance? org.openrdf.model.BNode subj)
                                     (create-blank-node model (str (.getID subj)))
                                     (create-resource model (str subj)))
                                   (create-resource model (str subj))))
                             p (create-property model (str (.getPredicate st)))
                             o (let [obj (.getObject st)]
                                 (if (instance? org.openrdf.model.Literal obj)
                                   (if (or (nil? (.getDatatype obj))
                                           (= (.getDatatype obj)
                                              "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))
                                     (create-literal model (.getLabel obj) (.getLanguage obj))
                                     (create-typed-literal model (.getLabel obj)
                                                           (str (.getDatatype obj))))
                                   (if (instance? org.openrdf.model.Resource obj)
                                     (if (instance? org.openrdf.model.BNode obj)
                                       (create-blank-node model (str (.getID obj)))
                                       (create-resource model (str obj)))
                                     (create-resource model (str obj)))))]
                         (f s p o)))
                     stmts)]
            (.commit connection)
            res))
        (catch RepositoryException e (.rollback connection))
        (finally (.close connection)))))
  (to-string [model]
    (walk-triples model (fn [s p o] [(to-string s) (to-string p) (to-string o)])))
  (load-stream [model stream format]
    (let [format (translate-plaza-format (parse-format format))
          connection (.getConnection mod)]
      (try
        (if (string? stream)
          (.add connection (plaza.utils/grab-document-url stream)
                stream format (into-array org.openrdf.model.Resource []))
          (.add connection stream *rdf-ns* format (into-array org.openrdf.model.Resource [])))
        (finally (.close connection)))
      model))
  (output-string [model writer format]
    (let [connection (.getConnection mod)
          writer (org.openrdf.rio.Rio/createWriter
                  (translate-plaza-format (parse-format format)) writer)]
      (try
        (.export connection writer (into-array org.openrdf.model.Resource []))
        (finally (.close connection))))
    model)
  (output-string [model format]
    (output-string model *out* format))
  (find-datatype [model literal]
    (translate-plaza-format (parse-format format)))
  (query [model query]
    (let [connection (.getConnection mod)]
      (try
        (model-query-fn model connection query)
        (finally (.close connection)))))
  (query-triples [model query]
    (let [connection (.getConnection mod)]
      (try
        (model-query-triples-fn model connection query)
        (finally (.close connection))))))

(deftype SesameSparqlFramework []
  SparqlFramework
  (parse-sparql->query [framework sparql]
    (parse-sparql->query-fn sparql))
  (parse-sparql->pattern [framework sparql]
    (parse-sparql->pattern-fn sparql))
  (build-filter [framework filter]
    (build-filter-fn framework filter))
  (build-query [framework query]
    (build-query-fn framework query))
  (var-expr? [framework expr]
    (var-expr-fn? expr))
  (var->keyword [framework var-expr]
    (let [s (.getVarName var-expr)]
      (if (.startsWith s "?")
        (keyword s)
        (keyword (str "?" s))))))

(defn- parse-sesame-object
  "Parses any Sesame relevant object into its plaza equivalent type"
  [model sesame]
  (cond (instance? org.openrdf.model.URI sesame) (create-resource model (str sesame))
        (instance? org.openrdf.model.BNode sesame) (create-blank-node model (str (.getID sesame)))
        (instance? org.openrdf.model.Literal sesame)
        (if (or (nil? (.getDatatype sesame))
                (= (.getDatatype sesame) "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))
          (create-literal model (.stringValue sesame) (.getLanguage sesame))
          (create-typed-literal model (sesame-typed-literal-tojava sesame)
                                (str (.getDatatype sesame))))
        true
        (throw (Exception. (str "Unable to parse object " sesame " of type " (class sesame))))))

;; Initialization

(defmethod build-model [:sesame]
  [& options]
  (let [repo (SailRepository. (MemoryStore.))]
    (.initialize repo)
    (plaza.rdf.implementations.sesame.SesameModel. repo)))

(defn init-sesame-framework
  "Setup all the root bindings to use Plaza with the Sesame framework. This function must be called
   before start using Plaza"
  []
  (alter-root-model (build-model :sesame))
  (alter-root-sparql-framework (plaza.rdf.implementations.sesame.SesameSparqlFramework.))
  (alter-root-model-builder-fn :sesame))
