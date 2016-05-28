;; @author Antonio Garrote
;; @email antoniogarrote@gmail.com
;; @date 09.05.2010

(ns plaza.rdf.implementations.jena
  (:use [plaza.rdf.core :only [*rdf-ns* *rdf-ns-table* alter-root-model
                               alter-root-model-builder-fn build-model
                               create-blank-node create-literal create-property
                               create-resource create-typed-literal
                               critical-read critical-write expand-ns
                               find-datatype literal-datatype-uri
                               literal-language literal-lexical-form
                               model-critical-read parse-format property?
                               output-string resource-id to-java to-string
                               walk-triples]]
        [plaza.rdf.sparql :only [*sparql-framework* alter-root-sparql-framework
                                 build-query pattern-bind pattern-reject-unbound
                                 sparql->query SparqlFramework]]
        plaza.rdf.implementations.common
        plaza.utils)
  (:require [clojure.tools.logging :as log])
  (:import org.apache.jena.datatypes.xsd.XSDDatatype
           org.apache.jena.datatypes.xsd.impl.XMLLiteralType
           org.apache.jena.graph.Node
           org.apache.jena.graph.Triple
           org.apache.jena.rdf.model.AnonId
           org.apache.jena.rdf.model.Literal
           org.apache.jena.rdf.model.ModelFactory
           org.apache.jena.rdf.model.ResourceFactory
           org.apache.jena.rdf.model.impl.PropertyImpl
           org.apache.jena.rdf.model.impl.ResourceImpl
           org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory
           org.apache.jena.shared.Lock
           org.apache.jena.sparql.core.Var
           org.apache.jena.sparql.expr.E_Datatype
           org.apache.jena.sparql.expr.E_Add
           org.apache.jena.sparql.expr.E_Bound
           org.apache.jena.sparql.expr.E_Divide
           org.apache.jena.sparql.expr.E_Equals
           org.apache.jena.sparql.expr.E_GreaterThan
           org.apache.jena.sparql.expr.E_GreaterThanOrEqual
           org.apache.jena.sparql.expr.E_IsBlank
           org.apache.jena.sparql.expr.E_IsIRI
           org.apache.jena.sparql.expr.E_IsLiteral
           org.apache.jena.sparql.expr.E_IsURI
           org.apache.jena.sparql.expr.E_Lang
           org.apache.jena.sparql.expr.E_LessThan
           org.apache.jena.sparql.expr.E_LessThanOrEqual
           org.apache.jena.sparql.expr.E_Multiply
           org.apache.jena.sparql.expr.E_NotEquals
           org.apache.jena.sparql.expr.E_Str
           org.apache.jena.sparql.expr.E_Subtract
           org.apache.jena.sparql.syntax.Element
           org.apache.jena.sparql.syntax.ElementFilter
           org.apache.jena.sparql.syntax.ElementGroup
           org.apache.jena.sparql.syntax.ElementOptional
           org.apache.jena.query.DatasetFactory
           org.apache.jena.query.QueryExecutionFactory
           org.apache.jena.query.QueryFactory
           org.apache.jena.vocabulary.ReasonerVocabulary
           java.util.GregorianCalendar
           plaza.rdf.core.JavaObjectWrapper
           plaza.rdf.core.RDFDatatypeMapper
           plaza.rdf.core.RDFModel
           plaza.rdf.core.RDFNode
           plaza.rdf.core.RDFPrintable
           plaza.rdf.core.RDFResource))

;; Loading RDFa java

;; (Class/forName "net.rootdev.javardfa.jena.RDFaReader")

;; declaration of symbols

(declare parse-jena-object)

;; SPARQL

;; bulding of queries and filters

(defn- process-model-query-result
  "Transforms a query result into a dicitionary of bindings"
  [model result]
  (let [vars (iterator-seq (.varNames result))]
    (reduce (fn [acum item]
              (assoc acum (keyword (str "?" item))
                     (parse-jena-object model (.get result item))))
            {} vars)))

(defn- model-query-fn
  "Queries a model and returns a map of bindings"
  [model query query-string]
  (model-critical-read
   model
   (let [qexec (QueryExecutionFactory/create query-string (to-java model))
         results (iterator-seq (cond (= (:kind query) :select)
                                     (.execSelect qexec)))]
     (map #(process-model-query-result model %1) results))))

(defn- model-query-triples-fn
  "Queries a model and returns a list of triple sets
 with results binding variables in que query pattern"
  [model query-or-string]
  (let [query (if (string? query-or-string) (sparql->query query-or-string) query-or-string)
        query-string (if (string? query-or-string)
                       query-or-string
                       (str (build-query *sparql-framework* query-or-string)))
        results (model-query-fn model query query-string)]
    (map #(pattern-reject-unbound (pattern-bind (:pattern query) %1)) results)))


;; JENA implementation

(deftype JenaResource [res]
  RDFNode
  (bnode? [resource]
    false)
  (resource? [resource]
    true)
  (property? [resource]
    false)
  (literal? [resource]
    false)

  RDFResource
  (resource-id [resource]
    (.getURI res))
  (qname-prefix [resource]
    (.getNameSpace res))
  (qname-local [resource]
    (.getLocalName res))
  (literal-value [resource]
    (throw (Exception. "Cannot retrieve literal value for a resource")))
  (literal-language [resource]
    (throw (Exception. "Cannot retrieve lang for a resource")))
  (literal-datatype-uri [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a resource")))
  (literal-datatype-obj [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a resource")))
  (literal-lexical-form [resource]
    (resource-id resource))

  JavaObjectWrapper
  (to-java [resource]
    res)

  RDFPrintable
  (to-string [resource]
    (.getURI res))

  Object
  (toString [resource]
    (.getURI res))
  (hashCode [resource]
    (.hashCode (resource-id resource)))
  (equals [resource other-resource]
    (and (= (class resource) (class other-resource))
         (= (resource-id resource) (resource-id other-resource)))))


(deftype JenaBlank [res]
  RDFResource RDFNode JavaObjectWrapper RDFPrintable
  (to-java [resource]
    res)
  (to-string [resource]
    (str "_:" (resource-id resource)))
  (bnode? [resource]
    true)
  (resource? [resource]
    false)
  (property? [resource]
    false)
  (literal? [resource]
    false)
  (resource-id [resource]
    (str (.getId res)))
  (qname-prefix [resource]
    "_")
  (qname-local [resource]
    (str (resource-id resource)))
  (literal-value [resource]
    (throw (Exception. "Cannot retrieve literal value for a blank node")))
  (literal-language [resource]
    (throw (Exception. "Cannot retrieve lang for a blank node")))
  (literal-datatype-uri [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a blank node")))
  (literal-datatype-obj [resource]
    (throw (Exception. "Cannot retrieve datatype-uri for a resource")))
  (literal-lexical-form [resource]
    (str "_:" (resource-id resource)))
  (toString [resource]
    (to-string resource))
  (hashCode [resource]
    (.hashCode (resource-id resource)))
  (equals [resource other-resource]
    (= (resource-id resource) (resource-id other-resource))))


(deftype JenaLiteral [res]
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
    (.getValue res))

  (literal-language [resource]
    (.getLanguage res))

  (literal-datatype-uri [resource]
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")

  (literal-datatype-obj [resource]
    (find-jena-datatype :xmlliteral))

  (literal-lexical-form [resource]
    (.getLexicalForm res))

  RDFPrintable

  (to-string [resource]
    (let [lang (literal-language resource)]
      (if (= "" lang)
        (literal-lexical-form resource)
        (str  (literal-lexical-form resource) "@" lang))))

  RDFDatatypeMapper

  (find-datatype [resource literal]
    (find-jena-datatype literal))

  JavaObjectWrapper

  (to-java [resource]
    res)

  Object

  (toString [resource]
    (to-string resource))

  (hashCode [resource]
    (.hashCode (resource-id resource)))

  (equals [resource other-resource]
    (= (resource-id resource) (resource-id other-resource))))

(deftype JenaTypedLiteral [res]
  RDFNode

  (bnode?    [_] false)
  (resource? [_] false)
  (property? [_] false)
  (literal?  [_] true)

  RDFResource

  (resource-id [resource] (to-string resource))

  (qname-prefix [resource]
    (throw (Exception. "Cannot retrieve qname-prefix value for a literal")))

  (qname-local [resource]
    (throw (Exception. "Cannot retrieve qname-local value for a literal")))

  (literal-value [_] (.getValue res))

  (literal-language [_] "")

  (literal-datatype-uri [_] (str (.getDatatypeURI res)))

  (literal-datatype-obj [resource]
    (find-jena-datatype (.getDatatypeURI res)))

  (literal-lexical-form [_] (.getLexicalForm res))

  RDFPrintable

  (to-string [resource]
    (str "\"" (literal-lexical-form resource) "\"^^<" (literal-datatype-uri resource) ">"))

  RDFDatatypeMapper

  (find-datatype [resource literal]
    (find-jena-datatype literal))

  JavaObjectWrapper

  (to-java [resource]
    res)

  Object

  (toString [resource]
    (to-string resource))

  (hashCode [resource]
    (.hashCode (resource-id resource)))

  (equals [resource other-resource]
    (and (= (class resource) (class other-resource))
         (= (resource-id resource) (resource-id other-resource)))))

(deftype JenaProperty [res]
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
    (.getNameSpace res))
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
    (to-string res))
  (toString [resource]
    (str res))
  (hashCode [resource]
    (.hashCode (resource-id resource)))
  (equals [resource other-resource]
    (and (= (class resource) (class other-resource))
         (= (resource-id resource) (resource-id other-resource)))))


(deftype JenaModel [mod]
  JavaObjectWrapper

  (to-java [_] mod)

  RDFModel

  (create-resource [model ns local]
    (create-property model ns local))

  (create-resource [model uri]
    (println "uri: " uri)
    (if (instance? RDFResource uri)
      uri
      (ResourceFactory/createResource
       (let [uri-string (keyword->string uri)]
         (if (or (.startsWith uri-string "http://") (.startsWith uri-string "https://"))
           uri-string
           (str *rdf-ns* uri-string))))))

  (create-property [_ ns local]
    (ResourceFactory/createProperty ns local))

  (create-property [model uri]
    (if (instance? RDFResource uri)
      (if (property? uri)
        uri
        (ResourceFactory/createProperty (str uri)))
      (if (or (.startsWith (keyword->string uri) "http://")
              (.startsWith (keyword->string uri) "https://"))
        (ResourceFactory/createProperty (keyword->string uri))
        (ResourceFactory/createProperty *rdf-ns* (keyword->string uri)))))

  (create-blank-node [_] (ResourceFactory/createResource))

  (create-blank-node [_ id] (ResourceFactory/createResource (keyword->string id)))

  (create-literal [model lit] (ResourceFactory/createStringLiteral lit))

  (create-literal [model lit lang] (ResourceFactory/createLangLiteral lit lang))

  (create-typed-literal [model lit] (ResourceFactory/createTypedLiteral lit))

  (create-typed-literal [model lit type]
    (println "lit: " lit)
    (println "type: " type)
    (let [dt (find-datatype model type)]
      (println "dt: " dt)
      (if (instance? GregorianCalendar lit)
        (ResourceFactory/createTypedLiteral lit)
        (ResourceFactory/createTypedLiteral (str lit) dt))))

  (critical-write [model f]
    (do
      (.enterCriticalSection mod Lock/WRITE)
      (let [res (f)]
        (.leaveCriticalSection mod)
        res)))
  (critical-read [model f]
    (do
      (.enterCriticalSection mod Lock/READ)
      (let [res (f)]
        (.leaveCriticalSection mod)
        res)))
  (add-triples [model triples]
    (critical-write
     model
     (fn []
       (loop [acum triples]
         (when (seq acum)
           (let [[ms mp mo] (first acum)]
             (.add mod (to-java ms) (to-java (create-property model mp)) (to-java mo))
             (recur (rest acum))))))))
  (remove-triples [model triples]
    (critical-write
     model
     (fn []
       (loop [acum triples]
         (when (seq acum)
           (let [[ms mp mo] (first acum)]
             (.remove mod
                      (first (iterator-seq
                              (.listStatements mod (to-java ms)
                                               (to-java (create-property model mp))
                                               (to-java mo)))))
             (recur (rest acum))))))))
  (walk-triples [model f]
    (critical-read
     model
     (fn []
       (let [stmts (iterator-seq (.listStatements mod))]
         (map
          (fn [st]
            (let [s (let [subj (.getSubject st)]
                      (if (instance? ResourceImpl subj)
                        (if (.isAnon subj)
                          (create-blank-node model (str (.getId subj)))
                          (create-resource model (str subj)))
                        (create-resource model (str subj))))
                  p (create-property model (str (.getPredicate st)))
                  o (let [obj (.getObject st)]
                      (if (instance? Literal obj)
                        (if (or (nil? (.getDatatypeURI obj))
                                (= (.getDatatypeURI obj)
                                   "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))
                          (create-literal model (.getValue obj) (.getLanguage obj))
                          (create-typed-literal model (.getValue obj) (.getDatatypeURI obj)))
                        (if (instance? ResourceImpl obj)
                          (if (.isAnon obj)
                            (create-blank-node model (str (.getId obj)))
                            (create-resource model (str obj)))
                          (create-resource model (str obj)))))]
              (f s p o)))
          stmts)))))
  (load-stream [model stream format]
    (let [format (parse-format format)]
      (critical-write
       model
       (fn []
         (if (string? stream)
           (.read mod stream format)
           (.read mod stream *rdf-ns* format))))
      model))
  (output-string [model writer format]
    (critical-read
     model
     (fn []
       (try
         (let [existing-prefixes (.getNsPrefixMap mod)
               new-map @*rdf-ns-table*
               new-prefixes (zipmap (map name (keys new-map)) (vals new-map))]
           (.setNsPrefixes mod (merge new-prefixes existing-prefixes)))
         (catch Exception e (.printStackTrace e)))
       (.write mod writer (parse-format format)))))
  (output-string  [model format]
    (output-string model *out* format))
  (query [model query]
    (model-query-fn model query (str (build-query *sparql-framework* query))))
  (query-triples [model query]
    (model-query-triples-fn model query))


  RDFDatatypeMapper
  (find-datatype [model literal]
    (find-jena-datatype literal))

  RDFPrintable
  (to-string [model]
    (walk-triples model (fn [s p o] [(to-string s) (to-string p) (to-string o)]))))

(deftype JenaSparqlFramework []
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


(defn parse-jena-object
  "Parses any Jena relevant object into its plaza equivalent type"
  [model jena]
  (if (instance? ResourceImpl jena)
    (if (.isAnon jena)
      (create-blank-node model (str (.getId jena)))
      (create-resource model (str jena)))
    (if (instance? PropertyImpl jena)
      (create-property model (str jena))
      (if (instance? Literal jena)
        (if (or (nil? (.getDatatypeURI jena))
                (= (.getDatatypeURI jena)
                   "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral"))
          (create-literal model (.getValue jena) (.getLanguage jena))
          (create-typed-literal model (.getValue jena) (.getDatatypeURI jena)))
        (throw (Exception. (str "Unable to parse object " jena " of type " (class jena))))))))

;; Initialization

(defmethod build-model [:jena]
  [& options] (JenaModel. (ModelFactory/createDefaultModel)))

(defn init-jena-framework
  "Setup all the root bindings to use Plaza with the Jena framework. This function must be called
   before start using Plaza"
  []
  (alter-root-model (build-model :jena))
  (alter-root-sparql-framework (JenaSparqlFramework.))
  (alter-root-model-builder-fn :jena))
