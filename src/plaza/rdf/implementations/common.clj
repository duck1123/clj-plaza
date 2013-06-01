;; @author Antonio Garote
;; @email antoniogarrote@gmail.com
;; @date 12.05.2010

(ns plaza.rdf.implementations.common
  (:use plaza.rdf.core
        plaza.rdf.sparql
        plaza.utils)
  (:require [clojure.tools.logging :as log])
  (:import com.hp.hpl.jena.datatypes.BaseDatatype
           com.hp.hpl.jena.datatypes.xsd.XSDDatatype
           com.hp.hpl.jena.datatypes.xsd.impl.XMLLiteralType
           com.hp.hpl.jena.graph.Node
           com.hp.hpl.jena.graph.Node_Literal
           com.hp.hpl.jena.graph.Node_URI
           com.hp.hpl.jena.graph.Triple
           com.hp.hpl.jena.query.Query
           com.hp.hpl.jena.query.QueryFactory
           com.hp.hpl.jena.query.QueryExecutionFactory
           com.hp.hpl.jena.query.DatasetFactory
           com.hp.hpl.jena.rdf.model.AnonId
           com.hp.hpl.jena.rdf.model.ModelFactory
           com.hp.hpl.jena.reasoner.rulesys.RDFSRuleReasonerFactory
           com.hp.hpl.jena.shared.Lock
           com.hp.hpl.jena.sparql.core.Var
           com.hp.hpl.jena.sparql.expr.E_Add
           com.hp.hpl.jena.sparql.expr.E_Bound
           com.hp.hpl.jena.sparql.expr.E_Datatype
           com.hp.hpl.jena.sparql.expr.E_Divide
           com.hp.hpl.jena.sparql.expr.E_Equals
           com.hp.hpl.jena.sparql.expr.E_GreaterThan
           com.hp.hpl.jena.sparql.expr.E_GreaterThanOrEqual
           com.hp.hpl.jena.sparql.expr.E_IsBlank
           com.hp.hpl.jena.sparql.expr.E_IsLiteral
           com.hp.hpl.jena.sparql.expr.E_IsIRI
           com.hp.hpl.jena.sparql.expr.E_IsURI
           com.hp.hpl.jena.sparql.expr.E_Lang
           com.hp.hpl.jena.sparql.expr.E_LessThan
           com.hp.hpl.jena.sparql.expr.E_LessThanOrEqual
           com.hp.hpl.jena.sparql.expr.E_Multiply
           com.hp.hpl.jena.sparql.expr.E_NotEquals
           com.hp.hpl.jena.sparql.expr.E_SameTerm
           com.hp.hpl.jena.sparql.expr.E_Str
           com.hp.hpl.jena.sparql.expr.E_Subtract
           com.hp.hpl.jena.sparql.expr.ExprFunction
           com.hp.hpl.jena.sparql.expr.ExprVar
           com.hp.hpl.jena.sparql.expr.NodeValue
           com.hp.hpl.jena.sparql.syntax.ElementFilter
           com.hp.hpl.jena.sparql.syntax.Element
           com.hp.hpl.jena.sparql.syntax.ElementFilter
           com.hp.hpl.jena.sparql.syntax.ElementGroup
           com.hp.hpl.jena.sparql.syntax.ElementOptional
           com.hp.hpl.jena.vocabulary.ReasonerVocabulary))

(defn make-custom-type
  "Builds a datatype for a custom XSD datatype URI based on the String basic type"
  [uri]
  (proxy [BaseDatatype] [uri]
    (unparse [v] (.lexicalValue v))
    (parse [lf] lf)
    (isEqual [v1 v2] (= v1 v2))))

(defn find-jena-datatype
  "Finds the right datatype object from the string representation"
  [literal]
  (let [lit (let [literal-str (keyword->string literal)]
              (if (.startsWith literal-str "http://")
                (aget (.split literal-str "#") 1)
                literal))]
    (condp = (.toLowerCase (keyword->string lit))
      "xmlliteral" XMLLiteralType/theXMLLiteralType
      "literal"    XMLLiteralType/theXMLLiteralType
      "anyuri"     XSDDatatype/XSDanyURI
      "boolean"    XSDDatatype/XSDboolean
      "byte"       XSDDatatype/XSDbyte
      "date"       XSDDatatype/XSDdate
      "datetime"   XSDDatatype/XSDdateTime
      "decimal"    XSDDatatype/XSDdecimal
      "double"     XSDDatatype/XSDdouble
      "float"      XSDDatatype/XSDfloat
      "int"        XSDDatatype/XSDint
      "integer"    XSDDatatype/XSDinteger
      "long"       XSDDatatype/XSDlong
      "string"     XSDDatatype/XSDstring

      (make-custom-type literal))))

(def datatype-symbols
  #{"xmlliteral" "literal" "anyuri" "boolean" "byte" "date" "datetime"
    "decimal" "double" "float" "int" "integer" "long" "string"})

(defn datatype-symbol
  "Transforms a XMLSchema datatype URI into a symbol representing the type"
  [literal]
  (let [literal-str (keyword->string literal)
        lit (if (and (.startsWith literal-str "http://")
                     (not= -1 (.indexOf literal-str "#")))
              (aget (.split literal-str "#") 1)
              literal)]
    (when (datatype-symbols lit)
      (keyword lit))))

(defn supported-datatype?
  "Returns true if the datatype sym or URI string is supported"
  [sym]
  (try
    (find-jena-datatype sym)
    true
    (catch Exception ex false)))

(defn datatype-uri
  "Returns the URI for a datatype symbol like :int :decimal or :anyuri"
  [sym]
  (.getURI (find-jena-datatype sym)))

(defn parse-dataype-string
  "Parses a string containing a datatype of type sym"
  [sym data]
  (.parse (find-jena-datatype sym) data))

(defn filter-expr?
  "Tests if one Jena expression is a filter expression"
  [expr]
  (or (instance? ExprFunction expr)
      (instance? ElementFilter expr)))

(defn var-expr-fn?
  "Tests if one Jena expression is a var expression"
  [expr]
  (or (and (keyword? expr)
           (.startsWith (keyword->string expr) "?"))
      (= (class expr) ExprVar)))

(defn- parse-pattern-literal
  "Parses a literal value"
  [lit]
  (if (nil? (.getLiteralDatatypeURI lit))
    (l (.getLiteralLexicalForm lit) (.getLiteralLanguage lit))
    (d (.getLiteralValue lit) (.getLiteralDatatypeURI lit))))

(defn- parse-pattern-atom
  "Parses a single component of a pattern: variable, literal, URI, etc"
  [atom pos]
  (condp instance? atom
    Var (keyword (str "?" (.getVarName atom)))
    Node_URI ((condp = pos
                :subject   rdf-resource
                :predicate rdf-property
                :object    rdf-resource)
              (.getURI atom))
    Node_Literal (parse-pattern-literal atom)
    atom))

(defn parse-literal-lexical
  [lit]
  (let [parts-a (.split lit "\\^\\^")
        val-a (aget parts-a 0)
        datatype (if (= (alength parts-a) 2)
                   (aget (.split (aget (.split (aget parts-a 1) "<") 1) ">") 0)
                   "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral")
        parts-b (.split val-a "@")
        val (let [val-tmp (aget parts-b 0)]
              (if (and (.startsWith val-tmp "\"")
                       (.endsWith val-tmp "\""))
                (aget (.split (aget (.split val-tmp "\"") 1) "\"") 0)
                val-tmp))
        lang-tag (when (= (alength parts-b) 2)
                   (aget parts-b 1))]
    (if (nil? lang-tag)
      (rdf-typed-literal val datatype)
      (rdf-literal val lang-tag))))

(declare parse-filter-expr)
(defn- parse-next-filter-expr
  [expr]
  (cond
   (var-expr-fn? expr) (keyword (str expr))
   (filter-expr? expr) (parse-filter-expr expr)
   true (parse-literal-lexical (str expr))))

(defn- parse-filter-expr-2
  [expr symbol]
  {:expression (keyword symbol)
   :kind :two-parts
   :args (mapv #(parse-next-filter-expr (.getArg expr %))
               [1 2])})

(defn- parse-filter-expr-1
  [expr symbol]
  {:expression (keyword symbol)
   :kind :one-part
   :args [(parse-next-filter-expr (.getArg expr 1))]})

(defn parse-filter-expr
  "Parses a filter expression"
  [expr]
  (condp = (class expr)
    E_Str                (parse-filter-expr-1 "str")
    E_Lang               (parse-filter-expr-1 expr "lang")
    E_Datatype           (parse-filter-expr-1 expr "datatype")

    E_Bound              (parse-filter-expr-1 expr "bound")
    E_IsIRI              (parse-filter-expr-1 expr "isIRI")
    E_IsURI              (parse-filter-expr-1 expr "isURI")
    E_IsBlank            (parse-filter-expr-1 expr "isBlank")
    E_IsLiteral          (parse-filter-expr-1 expr "isLiteral")

    E_GreaterThanOrEqual (parse-filter-expr-2 expr ">=")
    E_GreaterThan        (parse-filter-expr-2 expr ">")
    E_LessThanOrEqual    (parse-filter-expr-2 expr "<=")
    E_LessThan           (parse-filter-expr-2 expr "<")
    E_NotEquals          (parse-filter-expr-2 expr "!=")
    E_Equals             (parse-filter-expr-2 expr "=")
    E_Subtract           (parse-filter-expr-2 expr "-")
    E_Add                (parse-filter-expr-2 expr "+")
    E_Multiply           (parse-filter-expr-2 expr "*")
    E_Divide             (parse-filter-expr-2 expr "div")
    E_SameTerm           (parse-filter-expr-2 expr "sameTerm")

    (throw (Exception. (str "Trying to parse unknown/not supported filter: " expr)))))

(defn sparql->pattern-filters*
  [elem]
  (let [pattern-els (if (instance? ElementOptional elem)
                                (.patternElts (first (.getElements (.getOptionalElement elem))))
                                (if (instance? ElementFilter elem)
                                  (.iterator [elem])
                                  (.patternElts elem)))
                  is-optional (if (instance? ElementOptional elem)
                                true
                                false)]
              (loop [should-continue (.hasNext pattern-els)
                     acum []]
                (if should-continue
                  (let [next-elt (.next pattern-els)]
                    (if (instance? ElementFilter next-elt)
                      ;; This is a filter
                      (recur (.hasNext pattern-els)
                             (conj acum (with-meta (parse-filter-expr (.getExpr next-elt))
                                          {:filter true
                                           :optional is-optional})))
                      ;; A regular (maybe optional) expression
                      (recur (.hasNext pattern-els)
                             (conj acum
                                   (with-meta [(parse-pattern-atom (.getSubject next-elt) :subject)
                                               (parse-pattern-atom (.getPredicate next-elt) :predicate)
                                               (parse-pattern-atom (.getObject next-elt) :object)]
                                     {:optional is-optional
                                      :filter false})))))
                  acum))))

(defn sparql->pattern-filters
  "Parses a SPARQL query and transform it into a pattern and some filters"
  [sparql-string-or-query]
  (let [query (if (string? sparql-string-or-query)
                (QueryFactory/create sparql-string-or-query)
                sparql-string-or-query)
        query-pattern-els (.getElements (.getQueryPattern query))]
    (flatten-1
     (map sparql->pattern-filters* query-pattern-els))))

(defn parse-sparql->pattern-fn
  "Parses a SPARQL query and transform it into a pattern"
  [sparql-string-or-query]
  (filter (comp not :filter meta) (sparql->pattern-filters sparql-string-or-query)))

(defn parse-sparql->query-fn
  "Parses a SPARQL query and builds a whole query dictionary"
  [sparql-string]
  (let [query (QueryFactory/create sparql-string)
        pattern-filters (sparql->pattern-filters query)]
    {:vars (mapv keyword (.getResultVars query))
     :filters (filter (comp :filter meta)     pattern-filters)
     :pattern (filter (comp not :filter meta) pattern-filters)
     :kind (condp = (.getQueryType query)
             Query/QueryTypeAsk       :ask
             Query/QueryTypeConstruct :construct
             Query/QueryTypeDescribe  :describe
             Query/QueryTypeSelect    :select
             :unknown)}))

;; bulding of queries and filters

(defn- build-filter-two-parts
  "Builds a filter with two parts"
  [expression arg-0 arg-1]
  (condp = expression
    :>=       (E_GreaterThanOrEqual. arg-0 arg-1)
    :>        (E_GreaterThan. arg-0 arg-1)
    :<=       (E_LessThanOrEqual. arg-0 arg-1)
    :<        (E_LessThan. arg-0 arg-1)
    :!=       (E_NotEquals. arg-0 arg-1)
    :=        (E_Equals. arg-0 arg-1)
    :-        (E_Subtract. arg-0 arg-1)
    :+        (E_Add. arg-0 arg-1)
    :*        (E_Multiply. arg-0 arg-1)
    :div      (E_Divide. arg-0 arg-1)
    :sameTerm (E_SameTerm. arg-0 arg-1)))

(defn- build-filter-one-part
  [expression arg]
  (condp = expression
    :str       (E_Str. arg)
    :lang      (E_Lang. arg)
    :datatype  (E_Datatype. arg)
    :bound     (E_Bound. arg)
    :isIRI     (E_IsIRI. arg)
    :isURI     (E_IsURI. arg)
    :isBlank   (E_IsBlank. arg)
    :isLiteral (E_IsLiteral. arg)))

(defn- build-filter-arg
  [builder arg]
  (cond
   (keyword? arg) (ExprVar. (.replace (keyword->string arg) "?" ""))
   (map? arg) (build-filter builder arg)
   (resource? arg) (NodeValue/makeNode (Node/createURI (resource-id arg)))
   :default (NodeValue/makeNode
             (literal-lexical-form arg)
             (literal-language arg)
             (literal-datatype-uri arg))))

(defn build-filter-fn
  [builder filter]
  (if (= :two-parts (:kind filter))
    (build-filter-two-parts (:expression filter)
                            (build-filter-arg builder (first (:args filter)))
                            (build-filter-arg builder (second (:args filter))))
    (build-filter-one-part (:expression filter)
                           (build-filter-arg builder (first (:args filter))))))

(defn- build-query-atom
  "Transforms a query atom (subject, predicate or object) in the suitable Jena object for a Jena query"
  [atom]
  (if (keyword? atom)
    (Var/alloc (keyword->variable atom))
    (if (literal? atom)
      (if (= (find-jena-datatype (literal-datatype-uri atom)) (find-jena-datatype :xmlliteral))
        (Node/createLiteral (literal-lexical-form atom) (literal-language atom) false)
        (Node/createLiteral (literal-lexical-form atom) (literal-language atom) (find-jena-datatype (literal-datatype-uri atom))))
      (if (bnode? atom)
        (Node/createAnon (AnonId. (resource-id atom)))
        (Node/createURI (if (resource? atom)
                          (to-string atom)
                          (str atom)))))))

(defn build-query-fn*
  [acum item]
  (let [building (:building acum)
        optional (:optional acum)
        [s p o] item
        triple (Triple/create (build-query-atom s)
                              (build-query-atom p)
                              (build-query-atom o))]
    (if (:optional (meta item))
      ;; add it to the optional elem
      (let [optg (ElementGroup.)]
        (.addTriplePattern optg triple)
        {:building building
         :optional (conj optional optg)})
      ;; Is not an optional triple
      (do (.addTriplePattern building triple)
          {:building building
           :optional optional}))))

(defn build-query-fn
  "Transforms a query representation into a Jena Query object"
  [builder query]
  (let [built-query (Query.)
        pattern (:pattern query)
        built-patterns (reduce build-query-fn*
                               {:building (ElementGroup.)
                                :optional []}
                               pattern)
        built-pattern (do
                        (when-not (.isEmpty (:optional built-patterns))
                          (doseq [optg (:optional built-patterns)]
                            (.addElement (:building built-patterns)
                                         (ElementOptional. optg))))
                        (:building built-patterns))
        built-filters (loop [bfs (map (partial build-filter builder)
                                      (if (nil? (:filters query)) [] (:filters query)))]
                        (when-let [bf (first bfs)]
                          (.addElement built-pattern (ElementFilter. bf))
                          (recur (rest bfs))))]
    (do
      (loop [idx 0]
        (when (< idx (count (:vars query)))
          (do
            (.addResultVar built-query (keyword->variable (nth (:vars query) idx)))
            (recur (inc idx))))))
    (.setQueryPattern built-query built-pattern)

    (condp = (:kind query)
      :ask       (.setQueryAskType built-query)
      :construct (.setQueryConstructType built-query)
      :describe  (.setQueryDescribeType built-query)
      :select    (.setQuerySelectType built-query))

    (when (:limit query)
      (.setLimit built-query (:limit query)))
    (when (:offset query)
      (.setOffset built-query (:offset query)))
    (when (:distinct query)
      (.setDistinct built-query true))
    (when (:reduced query)
      (.setReduced built-query true))
    (when (:order-by query)
      (.addOrderBy built-query (keyword->variable (:order-by query)) 1))
    built-query))
