;; @author Antonio Garote
;; @email antoniogarrote@gmail.com
;; @date 04.05.2010

(ns plaza.rdf.predicates
  (:use [plaza.rdf.core :only [*rdf-model* bnode? expand-ns find-datatype
                               find-ns-registry literal-datatype-uri
                               literal-language literal-lexical-form
                               literal-value qname-local qname-prefix
                               resource-id]]
        [plaza.rdf.sparql :only [*sparql-framework* var-expr?]]
        [plaza.utils :only [keyword->string]])
  (:require [plaza.rdf.core :as rdf]))

(defn literal-fn?
  "Applies a custom predicate function to a literal"
  [f]
  (fn [triple atom] (f atom)))

;; boolean checkers

(defn and?
  "Applies and to a series of matchers"
  [& preds]
  (fn [triple atom]
    (reduce (fn [acum item] (and acum (item triple atom))) true preds)))

(defn or?
  "Applies or to a series of matchers"
  [& preds]
  (fn [triple atom]
    (reduce (fn [acum item] (or acum (item triple atom))) false preds)))

(defn not?
  "Negates a clause query"
  [pred]
  (fn [triple atom]
    (not (pred triple atom))))


(defn triple-and?
  "Checks if one triple matches a set of conditions"
  [& conds]
  (fn [triple] ((apply and? conds) triple triple)))

(defn triple-or?
  "Checks if one triple matches a set of conditions"
  [& conds]
  (fn [triple] ((apply or? conds) triple triple)))


(defn subject-and?
  "Checks a condition over the subject of a triple"
  [& conditions]
  (fn [triple atom]
    ((apply and? conditions) triple (nth triple 0))))

(defn subject-or?
  "Checks a condition over the subject of a triple"
  [& conditions]
  (fn [triple atom]
    ((apply or? conditions) triple (nth triple 0))))


(defn predicate-and?
  "Checks a condition over the predicate of a triple"
  [& conditions]
  (fn [triple atom]
    ((apply and? conditions) triple (nth triple 1))))

(defn predicate-or?
  "Checks a condition over the predicate of a triple"
  [& conditions]
  (fn [triple atom]
    ((apply or? conditions) triple (nth triple 1))))


(defn object-and?
  "Checks a condition over the object of a triple"
  [& conditions]
  (fn [triple atom]
    ((apply and? conditions) triple (nth triple 2))))

(defn object-or?
  "Checks a condition over the object of a triple"
  [& conditions]
  (fn [triple atom]
    ((apply or? conditions) triple (nth triple 2))))




(defn predicate?
  "Checks a condition over the predicate of a triple"
  [condition]
  (fn [triple atom]
    ((predicate-and? condition) triple atom)))

(defn subject?
  "Checks a condition over the subject of a triple"
  [condition]
  (fn [triple atom]
    ((subject-and? condition) triple atom)))

(defn object?
  "Checks a condition over the object of a triple"
  [condition]
  (fn [triple atom]
    ((object-and? condition) triple atom)))



;; model value extraction

(defn fn-apply?
  "Applies a function to a value"
  [f]
  (fn [triple atom] (f atom)))

(defn fn-triple-apply?
  "Applies a function to a value"
  [f]
  (fn [triple atom] (f triple)))

;; model testing

(defn triple-check
  "Checks if one triple matches a set of conditions"
  [cond]
  (fn [triple] ((triple-and? cond) triple)))

(defn triple-check-apply
  "Applies a predicate to a concrete value"
  [predicate val]
  (predicate val val))

(defn triple-transform
  "Accepts a single argument function that will receive a triple and transform it"
  [f]
  (fn [triple atom] (f triple)))

;; predicates

(defn is-bnode?
  "Matches a blank node"
  []
  (fn [triple atom]
    (and (not (or (string? atom)
                  (keyword? atom)))
         (bnode? atom))))

(defn is-literal?
  "Matches a literal with a certain literal value"
  []
  (fn [triple atom]
    (and (instance? plaza.rdf.core.RDFResource atom)
         (rdf/literal? atom))))

(defn is-resource?
  "Matches a literal with a certain literal value"
  []
  (fn [triple atom]
    (and (instance? plaza.rdf.core.RDFResource atom)
         (rdf/resource? atom))))

(defn is-variable?
  "Matches a variable"
  []
  (fn [triple atom]
    (var-expr? *sparql-framework* atom)))

(defn is-optional?
  "Checks if a triple is an optional part of a query"
  []
  (fn [triple atom]
    (:optional (meta triple))))

(defn has-datatype?
  "Matches the value or the value and language of a literal"
  [data-uri]
  (fn [triple atom]
    (and (instance? plaza.rdf.core.RDFResource atom)
         (rdf/literal? atom)
         (= (find-datatype *rdf-model* (literal-datatype-uri atom))
            (find-datatype *rdf-model* data-uri)))))

(defn matches-bnode?
  "Matches a blank node"
  [id]
  (fn [triple atom]
    (and (not (or (string? atom) (keyword? atom)))
         (instance? plaza.rdf.core.RDFResource atom)
         (bnode? atom)
         (= (name id) (str (resource-id atom))))))

(defn matches-literal?
  "Matches the value or the value and language of a literal"
  ([val]
     (fn [triple atom]
       (and (instance? plaza.rdf.core.RDFResource atom)
            (rdf/literal? atom)
            (= (literal-value atom) val))))
  ([val lang]
     (fn [triple atom]
       (and (instance? plaza.rdf.core.RDFResource atom)
            (rdf/literal? atom)
            (= (literal-value atom) val)
            (= (literal-language atom) lang)))))

(defn matches-literal-value?
  "Matches a literal with a certain literal value"
  [lit]
  (fn [triple atom]
    (and (rdf/literal? atom)
         (= (literal-lexical-form atom)
            (str lit)))))

(defn matches-qname-local?
  "Matches a URI or curie against a triple atom"
  [local]
  (fn [triple atom]
    (and (not (or (keyword? atom)
                  (rdf/literal? atom)))
         (= (qname-local atom)
            (keyword->string local)))))

(defn matches-qname-prefix?
  "Matches a URI or curie against a triple atom"
  [prefix]
  (fn [triple atom]
    (and (not (or (keyword? atom) (rdf/literal? atom)))
         (= (qname-prefix atom)
            (or (find-ns-registry prefix)
                (keyword->string prefix))))))

(defn matches-regex?
  "Checks if a value matches a ceratin regular expression"
  [regex]
  (fn [triple atom]
    (not (empty? (re-find regex (str atom))))))

(defn matches-uri?
  "Matches a URI or curie against a triple atom"
  ([ns local]
     (matches-uri? (expand-ns ns local)))
  ([uri]
     (fn [triple atom]
       (and (not (or (keyword? atom)
                     (rdf/literal? atom)))
            (= (resource-id atom) uri)))))

(defn tca
  "Shortcut for triple-check-apply"
  [& args]
  (apply triple-check-apply args))

(defn tc
  "Shortcut for triple-check"
  [& args]
  (apply triple-check args))

(defn tt
  "Shortcut for triple-transform"
  [& args]
  (apply triple-transform args))
