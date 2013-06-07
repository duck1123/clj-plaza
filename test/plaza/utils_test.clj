(ns plaza.utils-test
  (:use [plaza.utils :only [check-default-values cmd-param->keyword
                            flatten-1 fold-list]]
        [midje.sweet :only [=> fact]]))

(fact "check-default-values"
  (let [optsp (check-default-values {:password "foo"} {:password "bar" :username "guest"})]
    (:password optsp) => "foo"
    (:username optsp) => "guest"))

(fact "cmd-param->keywords"
  (cmd-param->keyword "hola") => "hola"
  (cmd-param->keyword :hola) => :hola
  (cmd-param->keyword "-hola") => :hola)

(fact "flatten-1"
  (->> [:a [[:s :p :o] [:s :p :o]] :c [[:s :p :o]] :d]
       flatten-1
       (into #{})) => #{:a [:s :p :o] :c :d})

(fact "flatten-1-preserves-meta"
  (let [to-flatten (with-meta [:a [[:s :p :o] [:s :p :o]] :c [[:s :p :o]] :d] {:flatten true})
        flattened (flatten-1 to-flatten)]
    (meta to-flatten) => (meta flattened)))

(fact "fold-list"
  (fold-list [1 2 3 4]) => [[1 2] [3 4]])
