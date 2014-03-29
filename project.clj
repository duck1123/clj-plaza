(defproject net.kronkltd/plaza "0.3.0-SNAPSHOT"
  :description "Plaza framework for semantic distributed applications"
  :url "http://github.com/duck1123/clj-plaza"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/data.json "0.2.4"]
                 [org.clojure/tools.logging "0.2.6"]
                 [org.apache.jena/jena-core "2.11.1"]
                 [org.apache.jena/jena-arq "2.11.1"]
                 [net.rootdev/java-rdfa "0.4.2"]
                 [com.franz/openrdf-sesame-onejar "2.3.1"]]
  :min-lein-version "2.0.0"
  :repositories {"jboss" "http://repository.jboss.org/nexus/content/groups/public/"
                 "apache-repo-release" "https://repository.apache.org/content/repositories/releases/"}
  :profiles {:dev
             {:dependencies
              [[log4j "1.2.17"]
               [midje "1.6.3"]]}}
  :plugins [[lein-midje "3.0.1"]
            [codox "0.6.1"]]
  :autodoc {:name "clj-plaza",
            :page-title "clj-plaza distributed semantic systems library"
            :author "Antonio Garrote <antoniogarrote@gmail.com> <agarrote@usal.es>"
            :copyright "2010 (c) Antonio Garrote, under the MIT license"
            :web-home "http://antoniogarrote.github.com/clj-plaza/api"})
