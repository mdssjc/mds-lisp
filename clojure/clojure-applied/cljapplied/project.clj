(defproject cljapplied "0.1.0-SNAPSHOT"
  :description "source projects for examples in Clojure Applied"
  :url "http://clojure-applied.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
    [[org.clojure/clojure "1.10.1"]
;
     [prismatic/schema "0.4.3"]
;
;
     [org.clojure/core.async "0.1.346.0-17112a-alpha"]
;
;
     [expectations "2.0.9"]
;
;
     [org.clojure/test.check "0.7.0"]
;
     [org.clojure/data.json "0.2.6"]
     [cheshire "5.4.0"]
     [medley "0.6.0"]
     [com.cognitect/transit-clj "0.8.271"]]

  :aot :all)
