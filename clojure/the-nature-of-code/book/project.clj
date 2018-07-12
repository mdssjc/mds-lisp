(defproject book "0.1.0-SNAPSHOT"
  :description "Estudo da obra The Nature of Code"
  :url "https://github.com/mdssjc/mds-lisp"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0-alpha6"]
                 [quil "2.7.1"]
                 [cider/orchard "0.4.0-SNAPSHOT"]]
  :plugins [[refactor-nrepl    "2.4.0-SNAPSHOT"]
            [cider/cider-nrepl "0.18.0-SNAPSHOT"]]
  :main book.core)
