(ns examples
  (:require [clojure.core.typed :as t]))

(t/ann f [A -> B])
(defn f [a]
  nil)

(t/ann g [B -> C])
(defn g [b]
  nil)
