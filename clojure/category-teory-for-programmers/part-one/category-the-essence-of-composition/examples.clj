(ns examples
  (:require [clojure.core.typed :as t]))

;; A, B, C and D are objects
;; f, g and h are morphisms

(t/ann f [A -> B])
(defn f [a]
  nil)

(t/ann g [B -> C])
(defn g [b]
  nil)

(t/ann h [C -> D])
(defn h [c]
  nil)

;; Composition
(comp h g f)

;; Identity Function

(t/ann id [a -> a])
(defn id [a]
  a)

(id 10)
(identity 10)

;; To summarize: A category consists of objects and arrows (morphisms).
;; Arrows can be composed, and the composition is associative.
;; Every object has an identity arrow that serves as a unit under composition.
