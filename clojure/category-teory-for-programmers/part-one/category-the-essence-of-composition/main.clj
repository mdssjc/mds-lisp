(ns main
  (:require [clojure.core.typed :refer :all]))

;;
;; Category: The Essence of Composition
;;
;; Arrows as Functions
;; Properties of Composition
;; Composition is the Essence of Programming
;; Challenges
;;


;; A, B, C and D are objects
;; f, g and h are morphisms

(ann f [A -> B])
(defn f [a]
  a)

(ann g [B -> C])
(defn g [b]
  b)

(ann h [C -> D])
(defn h [c]
  c)

;; Composition Function

(ann compose [f g -> x])
(defn compose [f g]
  (fn [x]
    (f (g x))))

(compose (compose f g) h)
(comp h g f)

;; Identity Function

(ann id [a -> a])
(defn id [a]
  a)

(id 10)
(identity 10)


;; Tests

(= (id ((compose inc inc) 2))
   (id ((comp inc inc) 2)))

((compose inc (partial * 2)) 2)
((comp inc (partial * 2)) 2)



;; To summarize: A category consists of objects and arrows (morphisms).
;; Arrows can be composed, and the composition is associative.
;; Every object has an identity arrow that serves as a unit under composition.

;; Challenges
;; 1. Implement, as best as you can, the identity function in your favorite language (or the second favorite, if your favorite language) happens to be Haskell.
;; 2. Implement the composition function in your favorite language. It takes two functions as arguments and returns a function that is their composition.
;; 3. Write a program that tries to test that your composition function respects identity.
;; 4. Is the world-wide web a category in any sense? Are links morphisms?
;; 5. Is Facebook a category, with people as objects and friendships as morphisms?
;; 6. When is a directed graph a category?
