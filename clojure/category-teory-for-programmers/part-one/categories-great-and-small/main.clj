(ns main)

;;
;; Categories Great and Small
;;
;; No Objects
;; Simple Graphs
;; Orders
;; Monoid as Set
;; Monoid as Category
;; Challenges
;;


(defprotocol Monoid
   (mempty [a])
   (mappend [a b]))

(extend-protocol Monoid
  java.lang.Number
    (mempty [a] 0)
    (mappend [a b] (+ a b))
  java.lang.String
    (mempty [a] "")
    (mappend [a b] (str a b)))

(mempty 1)
(mappend 1 2)

(mempty "Hi!")
(mappend "Hello" "World")


;; Tests

;; AND
(extend-protocol Monoid
  java.lang.Boolean
  (mempty [a] true)
  (mappend [a b] (and a b)))

(mempty true)
(mappend false false)
(mappend false true)
(mappend true false)
(mappend true true)

;; OR
(extend-protocol Monoid
  java.lang.Boolean
  (mempty [a] true)
  (mappend [a b] (or a b)))

(mempty true)
(mappend false false)
(mappend false true)
(mappend true false)
(mappend true true)



;; Challenges
;; 1. Generate a free category from:
;; (a) A graph with one node and no edges
;; (b) A graph with one node and one (directed) edge (hint: this edge can be composed with itself)
;; (c) A graph with two nodes and a single arrow between them
;; (d) A graph with a single node and 26 arrows marked with the letters of the alphabet: a, b, c ... z.
;; 2. What kind of order is this?
;; (a) A set of sets with the inclusion relation: A is included in B if every element of A is also an element of B.
;; (b) C++ types with the following subtyping relation: T1 is a subtype of T2 if a pointer to T1 can be passed to a function that expects a pointer to T2 without triggering a compilation error.
;; 3. Considering that Bool is a set of two values True and False, show that it forms two (set-theoretical) monoids with respect to, respectively, operator && (AND) and || (OR).
;; 4. Represent the Bool monoid with the AND operator as a category: List the morphisms and their rules of composition.
;; 5. Represent addition modulo 3 as a monoid category.
