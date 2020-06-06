(ns main
  (:require [clojure.core.typed :refer :all]
            [clojure.core.reducers :refer :all]))

;;
;; Types and Functions
;;
;; Who Needs Types?
;; Types Are About Composability
;; What Are Types?
;; Why Do We Need a Mathematical Model?
;; Pure and Dirty Functions
;; Examples of Types
;;


(defn fact [n]
  (fold * (range 1 n)))

(fact 6)

(ann f44 [Nothing -> Number])
(defn f44 []
  44)

(f44)

(ann fInt [Number -> Nothing])
(defn fInt [_]
  '())

(fInt 1)
