(ns book.chapter1.pvector
  (:use clojure.test))

(defn make-pvector [x y]
  {:x x
   :y y})

(defn add [v1 v2]
  {:x (+ (:x v1) (:x v2))
   :y (+ (:y v1) (:y v2))})

(defn sub [v1 v2]
  {:x (- (:x v1) (:x v2))
   :y (- (:y v1) (:y v2))})



;; Unit Testing

(def a (make-pvector 5 2))
(def b (make-pvector 3 4))

(deftest pvector-tests
  (is {:x 8 :y 6}  (add a b))
  (is {:x 2 :y -2} (sub a b)))
