(ns book.chapter1.pvector3d
  (:use clojure.test))

(defn make-pvector [x y z]
  {:x x
   :y y
   :z z})

(defn add [v1 v2]
  {:x (+ (:x v1) (:x v2))
   :y (+ (:y v1) (:y v2))
   :z (+ (:z v1) (:z v2))})



;; Unit Testing

(def a (make-pvector 5 2 1))
(def b (make-pvector 3 4 3))

(deftest pvector-tests
  (is {:x 8 :y 6 :z 4} (add a b)))
