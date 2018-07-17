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

(defn mult [v n]
  {:x (* (:x v) n)
   :y (* (:y v) n)})

(defn div [v n]
  {:x (/ (:x v) n)
   :y (/ (:y v) n)})

(defn mag [v]
  (let [x (:x v)
        y (:y v)]
    (Math/sqrt (+ (* x x)
                  (* y y)))))



;; Unit Testing

(def a (make-pvector 5 2))
(def b (make-pvector 3 4))

(deftest pvector-tests
  (is (= {:x 8   :y  6} (add  a b)))
  (is (= {:x 2   :y -2} (sub  a b)))
  (is (= {:x 10  :y  4} (mult a 2)))
  (is (= {:x 5/2 :y  1} (div  a 2)))
  (is (= 5.385164807134504 (mag a))))
