(ns book.chapter1.walker
  (:require [quil.core :as q]
            [book.chapter1.pvector :as v]))

(defn make-walker []
  (v/make-pvector (/ (q/width)  2.0)
                  (/ (q/height) 2.0)))

(defn setup []
  (q/background 255)
  (make-walker))

(defn step [w fun]
  (let [{x :x y :y} (fun w)
        cx          (q/constrain x 0 (- (q/width)  1))
        cy          (q/constrain y 0 (- (q/height) 1))]
    (v/make-pvector cx cy)))

(defn display [w]
  (q/stroke 0)
  (q/point (:x w)
           (:y w)))
