(ns book.introduction.walker
  (:require [quil.core :as q]))

(defn make-walker []
  {:x (/ (q/width)  2.0)
   :y (/ (q/height) 2.0)})

(defn setup []
  (q/background 255)
  (make-walker))

(defn step [walker f]
  (let [[x y] (f (:x walker)
                 (:y walker))
        cx    (q/constrain x 0 (- (q/width)  1))
        cy    (q/constrain y 0 (- (q/height) 1))]
    {:x cx
     :y cy}))

(defn display [walker]
  (q/stroke 0)
  (q/point (:x walker)
           (:y walker)))

;; Utils

(defn rnd-8a [x y]
  (let [stepx (+ x (- (int (q/random 3)) 1))
        stepy (+ y (- (int (q/random 3)) 1))]
    [stepx stepy]))

(defn rnd-8b [x y]
  (let [stepx (+ x (q/random -1 1))
        stepy (+ y (q/random -1 1))]
    [stepx stepy]))
