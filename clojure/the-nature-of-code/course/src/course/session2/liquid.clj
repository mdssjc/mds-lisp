(ns course.session2.liquid
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-liquid [x y w h c]
  {:x x
   :y y
   :w w
   :h h
   :c c})

(defn contains [p m]
  (let [l (:position m)
        x (.x l)
        y (.y l)]
    (and (> x (:x p))
         (< x (+ (:x p)
                 (:w p)))
         (> y (:y p))
         (< y (+ (:y p)
                 (:h p))))))

(defn calculate-drag [p m]
  (let [speed          (.mag (:velocity m))
        drag-magnitude (* (:c p) speed speed)
        drag-force     (.copy (:velocity m))]
    (.setMag (PVector/mult drag-force -1)) drag-magnitude))

(defn display [p]
  (q/no-stroke)
  (q/fill 50)
  (q/rect (:x p)
          (:y p)
          (:w p)
          (:h p)))
