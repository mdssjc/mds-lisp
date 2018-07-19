(ns book.chapter1.mover
  (:require [quil.core :as q]
            [book.chapter1.pvector :as v]))

(defn make-mover []
  {:location (v/make-pvector (q/random (q/width))
                             (q/random (q/height)))
   :velocity (v/make-pvector (q/random -2 2)
                             (q/random -2 2))})

(defn update [m]
  {:location (v/add (:location m)
                    (:velocity m))
   :velocity (:velocity m)})

(defn display [m]
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (get-in m [:location :x])
             (get-in m [:location :y])
             16 16))

(defn check-edges [m]
  (let [x (get-in m [:location :x])
        y (get-in m [:location :y])]
    {:location (v/make-pvector
                (cond (> x (q/width)) 0
                      (< x 0)         (q/width)
                      :else           x)
                (cond (> y (q/height)) 0
                      (< y 0)          (q/height)
                      :else            y))
     :velocity (:velocity m)}))
