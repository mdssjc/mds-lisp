(ns course.session2.liquid
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-liquid [x y w h c]
  {:x x
   :y y
   :w w
   :h h
   :c c})

(defn contains [l m]
  (let [ml (:position m)
        x  (.x ml)
        y  (.y ml)]
    (and (> x (:x l))
         (< x (+ (:x l)
                 (:w l)))
         (> y (:y l))
         (< y (+ (:y l)
                 (:h l))))))

(defn calculate-drag [l m]
  (let [speed          (.mag (:velocity m))
        drag-magnitude (* (:c l) speed speed)
        drag-force     (.copy (:velocity m))]
    (.setMag (.mult drag-force -1)) drag-magnitude))

(defn display [l]
  (q/no-stroke)
  (q/fill 50)
  (q/rect (:x l)
          (:y l)
          (:w l)
          (:h l)))
