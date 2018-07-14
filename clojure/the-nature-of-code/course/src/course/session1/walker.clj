(ns course.session1.walker
  (:import  [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-walker []
  {:position     (new PVector (/ (q/width) 2) (/ (q/height) 2))
   :velocity     (new PVector 0 0)
   :acceleration (new PVector 0 0)
   :xoff         0.0
   :yoff         10.0})

(defn update [w]
  (let [x   (* (q/noise (:xoff w)) (q/width))
        y   (* (q/noise (:yoff w)) (q/height))
        acc (.setMag (.sub (new PVector x y) (:position w)) 0.4)
        vel (.add (:velocity w) acc)
        pos (.add (:position w) vel)]
    {:position     pos
     :velocity     vel
     :acceleration acc
     :xoff         (+ (:xoff w) 0.05)
     :yoff         (+ (:yoff w) 0.05)}))

(defn display [w]
  (q/fill 255)
  (let [pos (:position w)]
    (q/ellipse (.x pos) (.y pos) 48 48)))
