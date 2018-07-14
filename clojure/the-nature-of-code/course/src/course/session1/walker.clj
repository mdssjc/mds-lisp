(ns course.session1.walker
  (:import  [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-walker []
  {:pos  (new PVector (/ (q/width) 2) (/ (q/height) 2))
   :vel  (new PVector 0 0)
   :xoff  0.0
   :yoff 10.0})

(defn update [w]
  (let [x   (* (q/noise (:xoff w)) (q/width))
        y   (* (q/noise (:yoff w)) (q/height))
        xy  (new PVector x y)
        acc (.setMag (.sub xy (:pos w)) 0.4)
        vel (.add (:vel w) acc)
        pos (.add (:pos w) vel)]
    {:pos  pos
     :vel  vel
     :xoff (+ (:xoff w) 0.05)
     :yoff (+ (:yoff w) 0.05)}))

(defn display [w]
  (q/fill 255)
  (let [pos (:pos w)]
    (q/ellipse (.x pos) (.y pos) 48 48)))
