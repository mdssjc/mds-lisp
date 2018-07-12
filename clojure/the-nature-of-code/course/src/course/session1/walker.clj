(ns course.session1.walker
  (:import  [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-walker []
  {:pos (new PVector (/ (q/width) 2) (/ (q/height) 2))
   :vel (new PVector 0 0)})

(defn update [w]
  (let [mouse (new PVector (q/mouse-x) (q/mouse-y))
        acc   (.setMag (.sub mouse (:pos w)) 0.4)
        vel   (.add (:vel w) acc)
        pos   (.add (:pos w) vel)]
    {:pos pos
     :vel vel}))

(defn display [w]
  (q/fill 255)
  (let [pos (:pos w)]
    (q/ellipse (.x pos) (.y pos) 48 48)))
