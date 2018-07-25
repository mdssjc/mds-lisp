(ns course.session2.particle
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-particle [m x y]
  {:mass         m
   :position     (PVector. x y)
   :velocity     (PVector. 0 0)
   :acceleration (PVector. 0 0)})

(defn apply-force
  "Newton's 2nd law: F = M * A or A = F / M"
  [p force]
  (let [f (PVector/div force (:mass p))]
    (assoc p :acceleration (.add (:acceleration p) f))))

(defn update [p]
  (let [velocity (.add (:velocity p) (:acceleration p))
        position (.add (:position p) velocity)]
    (assoc p :position     position
             :velocity     velocity
             :acceleration 0)))

(defn display [p]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 255 127)
  (q/ellipse (.x (:position p))
             (.y (:position p))
             (* (:mass p) 16)
             (* (:mass p) 16)))

(defn check-edges [p]
  (if (> (.y (:position p)) (q/height))
    (let [velocity-x (.x (:velocity p))
          velocity-y (* (.y (:velocity p)) -0.9)
          position-x (.x (:position p))]
      (assoc p :position (PVector. position-x (q/height))
               :velocity (PVector. velocity-x velocity-y)))
    p))
