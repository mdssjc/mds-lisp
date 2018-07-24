(ns course.session2.particle
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-particle [m x y]
  {:mass         m
   :position     (new PVector x y)
   :velocity     (new PVector 0 0)
   :acceleration (new PVector 0 0)})

(defn apply-force
  "Newton's 2nd law: F = M * A or A = F / M"
  [p force]
  (let [f (PVector/div force (:mass p))]
    (assoc p {:acceleration (PVector/add p f)})))

(defn update [p]
  (let [velocity (PVector/add p (:acceleration p))
        position (PVector/add p velocity)]
    (assoc p
           {:position     position
            :velocity     velocity
            :acceleration 0})))

(defn display [p]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 255 127)
  (q/ellipse (get-in p [:position :x])
             (get-in p [:position :y])
             (* (:mass p) 16)
             (* (:mass p) 16)))

(defn check-edges [p]
  (if (> (get-in p [:position :y]) (q/height))
    (let [velocity-x (get-in p [:velocity :x])
          velocity-y (* (get-in p [:velocity :y]) -0.9)
          position-x (get-in p [:position :x])]
      (assoc p
             {:position (new PVector position-x (q/height))
              :velocity (new PVector velocity-x velocity-y)}))
    p))
