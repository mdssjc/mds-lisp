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
  (let [f  (.get force)
        mf (.div (.get force) (float (:mass p)))]
    (assoc p :acceleration (.add (:acceleration p) mf))))

(defn update [p]
  (let [velocity (.add (:velocity p) (:acceleration p))
        position (.add (:position p) velocity)]
    (assoc p :position     position
             :velocity     velocity
             :acceleration (PVector. 0 0))))
