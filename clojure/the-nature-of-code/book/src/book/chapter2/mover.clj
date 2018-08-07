(ns book.chapter2.mover
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-mover
  ([]
   {:location     (PVector. (q/random (q/width))
                            (q/random (q/height)))
    :velocity     (PVector. 0 0)
    :acceleration (PVector. 0 0)
    :mass         10})
  ([x y]
   {:location     (PVector. x y)
    :velocity     (PVector. 0 0)
    :acceleration (PVector. 0 0)
    :mass         1})
  ([x y m]
   {:location     (PVector. x y)
    :velocity     (PVector. 0 0)
    :acceleration (PVector. 0 0)
    :mass         m}))

(defn apply-force
  "Newton's 2nd law: F = M * A or A = F / M"
  [m force]
  (assoc m :acceleration (.add (:acceleration m)
                               (.div (.get force)
                                     (:mass m)))))

(defn update [m]
  (let [acceleration (:acceleration m)
        velocity     (.add (:velocity m)
                           acceleration)]
    (assoc m
           :location     (.add (:location m) velocity)
           :velocity     velocity
           :acceleration (.mult acceleration 0))))

(defn display [m]
  (q/ellipse 0 0 0 0))


;; ---


(defn wind [m]
  (apply-force m (PVector. 0.5 0)))
