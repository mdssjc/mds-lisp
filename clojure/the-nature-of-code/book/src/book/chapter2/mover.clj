(ns book.chapter2.mover
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-mover []
  {:location     (PVector. 0 0)
   :velocity     (PVector. 0 0)
   :acceleration (PVector. 0 0)})

(defn apply-force
  "Newton's 2nd law: F = M * A or A = F / M"
  [m force]
  (assoc m :acceleration (.add (:acceleration m) force)))

(defn update [m]
  (let [velocity (.add (:velocity m)
                       (:acceleration m))]
    (assoc m
           :location     (.add (:location m) velocity)
           :velocity     velocity
           :acceleration (.mult (:acceleration m) 0))))

(defn display [m]
  (q/ellipse 0 0 0 0))


;; ---


(defn wind [m]
  (apply-force m (PVector. 0.5 0)))
