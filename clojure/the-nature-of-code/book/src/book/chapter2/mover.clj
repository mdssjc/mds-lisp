(ns book.chapter2.mover
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-mover
  ([]
   {:location     (PVector. 30 30)
    :velocity     (PVector. 0 0)
    :acceleration (PVector. 0 0)
    :mass         1})
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
  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (.x (:location m))
             (.y (:location m))
             (*  (:mass m) 16)
             (*  (:mass m) 16)))

(defn check-edges [m]
  (let [lx (.x (:location m))
        ly (.y (:location m))
        vx (.x (:velocity m))
        vy (.y (:velocity m))
        w  (q/width)
        h  (q/height)]
    (assoc m
           :location (PVector. (cond (> lx w) w
                                     (< lx 0) 0
                                     :else    lx)
                               (cond (> ly h) h
                                     :else    ly))
           :velocity (PVector. (cond (> lx w) (* vx -1)
                                     (< lx 0) (* vx -1)
                                     :else    vx)
                               (cond (> ly h) (* vy -1)
                                     :else    vy)))))


;; ---


(defn wind [m]
  (apply-force m (PVector. 0.5 0)))
