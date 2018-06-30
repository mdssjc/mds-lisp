(ns book.chapter1.exercise3
  (:use [book.chapter1.utils])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.pvector3d :as v3d]))

;; Exercise 1.3

(defn setup []
  (q/lights)
  {:location (v3d/make-pvector 100 100 0)
   :velocity (v3d/make-pvector 2.5 5 2.5)})

(defn draw [state]
  (q/background 255)

  (q/push-matrix)
  (q/translate (get-in state [:location :x])
               (get-in state [:location :y])
               (get-in state [:location :z]))
  (q/no-fill)
  (q/stroke 0)
  (q/sphere 20)
  (q/pop-matrix))

(defn update-state [state]
  (let [location (v3d/add (:location state) (:velocity state))]
    {:location location
     :velocity (v3d/make-pvector (check-bouncing (:x location)
                                                 (get-in state [:velocity :x])
                                                 q/width)
                                 (check-bouncing (:y location)
                                                 (get-in state [:velocity :y])
                                                 q/height)
                                 (check-bouncing (:z location)
                                                 (get-in state [:velocity :z])
                                                 -1000 300))}))

(q/defsketch run
  :size       [640 360]
  :renderer   :p3d
  :setup      setup
  :draw       draw
  :update     update-state
  :middleware [m/fun-mode])
