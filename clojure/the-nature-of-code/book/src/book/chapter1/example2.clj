(ns book.chapter1.example2
  (:use [book.chapter1.utils])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.PVector :as v]))

;; Example 1.2: Bouncing ball with PVectors!

(defn setup []
  {:location (v/make-pvector 100 100)
   :velocity (v/make-pvector 2.5 5)})

(defn draw [state]
  (q/background 255)

  (q/stroke 0)
  (q/fill 175)
  (q/ellipse (get-in state [:location :x])
             (get-in state [:location :y])
             16 16))

(defn update-state [state]
  (let [location (v/add (:location state) (:velocity state))]
    {:location location
     :velocity (v/make-pvector (check-bouncing (:x location)
                                               (get-in state [:velocity :x])
                                               q/width)
                               (check-bouncing (:y location)
                                               (get-in state [:velocity :y])
                                               q/height))}))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
