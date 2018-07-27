(ns course.session2.sketch-v2
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [course.session2.particle :as p]
            [course.session2.liquid :as l]))

;; Array of Particles, multiple forces

(defn setup []
  [])

(defn display [p]
  (q/fill 255 150)
  (q/stroke 255)
  (q/ellipse (.x (:position p))
             (.y (:position p))
             (* (:mass p) 10)
             (* (:mass p) 10)))

(defn draw [state]
  (q/background 51)

  (map display state))

(defn check-edges [p]
  (let [pos-x-gt-width?  (> (.x (:position p)) (q/width))
        pos-y-gt-height? (> (.y (:position p)) (q/height))
        pos-x (if pos-x-gt-width?  (q/width)  (.x (:position p)))
        pos-y (if pos-y-gt-height? (q/height) (.y (:position p)))
        vel-x (if pos-x-gt-width?  (* (.x (:velocity p)) -1) (.x (:velocity p)))
        vel-y (if pos-y-gt-height? (* (.y (:velocity p)) -1) (.y (:velocity p)))]
    (assoc p
           :position (PVector. pos-x pos-y)
           :velocity (PVector. vel-x vel-y))))

(defn update-state [state]
  state)

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
