(ns course.session2.sketch-v2
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [course.session2.particle :as p]
            [course.session2.attactor :as a]))

;; Gravitational Attraction

(defn setup []
  {:particle  (assoc (p/make-particle 1 400 50)
                     :velocity (PVector. 1 0))
   :attractor (a/make-attractor (/ (q/width)  2)
                                (/ (q/height) 2))})

(defn display [e m]
  (let [pos  (:position e)
        mass (:mass e)]
    (q/ellipse (.x pos)
               (.y pos)
               (* mass m)
               (* mass m))))

(defn draw [state]
  (q/background 51)

  ;; Particle
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 255 127)

  (display (:particle state) 16)

  ;; Attractor
  (q/ellipse-mode :center)
  (q/stroke-weight 4)
  (q/stroke 0)

  (display (:attractor state) 2))

(defn update-state [state]
  (let [particle (:particle state)
        force    (a/calculate-attraction (:attractor state) particle)]
    (assoc state :particle (p/update
                            (p/apply-force particle force)))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
