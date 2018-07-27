(ns course.session2.sketch
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [course.session2.particle :as p]
            [course.session2.liquid :as l]))

;; Drag Force

(defn setup []
  {:particle1 (p/make-particle 1 200 0)
   :particle2 (p/make-particle 3 400 0)
   :liquid    (l/make-liquid 0 (/ (q/height) 2) (q/width) (/ (q/height) 2) 0.1)})

(defn display [p]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 255 127)
  (q/ellipse (.x (:position p))
             (.y (:position p))
             (* (:mass p) 16)
             (* (:mass p) 16)))

(defn draw [state]
  (q/background 127)

  (l/display (:liquid  state))
  (display (:particle1 state))
  (display (:particle2 state)))

(defn check-edges [p]
  (if (> (.y (:position p)) (q/height))
    (let [velocity-x (.x (:velocity p))
          velocity-y (* (.y (:velocity p)) -0.9)
          position-x (.x (:position p))]
      (assoc p :position (PVector. position-x (q/height))
               :velocity (PVector. velocity-x velocity-y)))
    p))

(defn do-force [l p]
  (let [contains?  (l/contains l p)
        gravity    (PVector. 0 (* 0.1 (:mass p)))
        drag-force (if contains? (l/calculate-drag l p))]
    (check-edges
     (p/update
      (if contains?
        (p/apply-force (p/apply-force p drag-force) gravity)
        (p/apply-force p gravity))))))

(defn update-state [state]
  (let [l (:liquid state)]
    {:particle1 (do-force l (:particle1 state))
     :particle2 (do-force l (:particle2 state))
     :liquid    l}))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
