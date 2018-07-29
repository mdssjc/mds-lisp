(ns course.session2.sketch-v3
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [course.session2.particle :as p]))

;; Array of Particles, multiple forces

(defn setup []
  [])

(defn draw [state]
  (q/background 51)

  (doseq [p state]
    (q/fill 255 150)
    (q/stroke 255)
    (q/ellipse (.x (:position p))
               (.y (:position p))
               (*  (:mass p) 10)
               (*  (:mass p) 10))
    state))

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
  (let [wind (PVector. 0.1 0)]
    (map (fn [p]
           (let [gravity (PVector. 0 (* 0.2 (:mass p)))]
             (check-edges
              (p/update
               (if (q/mouse-pressed?)
                 (p/apply-force (p/apply-force p wind) gravity)
                 (p/apply-force p gravity))))))
         state)))

(defn mouse-pressed [state event]
  (cons (p/make-particle (q/random 2 4)
                         (q/mouse-x)
                         (q/mouse-y))
        state))

(defn key-pressed [state event]
  (if (= (:key event) (keyword " "))
    (rest state)
    state))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :mouse-pressed mouse-pressed
  :key-pressed   key-pressed
  :middleware [m/fun-mode])
