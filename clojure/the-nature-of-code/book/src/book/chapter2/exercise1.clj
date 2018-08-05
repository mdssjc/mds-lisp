(ns book.chapter2.exercise1
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter2.mover :as mover]))

;; Exercise 2.1

;; Using forces, simulate a helium-filled balloon floating upward and bouncing
;; off the top of a window. Can you add a wind force that changes over time,
;; perhaps according to Perlin noise?

(defn setup []
  (assoc (mover/make-mover (/ (q/width)  2)
                           (/ (q/height) 2))
         :acceleration (PVector. 0 0.01)
         :xoff 0.0))

(defn draw [state]
  (q/background 255)

  (q/stroke 0 102 204)
  (q/fill   0 128 255)

  (let [x (.x (:location state))
        y (.y (:location state))]
    (q/ellipse x y 30 50)))

(defn check-edges [m]
  (let [x (.x (:location m))
        y (.y (:location m))
        w (q/width)
        h (q/height)]
    (assoc m :location (PVector. (cond (> x (+ w 15)) (+ w 15)
                                       (< x 15)       15
                                       :else          x)
                                 (cond (> y (- h 25)) (- h 25)
                                       (< y 25)       25
                                       :else          y)))))

(defn update-state [state]
  (let [wind   (PVector. (q/map-range (q/noise (:xoff state)) 0 1 -0.005 0.005) 0)
        helium (PVector. 0 -0.001)]
    (-> state
        (mover/apply-force wind)
        (mover/apply-force helium)
        (mover/update)
        (check-edges)
        (assoc :xoff (+ (:xoff state) 0.01)))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
