(ns book.introduction.exerciseI9
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Exercise I.9

(defn setup []
  0)

(defn draw [state]
  (q/background 0)
  (q/noise-detail 8 0.65)
  (let [increment 0.01
        z         state]
    (doseq [x    (range (q/width))
            y    (range (q/height))
            :let [bright (q/map-range (q/noise (* x increment) (* y increment) z) 0 1 0 255)]]
      (q/set-pixel x y (q/color bright)))))

(defn update-state [state]
  (let [z           state
        z-increment 0.01]
    (+ z z-increment)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
