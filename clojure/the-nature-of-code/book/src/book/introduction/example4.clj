(ns book.introduction.example4
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Example I.4: Gaussian distribution

(defn setup []
  (q/background 255)
  0)

(defn draw [state]
  (q/no-stroke)
  (q/fill 175 10)
  (q/ellipse state 180 16 16))

(defn update-state [state]
  (let [num  (q/random-gaussian)
        sd   60
        mean 320]
    (+ (* sd num) mean)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
