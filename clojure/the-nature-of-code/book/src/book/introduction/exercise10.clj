(ns book.introduction.exercise10
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.landscape :as ls]))

;; Exercise I.10

(defn setup []
  (assoc (ls/make-landscape 20 800 400) :theta 0.0))

(defn draw [state]
  (q/background 255)
  (q/push-matrix)
  (q/translate (/ (:width state) 2)
               (+ (/ (:height state) 2) 20)
               -160)
  (q/rotate-x (/ (. Math PI) 3))
  (q/rotate-z (:theta state))
  (ls/render.v2 state)
  (q/pop-matrix))

(defn update-screen [state]
  (-> state
      (assoc  :z     (ls/calculate-z     state))
      (assoc  :z-off (ls/calculate-z-off state))
      (update :theta + 0.0025)))

(q/defsketch run
  :size   [800 200]
  :setup  setup
  :draw   draw
  :update update-screen
  :renderer :p3d
  :middleware [m/fun-mode])
