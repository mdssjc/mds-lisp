(ns book.chapter1.example1
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Example 1.1: Bouncing ball with no vectors

(defn setup []
  (q/background 255)
  (q/smooth)

  {:x       100.0
   :y       100.0
   :x-speed 2.5
   :y-speed 2.0})

(defn draw [state]
  (q/background 255)

  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)
  (q/ellipse (:x state) (:y state) 48 48))

(defn check-bouncing [pos speed f]
  (if (or (> pos (f))
          (< pos 0))
    (* speed -1)
    speed))

(defn update-state [state]
  (let [x-pos (+ (:x state) (:x-speed state))
        y-pos (+ (:y state) (:y-speed state))]
    {:x x-pos
     :y y-pos
     :x-speed (check-bouncing x-pos (:x-speed state) q/width)
     :y-speed (check-bouncing y-pos (:y-speed state) q/height)}))

(q/defsketch run
  :size   [800 200]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
