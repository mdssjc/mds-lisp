(ns book.chapter1.example7
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]))

;; Example 1.7: Motion 101 (velocity)

(defn setup []
  (mover/make-mover))

(defn draw [state]
  (q/background 255)

  (mover/display state))

(defn update-state [state]
  (mover/check-edges (mover/update state)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
