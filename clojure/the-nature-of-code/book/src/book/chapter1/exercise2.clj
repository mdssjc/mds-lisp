(ns book.chapter1.exercise2
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.PVector :as v]
            [book.chapter1.walker :as w]))

;; Exercise 1.2

(defn setup []
  (w/setup))

(defn draw [state]
  (w/display state))

(defn rnd-8b [state]
  (v/add state (v/make-pvector (q/random -1 1)
                               (q/random -1 1))))

(defn update-state [state]
  (w/step state rnd-8b))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
