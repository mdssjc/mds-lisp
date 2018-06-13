(ns book.chapter01.exerciseI1
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter01.walker :as walker]))

;; Exercise I.1

(def WIDTH  640)
(def HEIGHT 360)

(defn rnd-dr [x y]
  (let [stepx (+ x (q/random -1 2))
        stepy (+ y (q/random -1 2))]
    [stepx stepy]))

(defn draw []
  (walker/step    walker/w rnd-dr)
  (walker/display walker/w))

(q/defsketch run
  :size [WIDTH HEIGHT]
  :setup walker/setup
  :draw  draw)
