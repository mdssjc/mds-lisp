(ns book.chapter01.exerciseI3
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter01.walker :as walker]))

;; Exercise I.3

(def WIDTH  640)
(def HEIGHT 360)

(defn updateByMouse [x y]
  [(cond (> (q/mouse-x) x) (inc x)
         :else (dec x))
   (cond (> (q/mouse-y) y) (inc y)
         :else (dec y))])

(defn rnd-dp [x y]
  (let [r (q/random 1)]
    (cond (< r 0.125) [(inc x) y]
          (< r 0.250) [(dec x) y]
          (< r 0.375) [x (inc y)]
          (< r 0.500) [x (dec y)]
          :else (updateByMouse x y))))

(defn draw []
  (walker/step    walker/w rnd-dp)
  (walker/display walker/w))

(q/defsketch run
  :size [WIDTH HEIGHT]
  :setup walker/setup
  :draw  draw)
