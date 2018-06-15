(ns book.chapter01.exampleI3
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Example I.3: Walker that tends to move to the right

(def WIDTH  640)
(def HEIGHT 360)

(defn rnd-40r [x y]
  (let [r (q/random 1)]
    (cond (< r 0.4) [(inc x) y]
          (< r 0.6) [(dec x) y]
          (< r 0.8) [x (inc y)]
          :else     [x (dec y)])))

(defn draw []
  (walker/step    walker/w rnd-40r)
  (walker/display walker/w))

(q/defsketch run
  :size  [WIDTH HEIGHT]
  :setup walker/setup
  :draw  draw)
