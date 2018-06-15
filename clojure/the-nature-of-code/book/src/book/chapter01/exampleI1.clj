(ns book.chapter01.exampleI1
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Example I.1: Traditional random walk

(def WIDTH  640)
(def HEIGHT 360)

(defn rnd-4 [x y]
  (let [choice (int (q/random 4))]
    (cond (== choice 0) [(inc x) y]
          (== choice 1) [(dec x) y]
          (== choice 2) [x (inc y)]
          :else         [x (dec y)])))

(defn draw []
  (walker/step    walker/w rnd-4)
  (walker/display walker/w))

(q/defsketch run
  :size  [WIDTH HEIGHT]
  :setup walker/setup
  :draw  draw)
