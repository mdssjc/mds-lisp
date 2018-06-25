(ns book.introduction.exampleI6
  (:require [quil.core :as q]))

;; Example I.6: 2D Perlin noise

(def WIDTH  640)
(def HEIGHT 360)
(def increment 0.02)

(defn setup []
  (q/no-loop))

(defn draw []
  (q/background 255)
  (doseq [x (range WIDTH)
          y (range HEIGHT)
          :let [bright (* (q/noise (* x increment) (* y increment)) 255)]]
    (q/set-pixel x y (q/color bright))))

(q/defsketch run
  :size  [WIDTH HEIGHT]
  :setup setup
  :draw  draw)
