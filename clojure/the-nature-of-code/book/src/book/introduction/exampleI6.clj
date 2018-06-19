(ns book.introduction.exampleI6
  (:require [quil.core :as q]))

;; Example I.6: 2D Perlin noise
;; FIXME: problemas na renderização do sketch

(def WIDTH  640)
(def HEIGHT 360)
(def increment 0.02)

(defn setup []
  (q/no-loop))

(defn draw []
  (q/background 255)
  (q/pixels)
  (doseq [x (range WIDTH)
          y (range HEIGHT)]
    (let [bright (q/map-range (q/noise (* x increment) (* y increment)) 0 1 0 255)]
      (aset-int (q/pixels) (+ x (* y WIDTH)) (q/color bright))))
  (q/update-pixels))

(q/defsketch run
  :size  [WIDTH HEIGHT]
  :setup setup
  :draw  draw
  :renderer :p2d)
