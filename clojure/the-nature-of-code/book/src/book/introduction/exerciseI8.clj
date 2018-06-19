(ns book.introduction.exerciseI8
  (:require [quil.core :as q]))

;; Exercise I.8

(def WIDTH  640)
(def HEIGHT 360)
(def increment 0.01)

(defn setup []
  (q/no-loop))

(defn draw []
  (q/background 0)
  (q/noise-detail 8 0.65)
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
