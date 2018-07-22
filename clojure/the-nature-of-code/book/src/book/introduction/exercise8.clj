(ns book.introduction.exercise8
  (:require [quil.core :as q]))

;; Exercise I.8

(defn setup []
  (q/no-loop))

(defn draw []
  (q/background 0)
  (q/noise-detail 8 0.65)
  (let [increment 0.01]
    (doseq [x    (range (q/width))
            y    (range (q/height))
            :let [bright (q/map-range (q/noise (* x increment) (* y increment)) 0 1 0 255)]]
      (q/set-pixel x y (q/color bright)))))

(q/defsketch run
  :size  [640 360]
  :setup setup
  :draw  draw)
