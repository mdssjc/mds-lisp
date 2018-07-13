(ns book.introduction.exampleI6
  (:require [quil.core :as q]))

;; Example I.6: 2D Perlin noise

(defn setup []
  (q/no-loop))

(defn draw []
  (q/background 255)
  (let [increment 0.02]
    (doseq [x (range (q/width))
            y (range (q/height))
            :let [bright (* (q/noise (* x increment) (* y increment)) 255)]]
      (q/set-pixel x y (q/color bright)))))

(q/defsketch run
  :size  [640 360]
  :setup setup
  :draw  draw)
