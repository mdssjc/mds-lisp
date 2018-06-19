(ns book.introduction.exampleI4
  (:require [quil.core :as q]))

;; Example I.4: Gaussian distribution

(def WIDTH  640)
(def HEIGHT 360)

(defn setup []
  (q/background 255))

(defn draw []
  (q/no-stroke)
  (q/fill 175 10)

  (let [num  (q/random-gaussian)
        sd   60
        mean 320
        x    (+ (* sd num) mean)]

    (q/ellipse x 180 16 16)))

(q/defsketch run
  :size  [WIDTH HEIGHT]
  :setup setup
  :draw  draw)
