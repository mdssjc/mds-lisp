(ns book.introduction.exerciseI9
  (:require [quil.core :as q]))

;; Exercise I.9

(def WIDTH  640)
(def HEIGHT 360)
(def increment 0.01)
(def z-increment 0.01)
(def z (atom 0))

(defn setup []
  ;; ---
  )

(defn draw []
  (q/background 0)
  (q/noise-detail 8 0.65)
  (q/pixels)
  (doseq [x (range WIDTH)
          y (range HEIGHT)]
    (let [bright (q/map-range (q/noise (* x increment) (* y increment) @z) 0 1 0 255)]
      (aset-int (q/pixels) (+ x (* y WIDTH)) (q/color bright))))
  (q/update-pixels)
  (swap! z (fn [_] (+ @z z-increment))))

(q/defsketch run
  :size  [WIDTH HEIGHT]
  :setup setup
  :draw  draw
  :renderer :p2d)
