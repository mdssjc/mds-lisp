(ns book.introduction.landscape
  (:require [quil.core :as q]))

;; The Nature of Code
;; Daniel Shiffman
;; http://natureofcode.com

;; "Landscape" example

(defn make-landscape [scl w h]
  {:scale scl
   :width w
   :height h
   :rows (/ h scl)
   :cols (/ w scl)
   :z-off 0.0
   :z []})

(defn calculate-z
  "Calculate height values (based off a neural network)"
  [state]
  (mapv
   (fn [y]
     (mapv
      (fn [x]
        (q/map-range (q/noise (* y 0.1) (* x 0.1) (:z-off state)) 0 1 -120 120))
      (range 0 (:rows state))))
   (range 0 (:cols state))))

(defn calculate-z-off
  "Calculate height values (based off a neural network)"
  [state]
  (+ (:z-off state) 0.01))

(defn render
  "Render landscape as grid of quads"
  [state]
  (let [scl (:scale state)]
    (doseq [x (range (- (count (:z state)) 1))
            y (range (- (count (get-in state [:z x])) 1))]
      (q/stroke 0)
      (q/fill 100 100)
      (q/push-matrix)
      (q/begin-shape :quads)
      (q/translate (- (* x scl) (/ (:width  state) 2))
                   (- (* y scl) (/ (:height state) 2))
                   0)
      (q/vertex 0   0   (get-in state [:z      x       y]))
      (q/vertex scl 0   (get-in state [:z (inc x)      y]))
      (q/vertex scl scl (get-in state [:z (inc x) (inc y)]))
      (q/vertex 0   scl (get-in state [:z      x  (inc y)]))
      (q/end-shape)
      (q/pop-matrix))))

(defn render.v2
  "Render landscape as grid of quads"
  [state]
  (let [scl (:scale state)]
    (doseq [x (range (- (count (:z state)) 1))]
      (q/begin-shape :quad-strip)
      (doseq [y (range (count (get-in state [:z x])))]
        (q/stroke 0)
        (let [currentElevation (get-in state [:z x y])
              currentShade     (q/map-range currentElevation -120 120 0 255)
              xCoordinate      (- (* x scl) (/ (:width  state) 2))
              yCoordinate      (- (* y scl) (/ (:height state) 2))]
          (q/fill currentShade 255)
          (q/vertex xCoordinate         yCoordinate (get-in state [:z      x  y]))
          (q/vertex (+ xCoordinate scl) yCoordinate (get-in state [:z (inc x) y]))))
      (q/end-shape))))
