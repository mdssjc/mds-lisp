(ns book.chapter1.exercise6
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]
            [book.chapter1.pvector :as v]))

;; Exercise 1.6
;;
;; Referring back to the Introduction, implement acceleration according to
;; Perlin noise.

(defn setup []
  (assoc (mover/make-mover)
         :topspeed 10
         :tx 0.0
         :ty 10000.0))

(defn draw [state]
  (q/background 255)

  (mover/display state))

(defn update-state [state]
  (mover/check-edges
   (let [tx           (:tx state)
         ty           (:ty state)
         acceleration (v/make-pvector (q/map-range (q/noise tx) 0 1 -1 1)
                                      (q/map-range (q/noise ty) 0 1 -1 1))
         velocity     (v/limit (v/add (:velocity state)
                                      acceleration)
                               (:topspeed state))]
     (assoc state
            :location (v/add (:location state)
                             velocity)
            :velocity velocity
            :acceleration acceleration
            :tx (+ tx 0.01)
            :ty (+ ty 0.01)))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
