(ns book.chapter1.example8
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]
            [book.chapter1.pvector :as v]))

;; Example 1.9: Motion 101 (velocity and random acceleration)

(defn setup []
  (assoc (mover/make-mover)
         :topspeed 10))

(defn draw [state]
  (q/background 255)

  (mover/display state))

(defn update-state [state]
  (mover/check-edges
   (let [acceleration (v/mult (v/random2d) (q/random 2))
         velocity     (v/limit (v/add (:velocity state)
                                      acceleration)
                               (:topspeed state))]
     (assoc state
            :location (v/add (:location state)
                             velocity)
            :velocity velocity
            :acceleration acceleration))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
