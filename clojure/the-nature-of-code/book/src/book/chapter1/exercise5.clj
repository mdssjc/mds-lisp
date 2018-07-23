(ns book.chapter1.exercise5
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]
            [book.chapter1.pvector :as v]))

;; Exercise 1.5
;;
;; Create a simulation of a car (or runner) that accelerates when you press the
;; up key and brakes when you press the down key.

(defn setup []
  (assoc (mover/make-mover)
         :topspeed 20))

(defn draw [state]
  (q/background 255)

  (mover/display state))

(defn update-state [state]
  (mover/check-edges
   (let [velocity (v/limit (v/add (:velocity state)
                                  (:acceleration state))
                           (:topspeed state))]
     (assoc state
            :location (v/add (:location state)
                             velocity)
            :velocity velocity
            :acceleration (v/make-pvector 0.0 0.0)))))

(defn key-pressed [state event]
  (case (:key event)
    :up   (assoc state
                 :acceleration (v/make-pvector 0.0 1.0))
    :down (assoc state
                 :acceleration (v/mult (:velocity state) -0.25))
    state))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :key-pressed key-pressed
  :middleware [m/fun-mode])
