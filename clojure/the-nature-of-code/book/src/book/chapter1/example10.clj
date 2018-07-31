(ns book.chapter1.example10
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]
            [book.chapter1.pvector :as v]))

;; Example 1.10: Accelerating towards the mouse

(defn setup []
  (assoc (mover/make-mover)
         :topspeed 5))

(defn draw [state]
  (q/background 255)

  (mover/display state))

(defn update-state [state]
  (mover/check-edges
   (let [mouse        (v/make-pvector (q/mouse-x)
                                      (q/mouse-y))
         dir          (v/sub mouse (:location state))
         acceleration (v/mult (v/normalize dir) 0.5)
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
