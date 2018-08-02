(ns book.chapter1.exercise8
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]
            [book.chapter1.pvector :as v]))

;; Exercise 1.8
;; Try implementing the above example with a variable magnitude of acceleration,
;; stronger when it is either closer or farther away.

(defn setup []
  (assoc (mover/make-mover)
         :location (v/make-pvector (/ (q/width)  2)
                                   (/ (q/height) 2))
         :acceleration (v/make-pvector -0.001 0.01)
         :topspeed 10))

(defn draw [state]
  (q/background 255)

  (mover/display state))

(defn update-state [state]
  (mover/check-edges
   (let [mouse           (v/make-pvector (q/mouse-x)
                                         (q/mouse-y))
         dir             (v/sub mouse (:location state))
         mag             (v/mag dir)
         max             1
         within-distance 25
         acceleration    (v/set-mag dir
                                    (if (< mag within-distance)
                                      (q/map-range mag 0 within-distance 0 max)
                                      max))
         velocity        (v/limit (v/add (:velocity state)
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
