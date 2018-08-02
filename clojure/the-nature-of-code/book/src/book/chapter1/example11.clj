(ns book.chapter1.example11
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.mover :as mover]
            [book.chapter1.pvector :as v]))

;; Example 1.11: Array of movers accelerating towards the mouse

(defn setup []
  (for [i (range 20)]
    (assoc (mover/make-mover)
           :topspeed 4)))

(defn draw [state]
  (q/background 255)

  (doseq [s state]
    (mover/display s)))

(defn update-state [state]
  (map (fn [s]
         (mover/check-edges
          (let [mouse        (v/make-pvector (q/mouse-x)
                                             (q/mouse-y))
                dir          (v/sub mouse (:location s))
                acceleration (v/mult (v/normalize dir) 0.5)
                velocity     (v/limit (v/add (:velocity s)
                                             acceleration)
                                      (:topspeed s))]
            (assoc s
                   :location (v/add (:location s)
                                    velocity)
                   :velocity velocity
                   :acceleration acceleration))))
       state))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
