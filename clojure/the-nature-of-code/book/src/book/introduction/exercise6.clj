(ns book.introduction.exercise6
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Exercise I.6
;;
;; Use a custom probability distribution to vary the size of a step taken by the
;; random walker. The step size can be determined by influencing the range of
;; values picked. Can you map the probability exponentially—i.e. making the
;; likelihood that a value is picked equal to the value squared?

(defn setup []
  (assoc (w/setup)
         :x-prev (/ (q/width)  2.0)
         :y-prev (/ (q/height) 2.0)))

(defn draw [state]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/line (:x-prev state) (:y-prev state) (:x state) (:y state))
  (w/display state))

(defn montecarlo []
  (let [r1          (q/random 1)
        probability r1
        r2          (q/random 1)]

    (if (< r2 probability)
      r1
      (recur))))

(defn update-state [state]
  (assoc (w/step state
                 (fn [x y]
                   (let [stepsize (* (montecarlo) 10)
                         stepx    (q/random (- stepsize) stepsize)
                         stepy    (q/random (- stepsize) stepsize)]
                     [(+ x stepx) (+ y stepy)])))
         :x-prev (:x state)
         :y-prev (:y state)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
