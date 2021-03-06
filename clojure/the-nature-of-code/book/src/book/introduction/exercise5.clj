(ns book.introduction.exercise5
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Exercise I.5
;;
;; A Gaussian random walk is defined as one in which the step size (how far the
;; object moves in a given direction) is generated with a normal distribution.
;; Implement this variation of our random walk.

(defn setup []
  (assoc (w/setup)
         :x-prev (/ (q/width)  2.0)
         :y-prev (/ (q/height) 2.0)))

(defn draw [state]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/line (:x-prev state) (:y-prev state) (:x state) (:y state))
  (w/display state))

(defn update-state [state]
  (assoc (w/step state
                 (fn [x y]
                   (let [num    (q/random-gaussian)
                         sd     2
                         mean   10
                         r      (q/random 1)
                         lenght (+ (* sd num) mean)]
                     (cond (< r 0.25) [(+ x lenght) y]
                           (< r 0.50) [(- x lenght) y]
                           (< r 0.75) [x (+ y lenght)]
                           :else      [x (- y lenght)]))))
         :x-prev (:x state)
         :y-prev (:y state)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
