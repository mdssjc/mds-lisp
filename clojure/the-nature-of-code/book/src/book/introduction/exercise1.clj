(ns book.introduction.exercise1
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Exercise I.1
;;
;; Create a random walker that has a tendency to move down and to the
;; right.

(defn setup []
  (w/setup))

(defn draw [state]
  (w/display state))

(defn update-state [state]
  (w/step state
          (fn [x y]
            (let [stepx (+ x (q/random -1 2))
                  stepy (+ y (q/random -1 2))]
              [stepx stepy]))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
