(ns course.session1.sketch
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [course.session1.walker :as w]))

(defn setup []
  (w/make-walker))

(defn draw [state]
  (q/background 51)
  (w/display state))

(defn update-state [state]
  (w/update state))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
