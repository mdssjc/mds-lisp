(ns book.introduction.exercise7
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Exercise I.7

(defn setup []
  (assoc (w/setup)
         :x-prev (/ (q/width)  2.0)
         :y-prev (/ (q/height) 2.0)
         :tx     0.0
         :ty     10000.0))

(defn draw [state]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/line (:x-prev state) (:y-prev state) (:x state) (:y state))
  (w/display state))

(defn update-state [state]
  (assoc (w/step state
                 (fn [x y]
                   [(q/map-range (q/noise (:tx state)) 0 1 0 (q/width))
                    (q/map-range (q/noise (:ty state)) 0 1 0 (q/height))]))
         :x-prev (:x state)
         :y-prev (:y state)
         :tx (+ (:tx state) 0.01)
         :ty (+ (:ty state) 0.01)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
