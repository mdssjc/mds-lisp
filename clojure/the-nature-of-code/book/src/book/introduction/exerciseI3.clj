(ns book.introduction.exerciseI3
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Exercise I.3

(defn setup []
  (w/setup))

(defn draw [state]
  (w/display state))

(defn update-by-mouse [x y]
  [(cond (> (q/mouse-x) x) (inc x)
         :else             (dec x))
   (cond (> (q/mouse-y) y) (inc y)
         :else             (dec y))])

(defn update-state [state]
  (w/step state
          (fn [x y]
            (let [x (:x state)
                  y (:y state)
                  r (q/random 1)]
              (cond (< r 0.125) [(inc x) y]
                    (< r 0.250) [(dec x) y]
                    (< r 0.375) [x (inc y)]
                    (< r 0.500) [x (dec y)]
                    :else       (update-by-mouse x y))))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
