(ns book.introduction.exampleI1
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Example I.1: Traditional random walk

(defn setup []
  (w/setup))

(defn draw [state]
  (w/display state))

(defn update-state [state]
  (w/step state
          (fn [x y]
            (let [choice (int (q/random 4))]
              (cond (== choice 0) [(inc x) y]
                    (== choice 1) [(dec x) y]
                    (== choice 2) [x (inc y)]
                    :else         [x (dec y)])))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
