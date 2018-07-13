(ns book.introduction.exampleI3
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Example I.3: Walker that tends to move to the right

(defn setup []
  (w/setup))

(defn draw [state]
  (w/display state))

(defn update-state [state]
  (w/step state
          (fn [x y]
            (let [r (q/random 1)]
              (cond (< r 0.4) [(inc x) y]
                    (< r 0.6) [(dec x) y]
                    (< r 0.8) [x (inc y)]
                    :else     [x (dec y)])))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
