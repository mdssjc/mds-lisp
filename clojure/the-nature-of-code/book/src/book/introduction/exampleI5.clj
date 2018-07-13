(ns book.introduction.exampleI5
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.introduction.walker :as w]))

;; Example I.5: Perlin noise walker

(defn setup []
  (assoc (w/setup)
         :tx 0.0
         :ty 10000.0))

(defn draw [state]
  (q/background 255)
  (q/ellipse (:x state) (:y state) 16 16))

(defn update-state [state]
  (let [tx (:tx state)
        ty (:ty state)]
    (assoc (w/step state
                   (fn [x y]
                     (let [newx (q/map-range (q/noise tx) 0 1 0 (q/width))
                           newy (q/map-range (q/noise ty) 0 1 0 (q/height))]
                       [newx newy])))
           :tx (+ tx 0.01)
           :ty (+ ty 0.01))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
