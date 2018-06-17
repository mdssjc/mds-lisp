(ns book.chapter01.exampleI5
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Example I.5: Perlin noise walker

(def tx (atom 0.0))
(def ty (atom 10000.0))

(defn step [x y]
  (let [newx (q/map-range (q/noise @tx) 0 1 0 walker/WIDTH)
        newy (q/map-range (q/noise @ty) 0 1 0 walker/HEIGHT)]

    (swap! tx (fn [_] (+ @tx 0.01)))
    (swap! ty (fn [_] (+ @ty 0.01)))

    [newx newy]))

(defn draw [x y]
  (q/background 255)
  (q/ellipse x y 16 16))

(q/defsketch run
  :size  walker/SIZE
  :setup walker/setup
  :draw  #(walker/draw step draw))
