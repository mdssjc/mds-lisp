(ns book.chapter01.exerciseI7
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Exercise I.7

(def x-prev (atom 0.0))
(def y-prev (atom 0.0))
(def tx     (atom 0.0))
(def ty     (atom 10000.0))

(defn noise [x y]
  (let [newx (q/map-range (q/noise @tx) 0 1 0 walker/WIDTH)
        newy (q/map-range (q/noise @ty) 0 1 0 walker/HEIGHT)]

    (swap! x-prev (fn [_] x))
    (swap! y-prev (fn [_] y))
    (swap! tx (fn [_] (+ @tx 0.01)))
    (swap! ty (fn [_] (+ @ty 0.01)))

    [newx newy]))

(defn draw [x y]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/line @x-prev @y-prev x y))

(q/defsketch run
  :size  walker/SIZE
  :setup walker/setup
  :draw  #(walker/draw noise draw))
