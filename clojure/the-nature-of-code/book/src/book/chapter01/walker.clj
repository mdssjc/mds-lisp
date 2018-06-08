(ns book.chapter01.walker
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def WIDTH  640)
(def HEIGHT 360)

(defrecord Walker [x y])

(defn display [w]
  (q/stroke 0)
  (q/point (-> @w :x) (-> @w :y)))

(defn step [w]
  (let [wx (-> @w :x)
        wy (-> @w :y)
        choice (int (q/random 4))
        [x, y] (cond (== choice 0) [(inc wx), wy]
                     (== choice 1) [(dec wx), wy]
                     (== choice 2) [wx, (inc wy)]
                     :else         [wx, (dec wy)])]
    (swap! w assoc :x x)
    (swap! w assoc :y y)))

(def w (atom (Walker. 0 0)))

(defn setup []
  (swap! w assoc :x (/ WIDTH  2))
  (swap! w assoc :y (/ HEIGHT 2))
  (q/background 255))

(defn draw []
  (step    w)
  (display w))

(q/defsketch run
  :size [WIDTH HEIGHT]
  :setup setup
  :draw  draw)
