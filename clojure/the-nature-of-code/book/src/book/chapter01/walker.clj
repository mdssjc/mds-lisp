(ns book.chapter01.walker
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Example I.1: Traditional random walk

(def WIDTH  640)
(def HEIGHT 360)

(defrecord Walker [x y])

(defn rnd-4 [x y]
  (let [choice (int (q/random 4))]
    (cond (== choice 0) [(inc x) y]
          (== choice 1) [(dec x) y]
          (== choice 2) [x (inc y)]
          :else         [x (dec y)])))

(defn rnd-8a [x y]
  (let [stepx (+ x (- (int (q/random 3)) 1))
        stepy (+ y (- (int (q/random 3)) 1))]
    [stepx stepy]))

(defn rnd-8b [x y]
  (let [stepx (+ x (q/random -1 1))
        stepy (+ y (q/random -1 1))]
    [stepx stepy]))

(defn display [w]
  (q/stroke 0)
  (q/point (-> @w :x) (-> @w :y)))

(defn step [w func]
  (let [wx (-> @w :x)
        wy (-> @w :y)
        [x y] (func wx wy)]
    (swap! w assoc :x x)
    (swap! w assoc :y y)))

(def w (atom (Walker. 0 0)))

(defn setup []
  (swap! w assoc :x (/ WIDTH  2))
  (swap! w assoc :y (/ HEIGHT 2))
  (q/background 255))

(defn draw []
  (step    w rnd-8b)
  (display w))

(q/defsketch run
  :size [WIDTH HEIGHT]
  :setup setup
  :draw  draw)
