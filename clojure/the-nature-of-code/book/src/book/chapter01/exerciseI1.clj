(ns book.chapter01.exerciseI1
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Exercise I.1

(defn rnd-dr [x y]
  (let [stepx (+ x (q/random -1 2))
        stepy (+ y (q/random -1 2))]
    [stepx stepy]))

(q/defsketch run
  :size  walker/SIZE
  :setup walker/setup
  :draw  #(walker/draw rnd-dr))
