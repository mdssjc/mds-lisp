(ns book.introduction.exerciseI6
  (:require [quil.core :as q]
            [book.introduction.walker :as walker]))

;; Exercise I.6

(def x-prev (atom 0.0))
(def y-prev (atom 0.0))

(defn montecarlo []
  (let [r1          (q/random 1)
        probability r1
        r2          (q/random 1)]

    (if (< r2 probability)
      r1
      (recur))))

(defn rnd-normal [x y]
  (let [stepsize (* (montecarlo) 10)
        stepx    (q/random (- stepsize) stepsize)
        stepy    (q/random (- stepsize) stepsize)]

    (swap! x-prev (fn [_] x))
    (swap! y-prev (fn [_] y))

    [(+ x stepx) (+ y stepy)]))

(defn draw [x y]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/line @x-prev @y-prev x y))

(q/defsketch run
  :size  walker/SIZE
  :setup walker/setup
  :draw  #(walker/draw rnd-normal draw))
