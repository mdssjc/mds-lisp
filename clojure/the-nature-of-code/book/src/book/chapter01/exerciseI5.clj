(ns book.chapter01.exerciseI5
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Exercise I.5

(def x-prev (atom 0.0))
(def y-prev (atom 0.0))

(defn rnd-normal [x y]
  (let [num    (q/random-gaussian)
        sd     2
        mean   10
        r      (q/random 1)
        lenght (+ (* sd num) mean)]

    (swap! x-prev (fn [_] x))
    (swap! y-prev (fn [_] y))

    (cond (< r 0.25) [(+ x lenght) y]
          (< r 0.50) [(- x lenght) y]
          (< r 0.75) [x (+ y lenght)]
          :else      [x (- y lenght)])))

(defn draw [x y]
  (q/stroke 0)
  (q/stroke-weight 2)
  (q/line @x-prev @y-prev x y))

(q/defsketch run
  :size  walker/SIZE
  :setup walker/setup
  :draw  #(walker/draw rnd-normal draw))
