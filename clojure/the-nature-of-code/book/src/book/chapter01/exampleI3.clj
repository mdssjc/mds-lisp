(ns book.chapter01.exampleI3
  (:require [quil.core :as q]
            [book.chapter01.walker :as walker]))

;; Example I.3: Walker that tends to move to the right

(defn rnd-40r [x y]
  (let [r (q/random 1)]
    (cond (< r 0.4) [(inc x) y]
          (< r 0.6) [(dec x) y]
          (< r 0.8) [x (inc y)]
          :else     [x (dec y)])))

(q/defsketch run
  :size  walker/SIZE
  :setup walker/setup
  :draw  #(walker/draw rnd-40r))
