(ns book.chapter2.example1
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter2.mover :as mover]))

;; Example 2.1: Forces
;; FIXME: corrigir a atualização dos valores do mover (com saltos)

(defn setup []
  (mover/make-mover 30 30))

(defn draw [state]
  (q/background 255)

  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)

  (let [x (.x (:location state))
        y (.y (:location state))]
    (q/ellipse x y 48 48)))

(defn check-edges [m]
  (let [lx (.x (:location m))
        ly (.y (:location m))
        v  (* (.x (:velocity m)) -1)
        w  (q/width)
        h  (q/height)]
    (assoc m
           :location (PVector. (cond (> lx w) w
                                     (< lx 0) 0
                                     :else    lx)
                               (cond (> ly h) h
                                     :else    ly))
           :velocity (PVector. (cond (> lx w) v
                                     (< lx 0) v
                                     :else    lx)
                               (cond (> ly h) (* (.y (:velocity m)) -1)
                                     :else    ly)))))

(defn update-state [state]
  (let [wind    (PVector. 0.01 0)
        gravity (PVector. 0 0.1)]
    (-> state
        (mover/apply-force wind)
        (mover/apply-force gravity)
        (mover/update)
        (check-edges))))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
