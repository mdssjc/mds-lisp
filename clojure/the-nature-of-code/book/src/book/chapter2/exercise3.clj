(ns book.chapter2.exercise3
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter2.mover :as mover]))

;; Exercise 2.3
;; Instead of objects bouncing off the edge of the wall, create an example in
;; which an invisible force pushes back on the objects to keep them in the
;; window. Can you weight the force according to how far the object is from an
;; edge—i.e., the closer it is, the stronger the force?

(defn setup []
  (for [x (range 100)]
    (mover/make-mover (q/random 0.1 5) 0 0)))

(defn draw [state]
  (q/background 255)

  (q/stroke 0)
  (q/stroke-weight 2)
  (q/fill 127)

  (doseq [s state]
    (let [x (.x (:location s))
          y (.y (:location s))]
      (mover/display s))))

(defn check-edges [state]
  (let [lx (.x (:location state))
        ly (.y (:location state))
        w  (q/width)
        h  (q/height)
        f1 (PVector. (cond (< lx (* w 0.1))
                           (q/map-range lx 0 (* w 0.3) 0.3 0.1)
                           (> lx (* w 0.9))
                           (q/map-range lx w (* w 0.7) -0.3 -0.1)
                           :else 0)
                     0)
        f2 (PVector. 0
                     (cond (< ly (* h 0.1))
                           (q/map-range ly 0 (* h 0.1) 0.3  0.1)
                           (> ly (* h 0.9))
                           (q/map-range ly h (* h 0.9) -0.3 -0.1)
                           :else 0))]
    (mover/apply-force state
                       (.add f2 (.add (PVector. 0 0) f1)))))

(defn update-state [state]
  (let [wind    (PVector. 0.01 0)
        gravity (PVector. 0 0.1)]
    (map (fn [s]
           (-> s
               (mover/apply-force wind)
               (mover/apply-force gravity)
               (mover/update)
               (check-edges)))
         state)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
