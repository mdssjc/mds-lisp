(ns book.chapter2.example2
  (:import [processing.core PVector])
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter2.mover :as mover]))

;; Example 2.2: Forces acting on many objects

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

(defn update-state [state]
  (let [wind    (PVector. 0.01 0)
        gravity (PVector. 0 0.1)]
    (map (fn [s]
           (-> s
               (mover/apply-force wind)
               (mover/apply-force gravity)
               (mover/update)
               (mover/check-edges)))
         state)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
