(ns book.chapter1.example5
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.pvector :as v]))

;; Example 1.5: Vector magnitude

(defn setup []
  (v/make-pvector (q/mouse-x)
                  (q/mouse-y)))

(defn draw [state]
  (q/background 255)

  (let [m (v/mag state)]
    (q/fill 0)
    (q/rect 0 0 m 10))

  (q/translate (/ (q/width) 2) (/ (q/height) 2))
  (q/line 0 0 (:x state) (:y state)))

(defn update-state [state]
  (let [mouse  (v/make-pvector (q/mouse-x)
                               (q/mouse-y))
        center (v/make-pvector (/ (q/width)  2)
                               (/ (q/height) 2))]
    (v/sub mouse center)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
