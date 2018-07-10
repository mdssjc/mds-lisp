(ns book.chapter1.example4
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [book.chapter1.pvector :as v]))

;; Example 1.4: Multiplying a vector

(defn setup []
  (v/make-pvector (q/mouse-x)
                  (q/mouse-y)))

(defn draw [state]
  (q/background 255)
  (q/translate (/ (q/width)  2)
               (/ (q/height) 2))
  (q/line 0 0 (:x state) (:y state)))

(defn update-state [state]
  (let [mouse  (v/make-pvector (q/mouse-x)
                               (q/mouse-y))
        center (v/make-pvector (/ (q/width)  2)
                               (/ (q/height) 2))]
    (v/mult (v/sub mouse center) 0.5)))

(q/defsketch run
  :size   [640 360]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
