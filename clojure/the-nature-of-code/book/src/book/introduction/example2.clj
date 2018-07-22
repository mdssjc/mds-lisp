(ns book.introduction.example2
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Example I.2: Random number distribution

(defn setup []
  (vec (replicate 20 0)))

(defn draw [state]
  (q/background 255)
  (q/stroke 0)
  (q/fill 175)

  (let [length (count state)
        w      (/ (q/width) length)]
    (dotimes [x length]
      (let [element (get state x)]
        (q/rect (* x w)
                (- (q/height) element)
                (- w 1)
                element)))))

(defn update-state [state]
  (let [length (count state)
        index  (int (q/random length))]
    (update state index inc)))

(q/defsketch run
  :size   [640 240]
  :setup  setup
  :draw   draw
  :update update-state
  :middleware [m/fun-mode])
