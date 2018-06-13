(ns book.chapter01.randomDistribution
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;; Example I.2: Random number distribution

(def WIDTH  640)
(def HEIGHT 240)
(def LENGTH 20)

(def randomCounts (atom (vec (replicate LENGTH 0))))

(defn setup []
  ;; ---
  )

(defn draw []
  (q/background 255)
  (q/stroke 0)
  (q/fill 175) 
  
  (let [length (count @randomCounts)
        index  (int (q/random length))
        w      (/ WIDTH length)]
    (swap! randomCounts update index inc)
    
    (dotimes [x length]
      (let [element (get @randomCounts x)]
        (q/rect (* x w)
                (- HEIGHT element)
                (- w 1)
                element)))))

(q/defsketch run
  :size [WIDTH HEIGHT]
  :setup setup
  :draw  draw)
