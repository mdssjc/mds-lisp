(ns course.session2.attactor
  (:import [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-attractor [x y]
  {:position (PVector. x y)
   :mass     20
   :g        1})

(defn calculate-attraction [pi po]
  (let [force    (PVector/sub (:position pi)
                              (:position po))
        distance (q/constrain (.mag force) 5 25)
        strength (/ (* (:g    pi)
                       (:mass pi)
                       (:mass po))
                    (* distance distance))]
    (.mult (.normalize force) strength)))
