(ns course.session1.walker
  (:import  [processing.core PVector])
  (:require [quil.core :as q]))

(defn make-walker []
  (new PVector (/ (q/width) 2) (/ (q/height) 2)))

(defn update [w]
  (let [vel (new PVector (q/random -5 5) (q/random -5 5))]
    (.add w vel)))

(defn display [w]
  (q/fill 255)
  (q/ellipse (.x w) (.y w) 48 48))
