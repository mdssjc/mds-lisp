(ns ch1.image
  (:require [clojure.java.io :as io])
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage]))

(defrecord PlanetImage [src ^BufferedImage contents])

(defn make-planet-image
  "Make a PlanetImage; may throw IOException"
  [src]
  (with-open [img (ImageIO/read (io/input-stream src))]
    (->PlanetImage src img)))
