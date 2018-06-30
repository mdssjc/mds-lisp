(ns book.chapter1.utils)

(defn- normalize [location velocity min max]
  (if (or (> location max)
          (< location min))
    (* velocity -1)
    velocity))

(defn check-bouncing
  ([location velocity dimension]
   (normalize location velocity 0 (dimension)))
  ([location velocity min max]
   (normalize location velocity min max)))
