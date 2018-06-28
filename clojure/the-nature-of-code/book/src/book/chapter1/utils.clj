(ns book.chapter1.utils)

(defn check-bouncing [location velocity dimension]
  (if (or (> location (dimension))
          (< location 0))
    (* velocity -1)
    velocity))
