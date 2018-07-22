(ns book.chapter1.pvector)

(defn make-pvector [x y]
  {:x x
   :y y})

(defn add [v1 v2]
  {:x (+ (:x v1) (:x v2))
   :y (+ (:y v1) (:y v2))})

(defn sub [v1 v2]
  {:x (- (:x v1) (:x v2))
   :y (- (:y v1) (:y v2))})

(defn mult [v n]
  {:x (* (:x v) n)
   :y (* (:y v) n)})

(defn div [v n]
  {:x (/ (:x v) n)
   :y (/ (:y v) n)})

(defn mag [v]
  (let [x (:x v)
        y (:y v)]
    (Math/sqrt (+ (* x x)
                  (* y y)))))

(defn normalize [v]
  (let [m (mag v)]
    (div v m)))

(defn limit [v max]
  (if (> (mag v) max)
    (normalize v)
    (mult v max)))
