(ns bench-1)

; This won't work
(defmacro bench [expr]
  `(let [start (System/nanoTime)
         result ~expr]
     {:result result :elapsed (- (System/nanoTime) start)}))

(bench (str "a" "b"))
(macroexpand-1 '(bench (str "a" "b")))

(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

(bench (str "a" "b"))
