(ns chain-1)

; chain reimplements Clojure's .. macro
(defmacro chain [x form]
  (list '. x form))
