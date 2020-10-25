(ns chain-2)

(defmacro chain
  ([x form] (list '. x form))
  ([x form & more] (concat (list 'chain (list '. x form)) more)))

(macroexpand '(chain arm getHand))
(macroexpand '(chain arm getHand getFinger))
