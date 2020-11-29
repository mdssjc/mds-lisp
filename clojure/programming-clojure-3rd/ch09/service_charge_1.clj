(ns multimethods.service-charge-1
  (:require multimethods.account))

(in-ns 'multimethods.account)
(clojure.core/use 'clojure.core)

; bad approach
(defmulti service-charge account-level)
(defmethod service-charge ::basic [acct]
  (if (= (:tag acct) ::checking) 25 10))
(defmethod service-charge ::premium [_] 0)
