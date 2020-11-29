(ns multimethods.service-charge-2
  (:require multimethods.account))

(in-ns 'multimethods.account)
(clojure.core/use 'clojure.core)

(defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
(defmethod service-charge [::acc/basic ::acc/checking]   [_] 25)
(defmethod service-charge [::acc/basic ::acc/savings]    [_] 10)
(defmethod service-charge [::acc/premium ::acc/checking] [_] 0)
(defmethod service-charge [::acc/premium ::acc/savings]  [_] 0)

(service-charge {:tag ::acc/checking :balance 1000})
(service-charge {:tag ::acc/savings :balance 1000})
