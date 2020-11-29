(ns multimethods
  (:require [clojure.string :as str]))

(defmulti my-print class)

(my-print "foo")

(defmethod my-print String [s]
  (.write *out* s))

(my-print "stu")

(defmethod my-print nil [s]
  (.write *out* "nil"))

(defmethod my-print Number [n]
  (.write *out* (.toString n)))

(my-print 42)

(defmethod my-print :default [s]
  (.write *out* "#<")
  (.write *out* (.toString s))
  (.write *out* ">"))

(my-print (java.sql.Date. 0))
(my-print (java.util.Random.))

(defmethod my-print java.util.Collection [c]
  (.write *out* "(")
  (.write *out* (str/join " " c))
  (.write *out* ")"))

(my-print (take 6 (cycle [1 2 3])))
(my-print [1 2 3])

(defmethod my-print clojure.lang.IPersistentVector [c]
  (.write *out* "[")
  (.write *out* (str/join " " c))
  (.write *out* "]"))

(my-print [1 2 3])

(prefer-method my-print clojure.lang.IPersistentVector java.util.Collection)

(my-print (take 6 (cycle [1 2 3])))
(my-print [1 2 3])

(defmulti my-class identity)
(defmethod my-class nil [_] nil)
(defmethod my-class :default [x] (.getClass x))
