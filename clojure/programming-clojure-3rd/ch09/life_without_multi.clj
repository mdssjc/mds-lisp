(ns life_without_multi
  (:require [clojure.string :as str]))

(defn my-print [ob]
  (.write *out* ob))

(defn my-println [ob]
  (my-print ob)
  (.write *out* "\n"))

(my-println "hello")

(defn my-print [ob]
  (cond
   (nil? ob) (.write *out* "nil")
   (string? ob) (.write *out* ob)))

(my-println [1 2 3])

(defn my-print-vector [ob]
  (.write *out*"[")
  (.write *out* (str/join " " ob))
  (.write *out* "]"))

(defn my-print [ob]
  (cond
   (vector? ob) (my-print-vector ob)
   (nil? ob) (.write *out* "nil")
   (string? ob) (.write *out* ob)))

(my-println [1 2 3])
