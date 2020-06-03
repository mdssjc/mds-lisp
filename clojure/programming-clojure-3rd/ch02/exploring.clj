(ns exploring
  (:require [clojure.string :as str])
  (:import (java.io File)))

(defn greeting
  "Returns a greeting of the form 'Hello, username'
  Default username is 'world'."
  ([] (greeting "world"))
  ([username] (str "Hello, " username)))

(greeting "world")
(greeting)
;;(doc greeting)

(defn date [person-1 person-2 & chaperones]
  (println person-1 "and" person-2
           "went out with" (count chaperones) "chaperones."))

(date "Romeo" "Juliet" "Friar Lawrence" "Nurse")

(defn indexable-word? [word]
  (> (count word) 2))

(filter indexable-word? (str/split "A fine day it is" #"\W+"))
(filter (fn [w] (> (count w) 2)) (str/split "A fine day it is" #"\W+"))
(filter #(> (count %) 2) (str/split "A fine day it is" #"\W+"))

(defn indexable-words [text]
  (let [indexable-word? (fn [w] (> (count w) 2))]
    (filter indexable-word? (str/split text #"\W+"))))

(indexable-words "A fine day it is")

(defn make-greeter [greeting-prefix]
  (fn [username] [str greeting-prefix "," username]))

(def hello-greeting (make-greeter "Hello"))
(def aloha-greeting (make-greeter "Aloha"))

(hello-greeting "world")
(aloha-greeting "world")
((make-greeter "Howdy") "pardner")

(defn square-corners [bottom left size]
  (let [top (+ bottom size)
        right (+ left size)]
    [[bottom left] [top left] [top right] [bottom right]]))

(square-corners 10 5 12)

(defn greet-author-1 [author]
  (println "Hello," (:first-name author)))

(defn greet-author-2 [{fname :first-name}]
  (println "Hello," fname))

(greet-author-1 {:last-name "Vinge" :first-name "Vernor"})
(greet-author-2 {:last-name "Vinge" :first-name "Vernor"})

(defn ellipsize [words]
  (let [[w1 w2 w3] (str/split words #"\s+")]
    (str/join " " [w1 w2 w3 "..."])))

(ellipsize "The quick brown fox jumps over the lazy dog.")

(defn shout
  ([s] (clojure.string/upper-case s))
  {:tag String})
(meta #'shout)

(defn is-small? [number]
  (if (< number 100)
    "yes"
    (do
      (println "Saw a big number" number)
      "no")))

(is-small? 50)
(is-small? 50000)

(loop [result [] x 5]
  (if (zero? x)
    result
    (recur (conj result x) (dec x))))

(defn countdown [result x]
  (if (zero? x)
    result
    (recur (conj result x) (dec x))))

(countdown [] 5)

(into [] (take 5 (iterate dec 5)))
(into [] (drop-last (reverse (range 6))))
(vec (reverse (rest (range 6))))

(defn indexed [coll] (map-indexed vector coll))
(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))
(defn index-of-any [pred coll]
  (first (index-filter pred coll)))

(indexed "abcde")
(index-filter #{\a \b} "abcdbbb")
(index-filter #{\a \b} "xyz")
(index-of-any #{\z \a} "zzabyycdxx")
(index-of-any #{\b \y} "zzabyycdxx")
(nth (index-filter #{:h} [:t :t :h :t :h :t :t :t :h :h]) 2)
