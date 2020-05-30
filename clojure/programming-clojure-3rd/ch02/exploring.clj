(require '[clojure.string :as str])

(defn greeting
  "Returns a greeting of the form 'Hello, username'
  Default username is 'world'."
  ([] (greeting "world"))
  ([username]  (str "Hello, " username)))

(greeting "world")
(greeting)
(doc greeting)

(defn date [person-1 person-2 & chaperones]
  (println person-1 "and" person-2
           "went out with" (count chaperones) "chaperones."))

(date "Romeo" "Juliet" "Friar Lawrence" "Nurse")

(defn indexable-word?
  [word]
  (> (count word) 2))

(filter indexable-word? (str/split "A fine day it is" #"\W+"))
(filter (fn [w] (> (count w) 2)) (str/split "A fine day it is" #"\W+"))
(filter #(> (count %) 2) (str/split "A fine day it is" #"\W+"))

(defn indexable-words
  [text]
  (let [indexable-word? (fn [w] (> (count w) 2))]
    (filter indexable-word? (str/split text #"\W+"))))

(indexable-words "A fine day it is")

(defn make-greeter
  [greeting-prefix]
  (fn [username] [str greeting-prefix "," username]))

(def hello-greeting (make-greeter "Hello"))
(def aloha-greeting (make-greeter "Aloha"))

(hello-greeting "world")
(aloha-greeting "world")
((make-greeter "Howdy") "pardner")
