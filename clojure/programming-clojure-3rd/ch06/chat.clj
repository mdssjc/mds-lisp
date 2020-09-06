(ns chat)

(defrecord Message [sender text])

(->Message "Aaron" "Hello")

(defn valid-message? [msg]
  (and (:sender msg) (:text msg)))

(def validate-message-list #(every? valid-message? %))

(def messages (ref() :validator validate-message-list))

(defn add-message [msg]
  (dosync (alter messages conj msg)))
(defn add-message-commute [msg]
  (dosync (commute messages conj msg)))

(add-message (->Message "user 1" "hello"))
(add-message (->Message "user 2" "howdy"))
(add-message "not a valid message")
(add-message (->Message "Aaron" "Real Message"))
