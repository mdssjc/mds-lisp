(ns concurrency
  (:require [chat :refer :all]))

(def counter (ref 0))

(defn next-counter []
  (dosync (alter counter inc)))

(next-counter)



(def backup-agent (agent "messages-backup.clj"))

(defn add-message-with-backup [msg]
  (dosync
   (let [snapshot (commute messages conj msg)]
     (send-off backup-agent (fn [filename]
                              (spit filename snapshot)
                              filename))
     snapshot)))

(add-message-with-backup (->Message "John" "Message One"))
(add-message-with-backup (->Message "Jane" "Message Two"))
