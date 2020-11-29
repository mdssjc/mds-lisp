(ns pinger.core
  (:require [pinger.scheduler :as scheduler])
  (:import [java.net URL HttpURLConnection]))

(defn response-code [address]
  (let [conn ^HttpURLConnection (.openConnection (URL. address))
        code (.getResponseCode conn)]
    (when (< code 400)
      (-> conn .getInputStream .close))
    code))

(response-code "http://google.com")

(defn avaliable? [address]
  (= 200 (response-code address)))

(avaliable? "http://google.com")
(avaliable? "http://google.com/badurl")

(defn check[]
  (let [addresses ["http://google.com"
                   "https://clojure.org"
                   "http://google.com/badurl"]]
    (doseq [address addresses]
      (println address ":" (avaliable? address)))))

(def immediately 0)
(def every-minute (* 60 1000))

(defn start[e]
  "REPL helper. Start pinger on executor e."
  (scheduler/periodically e check immediately every-minute))

(defn stop[e]
  "REPL helper. Stop executor e."
  (scheduler/shutdown-executor e))

(defn -main[]
  (start (scheduler/scheduled-executor 1)))
