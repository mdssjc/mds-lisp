(ns pinger.scheduler
  (:import [java.util.concurrent Executors ExecutorService
            ScheduledExecutorService
            ScheduledFuture TimeUnit]))

(defn scheduled-executor
  "Create a scheduled executor."
  ^ScheduledExecutorService [threads]
  (Executors/newScheduledThreadPool threads))

(defn periodically
  "Schedule function f to run on executor e every 'delay'
  milliseconds after a delay of 'initial-delay' Returns
  a ScheduleFuture."
  ^ScheduledFuture
  [^ScheduledExecutorService e f initial-delay delay]
  (.scheduleWithFixedDelay e f initial-delay delay TimeUnit/MILLISECONDS))

(defn shutdown-executor
  "Shutdown an executor."
  [^ExecutorService e]
  (.shutdown e))
