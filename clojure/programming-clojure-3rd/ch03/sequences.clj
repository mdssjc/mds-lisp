(ns sequences
  (:require [clojure.string :refer :all])
  (:use [clojure.java.io :only (reader)])
  (:import java.io.File))

(let [m (re-matcher #"\w+" "the quick brown fox")]
  (loop [match (re-find m)]
    (when match
      (println match)
      (recur (re-find m)))))

(re-seq #"\w+" "the quick brown fox")
(sort (re-seq #"\w+" "the quick brown fox"))
(drop 2 (re-seq #"\w+" "the quick brown fox"))
(map upper-case (re-seq #"\w+" "the quick brown fox"))


(defn minutes-to-millis [mins]
  (* mins 1000 60))

(defn recently-modified? [file]
  (> (.lastModified file)
     (- (System/currentTimeMillis) (minutes-to-millis 30))))

(filter recently-modified? (file-seq (File. ".")))


(defn non-blank? [file]
  (not (blank? file)))

(defn non-svn? [file]
  (not (.contains (.toString file) ".svn")))

(defn clojure-source? [file]
  (.endsWith (.toString file) ".clj"))

(defn clojure-loc [base-file]
  (reduce + (for [file (file-seq base-file) :when (and (clojure-source? file)
                                                       (non-svn? file))]
              (with-open [rdr (reader file)]
                (count (filter non-blank? (line-seq rdr)))))))

(clojure-loc (File. "."))
