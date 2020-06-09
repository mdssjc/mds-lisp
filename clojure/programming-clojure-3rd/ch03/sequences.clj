(ns sequences
  (:require [clojure.string :refer :all]
            [clojure.set :as set])
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


(def song {:name "Agnus Dei"
           :artist "Krzysztof Penderecki"
           :album "Polish Requiem"
           :genre "Classical"})

(assoc song :kind "MPEG Audio File")
(dissoc song :genre)
(select-keys song [:name :artist])
(merge song {:size 8118166 :time 507245})
(merge-with
 concat
 {:rubble ["Barney"], :flintstone ["Fred"]}
 {:rubble ["Betty"], :flintstone ["Wilma"]}
 {:rubble ["Bam-Bam"], :flintstone ["Pebbles"]})


(def languages #{"java" "c" "d" "clojure"})
(def beverages #{"java" "chai" "pop"})

(set/union languages beverages)
(set/difference languages beverages)
(set/intersection languages beverages)
(set/select #(= 1 (count %)) languages)
