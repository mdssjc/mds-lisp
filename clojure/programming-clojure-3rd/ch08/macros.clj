(ns macros)

; This is doomed to fail...
(defn unless [expr form]
  (if expr nil form))

(unless false (println "this should print"))
(unless true (println "this should not print"))

(defn unless [expr form]
  (println "About to test...")
  (if expr nil form))

(unless false (println "this should print"))
(unless true (println "this should not print"))

(defmacro unless [expr form]
  (list 'if expr nil form))

(defmacro bad-unless [expr form]
  (list 'if 'expr nil form))

(defn with-out-str-as-fn [f]
  (let [s# (new java.io.StringWriter)]
    (binding [*out* s#]
      (f)
      (str s#))))

(defmacro evil-bench [expr]
  `(let [~'start (System/nanoTime)
         ~'result ~expr]
     {:result ~'result :elapsed (- (System/nanoTime) ~'start)}))

(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

(defn bench-fn [f]
  (let [start (System/nanoTime)
        result (f)]
     {:result result :elapsed (- (System/nanoTime) start)}))
