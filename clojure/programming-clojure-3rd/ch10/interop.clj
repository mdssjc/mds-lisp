(ns interop)

(defn say-hi []
  (println "Hello from thred" (.getName (Thread/currentThread))))

(dotimes [_ 3] (.start (Thread. say-hi)))

(defrecord Counter [n]
  Runnable
  (run [this] (println (range n))))

(def c(->Counter 5))
(.start (Thread. c))
(:n c)

(def c2 (assoc c :n 8))
(.start (Thread. c2))

(import '(org.xml.sax InputSource)
        '(org.xml.sax.helpers DefaultHandler)
        '(java.io StringReader)
        '(javax.xml.parsers SAXParserFactory))

(def print-element-handler
  (proxy [DefaultHandler] []
    (startElement [uri local qname atts]
      (println (format "Saw element: %s" qname)))))

(defn demo-sax-parse [string handler]
  (.. SAXParserFactory newInstance newSAXParser
      (parse (InputSource. (StringReader. string)) handler)))

(demo-sax-parse "
  <foo>
    <bar>Body of bar</bar>
  </foo>
" print-element-handler)

(defn demo-try-finally []
  (try
    (throw (Exception. "something failed"))
    (finally
      (println "we get to clean up"))))

; not caller-friendly
(defn class-available? [class-name]
  (Class/forName class-name))

(class-available? "borg.util.Assimilate")

(defn class-available? [class-name]
  (try
   (Class/forName class-name) true
   (catch ClassNotFoundException _ false)))

(class-available? "borg.util.Assimilate")
(class-available? "java.lang.String")

(defn describe-class [c]
  {:name (.getName c)
   :final (java.lang.reflect.Modifier/isFinal (.getModifiers c))})

(defn describe-class [#^Class c]
  {:name (.getName c)
   :final (java.lang.reflect.Modifier/isFinal (.getModifiers c))})

(describe-class StringBuffer)
(describe-class "foo")

; performance demo only, don't write code like this
(defn sum-to [n]
  (loop [i 1 sum 0]
    (if (<= i n)
      (recur (inc i) (+ i sum))
      sum)))

(sum-to 10)
(dotimes [_ 5] (time (sum-to 100000)))

(defn integer-sum-to ^long [^long n]
  (loop [i 1 sum 0]
    (if (<= i n)
      (recur (inc i) (+ i sum))
      sum)))

(integer-sum-to 10)
(dotimes [_ 5] (time (integer-sum-to 100000)))

(defn unchecked-sum-to ^long [^long n]
  (loop [i 1 sum 0]
    (if (<= i n)
      (recur (inc i) (unchecked-add i sum))
      sum)))

(unchecked-sum-to 10)
(dotimes [_ 5] (time (unchecked-sum-to 100000)))

(integer-sum-to 10000000000)
(unchecked-sum-to 10000000000)

(defn better-sum-to [n]
  (reduce + (range 1 (inc n))))

(dotimes [_ 5] (time (better-sum-to 100000)))

(defn best-sum-to [n]
  (/ (* n (inc n)) 2))

(dotimes [_ 5] (time (best-sum-to 100000)))

(defn painstakingly-create-array []
  (let [arr (make-array String 5)]
    (aset arr 0 "Painstaking")
    (aset arr 1 "to")
    (aset arr 2 "fill")
    (aset arr 3 "in")
    (aset arr 4 "arrays")
    arr))

(aget (painstakingly-create-array) 0)
(alength (painstakingly-create-array))
