(ns functional)

(defn stack-consuming-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (stack-consuming-fibo (- n 1))
             (stack-consuming-fibo (- n 2)))))

(stack-consuming-fibo 9)
(stack-consuming-fibo 1000000)


(defn tail-fibo [n]
  (letfn [(fib [current next n]
            (if (zero? n)
              current
              (fib next (+ current next) (dec n))))]
    (fib 0N 1N n)))

(tail-fibo 9)
(tail-fibo 1000000)


(defn recur-fibo [n]
  (letfn [(fib [current next n]
            (if (zero? n)
              current
              (recur next (+ current next) (dec n))))]
    (fib 0N 1N n)))

(recur-fibo 9)
(recur-fibo 1000000)


(defn lazy-seq-fibo
  ([]
   (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
      (cons n (lazy-seq-fibo b n))))))

(take 10 (lazy-seq-fibo))
(rem (nth (lazy-seq-fibo) 1000000) 1000)

(take 5 (iterate (fn [[a b]] [b (+ a b)]) [0 1]))


(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(take 10 (fibo))


(def head-fibo(lazy-cat [0N 1N] (map + head-fibo (rest head-fibo))))

(take 10 head-fibo)
(nth head-fibo 1000000)


(defn count-heads-pairs [coll]
  (loop [cnt 0 coll coll]
    (if (empty? coll)
      cnt
      (recur (if (= :h (first coll) (second coll))
               (inc cnt)
               cnt)
             (rest coll)))))

(count-heads-pairs [:h :h :h :t :h])
(count-heads-pairs [:h :t :h :t :h])

(defn by-pairs [coll]
  (let [take-pair (fn [c]
                    (when (next c) (take 2 c)))]
    (lazy-seq (when-let [pair (seq (take-pair coll))]
                (cons pair (by-pairs (rest coll)))))))

(by-pairs [:h :t :t :h :h :h])

(defn count-heads-pairs [coll]
  (count (filter (fn [pair] (every? #(= :h %) pair))
                 (by-pairs coll))))

(count-heads-pairs [:h :h :h :t :h])
(count-heads-pairs [:h :t :h :t :h])

(partition 2 1 [:h :t :t :h :h :h])

(def ^{:doc "Count items matching a filter"}
  count-if (comp count filter))

(defn count-runs
  "Count runs of length n where pred is true in coll."
  [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))

(count-runs 2 #(= % :h) [:h :t :t :h :h :h])
(count-runs 2 #(= % :t) [:h :t :t :h :h :h])
(count-runs 3 #(= % :h) [:h :t :t :h :h :h])


(def ^{:doc "Count runs of length two that are both heads"}
  count-heads-pairs (partial count-runs 2 #(= % :h)))


(declare my-odd? my-even?)

(defn my-odd? [n]
  (if (= n 0)
    false
    (my-even? (dec n))))

(defn my-even? [n]
  (if (= n 0)
    true
    (my-odd? (dec n))))

(map my-even? (range 10))
(map my-odd? (range 10))
(my-even? (* 1000 1000 1000))

;; Converting to Self-recursion

(defn parity [n]
  (loop [n n par 0]
    (if (= n 0)
      par
      (recur (dec n) (- 1 par)))))

(map parity (range 10))

(defn my-even? [n] (= 0 (parity n)))
(defn my-odd? [n] (= 1 (parity n)))

;; Trampolining Mutual Recursion

(declare my-odd? my-even?)

(defn my-odd? [n]
  (if (= n 0)
    false
    #(my-even? (dec n))))

(defn my-even? [n]
  (if (= n 0)
    true
    #(my-odd? (dec n))))

(trampoline my-even? 1000000)

;; Replacing Rercusion with Laziness

(declare replace-symbol replace-symbol-expression)
(defn replace-symbol [coll oldsym newsym]
  (if (empty? coll)
    ()
    (cons (replace-symbol-expression (first coll) oldsym newsym)
          (replace-symbol (rest coll) oldsym newsym))))
(defn replace-symbol-expression [symbol-expr oldsym newsym]
  (if (symbol? symbol-expr)
    (if (= symbol-expr oldsym)
      newsym
      symbol-expr)
    (replace-symbol symbol-expr oldsym newsym)))

(defn deeply-nested [n]
  (loop [n n result '(bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))

(defn- coll-or-scalar [x & _] (if (coll? x) :collection :scalar))
(defmulti replace-symbol coll-or-scalar)
(defmethod replace-symbol :collection [coll oldsym newsym]
  (lazy-seq (when (seq coll)
              (cons (replace-symbol (first coll) oldsym newsym)
                    (replace-symbol (rest coll) oldsym newsym)))))
(defmethod replace-symbol :scalar [obj oldsym newsym]
  (if (= obj oldsym)
    newsym
    obj))

(set! *print-level* 25)

(deeply-nested 5)
(deeply-nested 25)

(replace-symbol (deeply-nested 5) 'bottom 'deepest)
(replace-symbol (deeply-nested 10000) 'bottom 'deepest)

;; Shortcutting Recursion with Memoization

(declare m f)
(defn m [n]
  (if (zero? n)
    0
    (- n (f (m (dec n))))))
(defn f [n]
  (if (zero? n)
    1
    (- n (m (f (dec n))))))

(def m (memoize m))
(def f (memoize f))

(def m-seq (map m (iterate inc 0)))
(def f-seq (map f (iterate inc 0)))

(time (m 250))
(m 10000)
(nth m-seq 250)
(time (nth m-seq 10000))


(defn square [x] (* x x))
(defn sum-squares-seq [n]
  (vec (map square (range n))))
(defn sum-squares [n]
  (into [] (map square) (range n)))

(defn preds-seq []
  (->> (all-ns)
       (map ns-publics)
       (mapcat vals)
       (filter #(clojure.string/ends-with? % "?"))
       (map #(str (.-sym %)))
       vec))

(defn preds[]
  (into []
       (comp (map ns-publics)
             (mapcat vals)
             (filter #(clojure.string/ends-with? % "?"))
             (map #(str (.-sym %)))
             (all-ns))))


(defn non-blank? [s]
  (not (clojure.string/blank? s)))

(defn non-blank-lines-seq [file-name]
  (let [reader (clojure.java.io/reader file-name)]
    (filter non-blank? (line-seq reader))))

(defn non-blank-lines [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (into [] (filter non-blank?) (line-seq reader))))

(defn non-blank-lines-eduction [reader]
  (eduction (filter non-blank?) (line-seq reader)))

(defn line-count [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (reduce (fn [cnt el] (inc cnt)) 0 (non-blank-lines-eduction reader))))
