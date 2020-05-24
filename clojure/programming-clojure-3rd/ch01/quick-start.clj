(println "hello world")

(defn hello [name] (str "Hello, " name))
(hello "Stu")

(conj #{} "Stu")

(def visitors (atom #{}))

(swap! visitors conj "Stu")
(deref visitors)
@visitors

(defn hello
  "Writes hello message to *out*. Calls you by username. Knows if you have been here before."
  [username]
  (swap! visitors conj username)
  (str "Hello, " username))

(hello "Rich")
@visitors
