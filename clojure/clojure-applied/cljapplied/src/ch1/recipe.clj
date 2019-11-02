(ns ch1.recipe)

(defrecord Recipe
  [name        ;; string
   author      ;; recipe creator
   description ;; string
   ingredients ;; list of ingredients
   steps       ;; sequence of string
   servings    ;; number of servings
   ])

(defrecord Person
  [fname ;; first name
   lname ;; last name
   ])

(def toast
  (->Recipe
    "Toast"
    (->Person "Alex" "Miller")
    "Crispy bread"
    ["Slice of bread"]
    ["Toast bread in toaster"]
    1))

(def people
  {"p1" (->Person "Alex" "Miller")})

(def recipes
  {"r1" (->Recipe
          "Toast"
          "p1" ;; Person id
          "Crispy bread"
          ["Slice of bread"]
          ["Toast bread in toaster"]
          1)})
