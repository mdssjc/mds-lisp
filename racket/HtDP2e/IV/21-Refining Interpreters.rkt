;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |21-Refining Interpreters|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 21-Refining Interpreters.rkt
;; IV - Intertwined Data
;; 21 - Refining Interpreters



;; 21.1 - Interpreting Expressions

;; Exercise 345


;; =================
;; Data definitions:

(define-struct add [left right])
; An Add is a structure:
;   (make-add BSL-expr BSL-expr)
; interpretation (make-add l r) specifies an addition expression
;  l: is the left operand; and
;  r: is the right operand

(define-struct mul [left right])
; A Mul is a structure:
;   (make-mul BSL-expr BSL-expr)
; interpretation (make-mul l r) specifies a multiplication expression
;  l: is the left operand; and
;  r: is the right operand

; A BSL-expr is one of:
;  - Number
;  - (make-add BSL-expr BSL-expr)
;  - (make-mul BSL-expr BSL-expr)
; interpretation class of values to which a representation of a BSL expression can evaluate

(define EXPR1 (make-add 10 -10))
(define EXPR2 (make-add (make-mul 20 3) 33))
(define EXPR3 (make-add (make-mul 3.14 (make-mul 2 3)) (make-mul 3.14 (make-mul -1 -9))))
; (+ -1 2)
; (+ (* -2 -3) 33)
; (* (+ 1 (* 2 3)) 3.14)


;; =================
;; Functions:

; BSL-expr -> Number
; computes its value
(check-expect (calculator 3) 3)
(check-expect (calculator (make-add 1 1)) 2)
(check-expect (calculator (make-mul 3 10)) 30)
(check-expect (calculator (make-add (make-mul 1 1) 10)) 11)

(define (calculator expr)
  (cond [(number? expr) expr]
        [(add? expr)
         (+ (calculator (add-left  expr))
            (calculator (add-right expr)))]
        [(mul? expr)
         (* (calculator (mul-left  expr))
            (calculator (mul-right expr)))]))

;; Exercise 346


;; =================
;; Data definitions:

(define-struct div [left right])
; A Div is a structure:
;   (make-div BSL-expr BSL-expr)
; interpretation (make-div l r) specifies a division expression
;  l: is the left operand; and
;  r: is the right operand

; A BSL-expr is one of:
;  - Number
;  - (make-add BSL-expr BSL-expr)
;  - (make-mul BSL-expr BSL-expr)
;  - (make-div BSL-expr BSL-expr)
; interpretation class of values to which a representation of a BSL expression can evaluate

(define EXPR4 (make-div (make-mul 4 2) 8))
; (/ (* 4 2) 8)

;; Exercise 347


;; =================
;; Functions:

; BSL-expr -> Number
; computes its value
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression EXPR1) 0)
(check-expect (eval-expression EXPR2) 93)
(check-expect (eval-expression EXPR3) 47.1)
(check-expect (eval-expression EXPR4) 1)

(define (eval-expression expr)
  (cond [(number? expr) expr]
        [(add? expr)
         (+ (eval-expression (add-left  expr))
            (eval-expression (add-right expr)))]
        [(mul? expr)
         (* (eval-expression (mul-left  expr))
            (eval-expression (mul-right expr)))]
        [(div? expr)
         (/ (eval-expression (div-left  expr))
            (eval-expression (div-right expr)))]))

;; Exercise 348


;; =================
;; Data definitions:

(define-struct and2 [left right])
; An And is a structure:
;   (make-and BSL-boolean-expr BSL-boolean-expr)
; interpretation (make-and2 l r) specifies an AND operation
;  l: is the left operand; and
;  r: is the right operand

(define-struct or2 [left right])
; An Or is a structure:
;   (make-or BSL-boolean-expr BSL-boolean-expr)
; interpretation (make-or2 l r) specifies an OR operation
;  l: is the left operand; and
;  r: is the right operand

(define-struct not2 [operand])
; A Not is a structure:
;   (make-not BSL-boolean-expr)
; interpretation (make-not2 o) specifies a NOT operation
;  o: is an operand

; A BSL-boolean-expr is one of:
;  - #false
;  - #true
;  - (make-and2 BSL-boolean-expr BSL-boolean-expr)
;  - (make-or2  BSL-boolean-expr BSL-boolean-expr)
;  - (make-not2 BSL-boolean-expr)
; interpretation class of values to which a representation of a BSL boolean expression can evaluate


;; =================
;; Functions:

; BSL-boolean-expr -> Boolean
; computes its value
(check-expect (eval-bool-expression #false) #false)
(check-expect (eval-bool-expression #true) #true)
(check-expect (eval-bool-expression (make-and2 #true    #true))  #true)
(check-expect (eval-bool-expression (make-and2 #true    #false)) #false)
(check-expect (eval-bool-expression (make-or2  #true    #false)) #true)
(check-expect (eval-bool-expression (make-or2  #false   #false)) #false)
(check-expect (eval-bool-expression (make-not2 #false)) #true)
(check-expect (eval-bool-expression (make-not2 #true))  #false)
(check-expect (eval-bool-expression (make-not2 (make-and2 #true (make-or2 #false #true)))) #false)

(define (eval-bool-expression expr)
  (cond [(boolean? expr) expr]
        [(and2? expr)
         (and (eval-bool-expression (and2-left  expr))
              (eval-bool-expression (and2-right expr)))]
        [(or2? expr)
         (or (eval-bool-expression (or2-left  expr))
             (eval-bool-expression (or2-right expr)))]
        [(not2? expr)
         (not (eval-bool-expression (not2-operand expr)))]))

;; Exercise 349


;; =================
;; Constants:

(define WRONG "Invalid datatype")


;; =================
;; Data definitions:

; An S-expr is one of:
; - Atom
; - SL

; An SL is one of:
; - '()
; - (cons S-expr SL)

; An Atom is one of:
; - Number
; - String
; - Symbol


;; =================
;; Functions:

; S-expr -> BSL-expr
; computes its value
(check-expect (parse 1) 1)
(check-error  (parse "1") WRONG)
(check-expect (parse '1) 1)
(check-error  (parse 'a) WRONG)
(check-error  (parse '(1 1)) WRONG)
(check-error  (parse '(1 1 1 1)) WRONG)
(check-expect (parse '(+ 1 1)) (make-add 1 1))
(check-expect (parse '(* 2 3)) (make-mul 2 3))
(check-expect (parse '(/ 1 1)) (make-div 1 1))

(define (parse s)
  (cond [(atom? s) (parse-atom s)]
        [else
         (parse-sl s)]))

; Atom -> Boolean
; predicates if it's an atom
(check-expect (atom? #true) #false)
(check-expect (atom? 1)     #true)
(check-expect (atom? "abc") #true)
(check-expect (atom? 'sb)   #true)

(define (atom? a)
  (or (number? a)
      (string? a)
      (symbol? a)))

; SL -> BSL-expr
(define (parse-sl s)
  (local ((define L (length s)))
    (cond [(< L 3) (error WRONG)]
          [(and (= L 3)
                (symbol? (first s)))
           (cond [(symbol=? (first s) '+)
                  (make-add (parse (second s))
                            (parse (third  s)))]
                 [(symbol=? (first s) '*)
                  (make-mul (parse (second s))
                            (parse (third  s)))]
                 [(symbol=? (first s) '/)
                  (make-div (parse (second s))
                            (parse (third  s)))]
                 [else
                  (error WRONG)])]
          [else
           (error WRONG)])))

; Atom -> BSL-expr
(define (parse-atom s)
  (cond [(number? s) s]
        [(string? s) (error WRONG)]
        [(symbol? s) (error WRONG)]))

;; Exercise 350

; What is unusual about the definition of this program with respect to the design recipe?
;
; Note One unusual aspect is that parse uses length on the list argument.
; Real parsers avoid length because it slows the functions down.

;; Exercise 351


;; =================
;; Functions:

; S-expr -> Number
; produces its value; otherwise, it signals an error
(check-error  (interpreter-expr "1") WRONG)
(check-expect (interpreter-expr 1) 1)
(check-expect (interpreter-expr '(+ 1 1)) 2)
(check-expect (interpreter-expr '(* 2 3)) 6)
(check-expect (interpreter-expr '(/ 8 2)) 4)

(define (interpreter-expr s)
  ((compose eval-expression parse) s))



;; 21.2 - Interpreting Variables


;; =================
;; Data definitions:

; A BSL-var-expr is one of:
; - Number
; - Symbol
; - (make-add BSL-var-expr BSL-var-expr)
; - (make-mul BSL-var-expr BSL-var-expr)
; interpretation class of values and variables to which a representation of a BSL expression can evaluate

;; Exercise 352


;; =================
;; Functions:

; BSL-var-expr Symbol Number -> BSL-var-expr
; produces a BSL-var-expr like ex with all occurrences of x replaced by v
(check-expect (subst 1  'a 2) 1)
(check-expect (subst 'a 'a 2) 2)
(check-expect (subst 'b 'a 2) 'b)
(check-expect (subst (make-add 'a 'b) 'a 2) (make-add 2 'b))
(check-expect (subst (make-add 'a 'a) 'a 2) (make-add 2 2))
(check-expect (subst (make-mul 'a 'b) 'a 3) (make-mul 3 'b))
(check-expect (subst (make-mul 'a 'a) 'a 3) (make-mul 3 3))
(check-expect (subst (make-mul 'a (make-add 'b 'a)) 'a 3) (make-mul 3 (make-add 'b 3)))

(define (subst ex x v)
  (cond [(number? ex) ex]
        [(symbol? ex)
         (if (symbol=? ex x) v ex)]
        [(add? ex)
         (make-add (subst (add-left  ex) x v)
                   (subst (add-right ex) x v))]
        [(mul? ex)
         (make-mul (subst (mul-left  ex) x v)
                   (subst (mul-right ex) x v))]))

;; Exercise 353


;; =================
;; Functions:

; BSL-var-expr -> Boolean
; determines whether a BSL-var-expr is also a BSL-expr
(check-expect (numeric? 1)  #true)
(check-expect (numeric? 'a) #false)
(check-expect (numeric? EXPR1) #true)
(check-expect (numeric? EXPR2) #true)
(check-expect (numeric? EXPR3) #true)
(check-expect (numeric? EXPR4) #false)

(define (numeric? ex)
  (cond [(number? ex) #true]
        [(symbol? ex) #false]
        [(add? ex)
         (and (numeric? (add-left  ex))
              (numeric? (add-right ex)))]
        [(mul? ex)
         (and (numeric? (mul-left  ex))
              (numeric? (mul-right ex)))]
        [else
         #false]))

;; Exercise 354


;; =================
;; Data definitions:

; An AL (short for association list) is [List-of Association]
; An Association is a list of two items:
;   (cons Symbol (cons Number '()))
(define AL1 '((a 2)))
(define AL2 '((c 1) (b 3) (a 2)))


;; =================
;; Functions:

; BSL-var-expr AL -> Number
; determines its value; otherwise it signals an error
(check-expect (eval-variable* 1 AL1) 1)
(check-expect (eval-variable* (make-add 'a 1) AL1) 3)
(check-expect (eval-variable* (make-add (make-mul 'c 'b) 'a) AL2) 5)
(check-error  (eval-variable* (make-add 'b 1) AL1) WRONG)
(check-error  (eval-variable* (make-add (make-mul 'c 'b) (make-mul 'a 'd)) AL2) WRONG)

(define (eval-variable* ex da)
  (eval-variable
   (foldl (lambda (a b) (subst b (first a) (second a))) ex da)))

; BSL-var-expr -> Number or Error
; determines its value if numeric? yields true for the input; otherwise it signals an error
(check-expect (eval-variable 1) 1)
(check-expect (eval-variable (make-add 1 2)) 3)
(check-expect (eval-variable (make-add (make-mul 1 2) 3)) 5)
(check-error  (eval-variable 'a) WRONG)
(check-error  (eval-variable (make-add 1 'a)) WRONG)
(check-error  (eval-variable (make-add (make-mul 1 2) 'a)) WRONG)

(define (eval-variable ex)
  (if (numeric? ex)
      (calculator ex)
      (error WRONG)))

;; Exercise 355


;; =================
;; Functions:

; BSL-var-expr AL -> Number
; determines its value; otherwise it signals an error
(check-expect (eval-var-lookup 1 AL1) 1)
(check-expect (eval-var-lookup (make-add 'a 1) AL1) 3)
(check-expect (eval-var-lookup (make-add (make-mul 'c 'b) 'a) AL2) 5)
(check-error  (eval-var-lookup (make-add 'b 1) AL1) WRONG)
(check-error  (eval-var-lookup (make-add (make-mul 'c 'b) (make-mul 'a 'd)) AL2) WRONG)

(define (eval-var-lookup e da)
  (local ((define (eval-var-lookup e)
            (cond [(number? e) e]
                  [(symbol? e)
                   (local ((define result (assq e da)))
                     (if (false? result)
                         (error WRONG)
                         (second result)))]
                  [(add? e)
                   (+ (eval-var-lookup (add-left e))
                      (eval-var-lookup (add-right e)))]
                  [(mul? e)
                   (* (eval-var-lookup (mul-left e))
                      (eval-var-lookup (mul-right e)))])))

    (eval-var-lookup e)))



;; 21.3 - Interpreting Functions

;; Exercise 356


;; =================
;; Data definitions:

(define-struct fun-app [name arg])
; A Fun-App is a structure:
;   (make-fun-app Symbol BSL-fun-expr)
; interpretation (make-fun-app n a) specifies an function application
;  n: is the name of the function; and
;  a: is the an argument of the function

; A BSL-fun-expr is one of:
;  - Number
;  - Symbol
;  - (make-add BSL-fun-expr BSL-fun-expr)
;  - (make-mul BSL-fun-expr BSL-fun-expr)
;  - (make-fun-app Symbol BSL-fun-expr)
; interpretation class of values, variables and function application to which a representation of a BSL expression can evaluate
(define k (make-fun-app 'k (make-add 1 1)))
(define EXPR5 (make-mul 5 k))
(define i (make-fun-app 'i 5))
(define EXPR6 (make-mul i k))

;; Exercise 357


;; =================
;; Functions:

; BSL-fun-expr Symbol Symbol BSL-fun-expr -> Number
; determines the value of ex
(check-error  (eval-definition1 (make-add (make-fun-app 'f 'x) 'a) 'f 'x (make-add 1 1)) WRONG)
(check-error  (eval-definition1 (make-add (make-fun-app 'f 'x) 'a) 'g 'x (make-add 1 1)) WRONG)
(check-expect (eval-definition1 (make-add (make-fun-app 'f 1) 1) 'f 'x (make-add 'x 1)) 3)
(check-expect (eval-definition1 (make-add (make-fun-app 'f 2) 1) 'f 'x (make-mul 'x 3)) 7)

(define (eval-definition1 ex f x b)
  (cond [(number? ex) ex]
        [(symbol? ex) (error WRONG)]
        [(add? ex)
         (+ (eval-definition1 (add-left  ex) f x b)
            (eval-definition1 (add-right ex) f x b))]
        [(mul? ex)
         (* (eval-definition1 (mul-left  ex) f x b)
            (eval-definition1 (mul-right ex) f x b))]
        [(fun-app? ex)
         (if (symbol=? (fun-app-name ex) f)
             (local ((define value (eval-definition1 (fun-app-arg ex) f x b))
                     (define plugd (subst b x value)))
               (eval-definition1 plugd f x b))
             (error WRONG))]))

;; Exercise 358


;; =================
;; Data definitions:

(define-struct fun-def [name parameter body])
; A Fun-Def (BSL-fun-def) is a structure:
;   (make-fun-def Symbol Symbol BSL-fun-expr)
; interpretation (make-fun-def n p b) specifies an function definition
;  n: is the name of the function
;  p: is the parameter of the function; and
;  b: is the body expression of the function
(define f (make-fun-def 'f 'x (make-add 3 'x)))
(define g (make-fun-def 'g 'y (make-fun-app 'f (make-mul 2 'y))))
(define h (make-fun-def 'h 'v (make-add (make-fun-app 'f 'v) (make-fun-app 'g 'v))))

; A BSL-fun-def* is a [List-of BSL-fun-def]:
; interpretation represent a definitions area that consists of a number of one-argument function definitions
(define da-fgh (list f g h))


;; =================
;; Functions:

; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(check-expect (lookup-def da-fgh 'g) g)
(check-error  (lookup-def da-fgh 'a) WRONG)

(define (lookup-def da f)
  (cond [(empty? da) (error WRONG)]
        [else
         (if (symbol=? (fun-def-name (first da)) f)
             (first da)
             (lookup-def (rest da) f))]))

;; Exercise 359


;; =================
;; Functions:

; BSL-fun-expr BSL-fun-def* -> Number
; computes its value
(check-expect (eval-function* (make-fun-app 'f 2) da-fgh) 5)
(check-expect (eval-function* (make-fun-app 'g 6) da-fgh) 15)
(check-expect (eval-function* (make-fun-app 'h (make-add 1 1)) da-fgh) 12)
(check-error  (eval-function* (make-fun-app 'h (make-fun-app 'square 2)) da-fgh) WRONG)

(define (eval-function* ex da)
  (local (; BSL-fun-expr Symbol Number -> BSL-fun-expr
          (define (subst ex x v)
            (cond [(number? ex) ex]
                  [(symbol? ex)
                   (if (symbol=? ex x) v ex)]
                  [(add? ex)
                   (make-add (subst (add-left  ex) x v)
                             (subst (add-right ex) x v))]
                  [(mul? ex)
                   (make-mul (subst (mul-left  ex) x v)
                             (subst (mul-right ex) x v))]
                  [(fun-app? ex)
                   (make-fun-app (fun-app-name ex)
                                 (subst (fun-app-arg ex) x v))])))

    (cond [(number? ex) ex]
          [(symbol? ex) (error WRONG)]
          [(add? ex)
           (+ (eval-function* (add-left  ex) da)
              (eval-function* (add-right ex) da))]
          [(mul? ex)
           (* (eval-function* (mul-left  ex) da)
              (eval-function* (mul-right ex) da))]
          [(fun-app? ex)
           (local ((define f (lookup-def da (fun-app-name ex)))
                   (define b (fun-def-body f))
                   (define x (fun-def-parameter f))
                   (define v (eval-function* (fun-app-arg ex) da)))
             (eval-function* (subst b x v) da))])))



;; 21.4 - Interpreting Everything

;; Exercise 360


;; =================
;; Data definitions:

; A BSL-da is one of:
; - Association
; - Fun-Def
; interpretation data definition for the representation of DrRacket’s definition area

; A BSL-da-all is a [List-of BSL-da]:
; interpretation a sequence that freely mixes constant definitions and one-argument function definitions

(define close-to-pi '(close-to-pi 3.14))
(define area-of-circle (make-fun-def 'area-of-circle 'r (make-mul 'close-to-pi (make-mul 'r 'r))))
(define volume-of-10-cylinder (make-fun-def 'volume-of-10-cylinder 'r (make-mul 10 (make-fun-app 'area-of-circle 'r))))

(define da-list (list close-to-pi area-of-circle volume-of-10-cylinder))


;; =================
;; Functions:

; BSL-da-all Symbol -> Number
; produces the representation of a constant definition whose name is x
(check-expect (lookup-con-def da-list 'close-to-pi)  3.14)
(check-error  (lookup-con-def da-list 'closed-to-pi) WRONG)

(define (lookup-con-def da x)
  (cond [(empty? da) (error WRONG)]
        [else
         (if (and (list? (first da))
                  (symbol=? (first (first da)) x))
             (second (first da))
             (lookup-con-def (rest da) x))]))

; BSL-da-all Symbol -> Fun-Def
; produces the representation of a function definition whose name is f
(check-error  (lookup-fun-def da-list 'close-to-pi) WRONG)
(check-expect (lookup-fun-def da-list 'area-of-circle) area-of-circle)
(check-expect (lookup-fun-def da-list 'volume-of-10-cylinder) volume-of-10-cylinder)

(define (lookup-fun-def da f)
  (cond [(empty? da) (error WRONG)]
        [else
         (if (and (fun-def? (first da))
                  (symbol=? (fun-def-name (first da)) f))
             (first da)
             (lookup-fun-def (rest da) f))]))

;; Exercise 361


;; =================
;; Functions:

; BSL-fun-expr BSL-da-all -> Number
; produces the same value that DrRacket
(check-expect (eval-all 'close-to-pi da-list)  3.14)
(check-error  (eval-all 'close-to-pi2 da-list) WRONG)
(check-expect (eval-all (make-fun-app 'area-of-circle 5) da-list) 78.5)
(check-expect (eval-all (make-fun-app 'volume-of-10-cylinder 5) da-list) 785)
(check-error  (eval-all (make-fun-app 'area-of-circle2 5) da-list) WRONG)

(define (eval-all ex da)
  (local (; BSL-fun-expr Symbol Number -> BSL-fun-expr
          (define (subst ex x v)
            (cond [(number? ex) ex]
                  [(symbol? ex)
                   (if (symbol=? ex x) v ex)]
                  [(add? ex)
                   (make-add (subst (add-left  ex) x v)
                             (subst (add-right ex) x v))]
                  [(mul? ex)
                   (make-mul (subst (mul-left  ex) x v)
                             (subst (mul-right ex) x v))]
                  [(fun-app? ex)
                   (make-fun-app (fun-app-name ex)
                                 (subst (fun-app-arg ex) x v))])))

    (cond [(number? ex) ex]
          [(symbol? ex) (lookup-con-def da ex)]
          [(add? ex)
           (+ (eval-all (add-left  ex) da)
              (eval-all (add-right ex) da))]
          [(mul? ex)
           (* (eval-all (mul-left  ex) da)
              (eval-all (mul-right ex) da))]
          [(fun-app? ex)
           (local ((define def (lookup-fun-def da (fun-app-name ex)))
                   (define b (fun-def-body def))
                   (define x (fun-def-parameter def))
                   (define v (eval-all (fun-app-arg ex) da)))
             (eval-all (subst b x v) da))])))

;; Exercise 362


;; =================
;; Functions:

; S-expr SL -> Number
; produces the result of expression
(define sl '((close-to-pi 3.14)
             (def area-of-circle r (* close-to-pi (* r r)))
             (def volume-of-10-cylinder r (* 10 (app area-of-circle r)))))

(check-expect (interpreter 'close-to-pi sl) 3.14)
(check-expect (interpreter '(app area-of-circle 5) sl) 78.5)
(check-expect (interpreter '(app volume-of-10-cylinder 5) sl) 785)

(define (interpreter s-expr sl)
  (local ((define (parse-atom s)
            (cond [(or (number? s)
                       (symbol? s)) s]
                  [(string? s) (error WRONG)]))

          (define (parse-sl s)
            (cond [(and (= (length s) 2)
                        (symbol? (first s)))
                   (list (first s) (parse (second s)))]
                  [(and (= (length s) 3)
                        (symbol? (first s)))
                   (cond [(symbol=? (first s) '+)
                          (make-add (parse (second s)) (parse (third s)))]
                         [(symbol=? (first s) '*)
                          (make-mul (parse (second s)) (parse (third s)))]
                         [(symbol=? (first s) 'app)
                          (make-fun-app (second s) (parse (third s)))]
                         [else
                          (error WRONG)])]
                  [(and (= (length s) 4)
                        (symbol? (first s)))
                   (cond [(symbol=? (first s) 'def)
                          (make-fun-def (second s) (third s) (parse (fourth s)))]
                         [else
                          (error WRONG)])]
                  [else
                   (error WRONG)]))

          (define (parse s)
            (cond [(atom? s) (parse-atom s)]
                  [else
                   (parse-sl s)])))

    (eval-all (parse s-expr) (map parse sl))))
