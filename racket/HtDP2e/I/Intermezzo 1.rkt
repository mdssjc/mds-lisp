;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Intermezzo 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Intermezzo 1.rkt
;; Intermezzo 1: Beginning Student Language



;; BSL Vocabulary



;; BSL Grammar

;; Exercise 116

; x
; it's a variable

; (= y z)
; it's a primitive application

; (= (= y z) 0)
; it's a primitive application

;; Exercise 117

; (3 + 4)
; syntax error, the correct is (+ 3 4)

; number?
; syntax error, the correct is (number? x)

; (x)
; syntax error, the correct is x

;; Exercise 118

; (define (f x) x)
; def -> variable variable
;   variable (inner)

; (define (f x) y)
; def -> variable variable
;   variable (outer)

; (define (f x y) 3)
; def -> variable variable variable
;   value

;; Exercise 119

; (define (f "x") x)
; a value in place of the variable

; (define (f x y z) (x))
; a variable called as function

;; Exercise 120

; (x)
; illegal - variable

; (+ 1 (not x))
; illegal - sum of integer with boolean

; (+ 1 2 3)
; legal - expr -> (primitive value value value)



;; BSL Meaning

;; Exercise 121

; 1.
(+ (* (/ 12 8) 2/3)
   (- 20 (sqrt 4)))
(+ (* 3/2 2/3)
   (- 20 (sqrt 4)))
(+ 1 (- 20 (sqrt 4)))
(+ 1 (- 20 2))
(+ 1 18)
19

; 2.
(cond
  [(= 0 0) #false]
  [(> 0 1) (string=? "a" "a")]
  [else (= (/  1 0) 9)])
(cond
  [#true #false]
  [(> 0 1) (string=? "a" "a")]
  [else (= (/  1 0) 9)])
#false

; 3.
(cond
  [(= 2 0) #false]
  [(> 2 1) (string=? "a" "a")]
  [else (= (/  1 2) 9)])
(cond
  [#false #false]
  [(> 2 1) (string=? "a" "a")]
  [else (= (/  1 2) 9)])
(cond
 ((> 2 1)
  (string=? "a" "a"))
 (else (= (/ 1 2) 9)))
(cond
  [#true (string=? "a" "a")]
  [else (= (/  1 2) 9)])
(string=? "a" "a")
#true

;; Exercise 122

(define (f x y)
  (+ (* 3 x) (* y y)))

; 1.
(+ (f 1 2) (f 2 1))
(+ (+ (* 3 1) (* 2 2)) (f 2 1))
(+ (+ 3 (* 2 2)) (f 2 1))
(+ (+ 3 4) (f 2 1))
(+ 7 (f 2 1))
(+ 7 (+ (* 3 2) (* 1 1)))
(+ 7 (+ 6 (* 1 1)))
(+ 7 (+ 6 1))
(+ 7 7)
14

; 2.
(f 1 (* 2 3))
(f 1 6)
(+ (* 3 1) (* 6 6))
(+ 3 (* 6 6))
(+ 3 36)
39

; 3.
(f (f 1 (* 2 3)) 19)
(f (f 1 6) 19)
(f (+ (* 3 1) (* 6 6)) 19)
(f (+ 3 (* 6 6)) 19)
(f (+ 3 36) 19)
(f 39 19)
(+ (* 3 39) (* 19 19))
(+ 117 (* 19 19))
(+ 117 361)
478



;; Meaning and Computing



;; BSL Errors



;; Boolean Expressions

;; Exercise 123

(define x 5)
(define y 5)

(if (= x y) x y)
(cond [(= x y) x]
      [else y])



;; Constant Definitions

;; Exercise 124

(define PRICE 5)
(define SALES-TAX (* 0.08 PRICE))
(* 0.08 5)
0.4
(define TOTAL (+ PRICE SALES-TAX))
(+ 5 0.4)
5.4

; fahrenheit->celsius is used here before its definition
;(define COLD-F 32)
;(define COLD-C (fahrenheit->celsius COLD-F))
;(define (fahrenheit->celsius f2)
;  (* 5/9 (- f2 32)))

(define LEFT -100)
(define RIGHT 100)
(define (f2 x) (+ (* 5 (expt x 2)) 10))

(define f@LEFT (f2 LEFT))
(f2 -100)
(+ (* 5 (expt -100 2)) 10)
(+ (* 5 10000) 10)
(+ 50000 10)
50010

(define f@RIGHT (f2 RIGHT))
(f2 100)
(+ (* 5 (expt 100 2)) 10)
(+ 50000 10)
50010



;; Structure Type Definitions

;; Exercise 125

(define-struct oops [])
; it's legal

(define-struct child [parents dob date])
; it's legal

; (define-struct (child person) [dob date])
; it's illegal, incorrect syntax

;; Exercise 126

(define-struct point [x y z])
(define-struct none  [])

(make-point 1 2 3)

(make-point (make-point 1 2 3) 4 5)

;(make-point (+ 1 2) 3 4)
(make-point 3 3 4)

(make-none)

;(make-point (point-x (make-point 1 2 3)) 4 5)
(make-point 1 4 5)

;; Exercise 127

(define-struct ball [x y speed-x speed-y])

(number? (make-ball 1 2 3 4))
#false

(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3))
3

(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3))
6

; (ball-x (make-posn 1 2))
; error, ball-x expects a ball

; (ball-speed-y 5)
; error, ball-x expects a ball



;; BSL Tests

;; Exercise 128

(check-expect 3 4)

(check-member-of "green" "red" "yellow" "grey")

(check-within (make-posn #i1.0 #i1.1)
              (make-posn #i0.9 #i1.2)
              0.01)

(check-range #i0.9 #i0.6 #i0.8)

(check-error (/ 1 1))

(check-random (make-posn (random 3) (random 9))
              (make-posn (random 9) (random 3)))

; (check-satisfied 4 odd?)
(check-satisfied 4 even?)



;; BSL Error Messages
