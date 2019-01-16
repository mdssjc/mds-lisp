;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname more-foo-evaluation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; more-foo-evaluation.rkt
;; BSL P6 - More Foo Evaluation
;; Step by step evaluation of a call to a function that calls a number primitive in its body.


;;
;; PROBLEM:
;;
;; Given the following function definition:
;;
;; (define (foo n)
;;   (* n n))
;;
;; Write out the step-by-step evaluation of the expression: 
;;
;; (foo (+ 3 4))
;;
;; Be sure to show every intermediate evaluation step.

(define (foo n)
  (* n n))

(foo (+ 3 4))

(foo 7)

(* 7 7)

49
