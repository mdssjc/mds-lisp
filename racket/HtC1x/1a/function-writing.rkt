;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname function-writing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; function-writing.rkt
;; BSL P15 - Function Writing
;; Write a function that produces the larger of two given numbers.


;;
;; PROBLEM:
;;
;; Write a function that consumes two numbers and produces the larger of the two.

(define (larger a b)
  (if (> a b) a b))

(larger 10 20)
(larger 20 10)