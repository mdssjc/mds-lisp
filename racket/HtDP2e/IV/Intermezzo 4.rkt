;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Intermezzo 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Intermezzo 4.rkt
;; Intermezzo 4: The Nature of Numbers



;; Fixed-Size Number Arithmetic


;; =================
;; Data definitions:

(define-struct inex [mantissa sign exponent])
; An Inex is a structure:
;   (make-inex N99 S N99)
; An S is one of:
; - 1
; - -1
; An N99 is an N between 0 and 99 (inclusive).


;; =================
;; Functions:

; create-inex: N Number N -> Inex
; makes an instance of Inex after checking the arguments
(define (create-inex m s e)
  (cond [(and (<= 0 m 99)
              (<= 0 e 99)
              (or (= s 1)
                  (= s -1)))
         (make-inex m s e)]
        [else
         (error "bad values given")]))

; inex->number: Inex -> Number
; converts an inex into its numeric equivalent
(check-expect (inex->number (create-inex 12 1 2)) 1200)
(check-error  (inex->number (create-inex 120 1 1)) "bad values given")
(check-expect (inex->number (create-inex 50 -1 20)) 5e-19)
(check-expect (inex->number (create-inex  5 -1 19)) 5e-19)

(define (inex->number an-inex)
  (* (inex-mantissa an-inex)
     (expt 10 (* (inex-sign     an-inex)
                 (inex-exponent an-inex)))))


;; =================
;; Constants:

(define MAX-POSITIVE (create-inex 99 1 99))
(define MIN-POSITIVE (create-inex 1 -1 99))


;; (check-expect (inex+ (create-inex 1 1 0) (create-inex 2 1 0)) (create-inex 3 1 0))
;; (check-expect (inex+ (create-inex 55 1 0) (create-inex 55 1 0)) (create-inex 11 1 1))
;; (check-expect (inex+ (create-inex 56 1 0) (create-inex 56 1 0)) (create-inex 11 1 1))
;; (check-expect (inex* (create-inex 2 1 4) (create-inex 8 1 10)) (create-inex 16 1 14))
;; (check-expect (inex* (create-inex 20 1 1) (create-inex  5 1 4)) (create-inex 10 1 6))
;; (check-expect (inex* (create-inex 27 -1 1) (create-inex  7 1 4)) (create-inex 19 1 4))
