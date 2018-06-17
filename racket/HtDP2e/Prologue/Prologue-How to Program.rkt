;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Prologue-How to Program|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Prologue-How to Program.rkt
;; Prologue - How to Program

(require 2htdp/image)
(require 2htdp/universe)


(+ 1 1)

(+ 2 2)
(* 3 3)
(- 4 2)
(/ 6 2)

(sqr 3)
(expt 2 3)
(sin 0)
(cos pi)

(+ 2 (+ 3 4))
(+ 2 3 4)

(+ 2 (+ (* 3 3) 4))
(+ 2 (+ (* 3 (/ 12 4)) 4))
(+ (* 5 5) (+ (* 3 (/ 12 4)) 4))

;; (+ (1) (2))

(+ 1 2 3 4 5 6 7 8 9 0)
(* 1 2 3 4 5 6 7 8 9 0)



;; Arithmetic and Arithmetic

"hello world"

(string-append "hello" "world")
(string-append "hello " "world")

(string-append "hello" " " "world")

(+ (string-length "hello world") 20)
(number->string 42)

(string->number "42")

(string->number "hello world")

(and #true  #true)
(and #true  #false)
(or  #true  #false)
(or  #false #false)
(not #false)

(> 10 9)
(< -1 0)
(= 42 9)

(and (or (= (string-length "hello world")
            (string->number "11"))
         (string=? "hello world" "good morning"))
     (>= (+ (string-length "hello world") 60) 80))

(define ROCKET (rectangle 10 20 "solid" "blue"))
(* (image-width ROCKET) (image-height ROCKET))

(circle 10 "solid" "red")
(rectangle 30 20 "outline" "blue")

(overlay (circle 5 "solid" "red")
         (rectangle 20 20 "solid" "blue"))

(overlay (rectangle 20 20 "solid" "blue")
         (circle 5 "solid" "red"))

(image-width (square 10 "solid" "red"))
(image-width (overlay (rectangle 20 20 "solid" "blue")
                      (circle 5 "solid" "red")))

(place-image (circle 5 "solid" "green")
             50 80
             (empty-scene 100 100))



;; Inputs and Output

(define (y x) (* x x))

(y 1)
(y 2)
(y 3)
(y 4)
(y 5)

(empty-scene 100 60)

(place-image ROCKET 50 23 (empty-scene 100 60))
(place-image ROCKET 50 20 (empty-scene 100 60))
(place-image ROCKET 50 30 (empty-scene 100 60))
(place-image ROCKET 50 40 (empty-scene 100 60))

(define (picture-of-rocket height)
  (place-image ROCKET 50 height (empty-scene 100 60)))

(picture-of-rocket 0)
(picture-of-rocket 10)
(picture-of-rocket 20)
(picture-of-rocket 30)

(animate picture-of-rocket)



;; Many Ways to Compute

(define (sign x)
  (cond [(> x 0) 1]
        [(= x 0) 0]
        [(< x 0) -1]))

(sign 10)
(sign -5)
(sign 0)

(define (picture-of-rocket.v2 height)
  (cond [(<= height 60)
         (place-image ROCKET 50 height (empty-scene 100 60))]
        [(> height 60)
         (place-image ROCKET 50 60 (empty-scene 100 60))]))

(picture-of-rocket 5555)
(picture-of-rocket.v2 5555)

(animate picture-of-rocket.v2)

(- 60 (/ (image-height ROCKET) 2))
(place-image ROCKET 50 (- 60 (image-height ROCKET)) (empty-scene 100 60))

(define (picture-of-rocket.v3 height)
  (cond [(<= height (- 60 (/ (image-height ROCKET) 2)))
         (place-image ROCKET 50 height (empty-scene 100 60))]
        [(> height (- 60 (/ (image-height ROCKET) 2)))
         (place-image ROCKET 50 (- 60 (/ (image-height ROCKET) 2)) (empty-scene 100 60))]))



;; One Program, Many Definitions

(define WIDTH  100)
(define HEIGHT  60)

(define (picture-of-rocket.v4 h)
  (cond [(<= h (- HEIGHT (/ (image-height ROCKET) 2)))
         (place-image ROCKET 50 h (empty-scene WIDTH HEIGHT))]
        [(> h (- HEIGHT (/ (image-height ROCKET) 2)))
         (place-image ROCKET 50 (- HEIGHT (/ (image-height ROCKET) 2)) (empty-scene WIDTH HEIGHT))]))

(animate picture-of-rocket.v4)

(- HEIGHT (/ (image-height ROCKET) 2))

(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))

(define CENTER 100)
(define HEIGHT.V1 (* 2 CENTER))

; constants
;; (define WIDTH  100)
;; (define HEIGHT  60)
(define MTSCN (empty-scene WIDTH HEIGHT))
;; (define ROCKET ...)
;; (define ROCKET-CENTER-TO-TOP
;;   (- HEIGHT (/ (image-height ROCKET) 2)))

; functions
(define (picture-of-rocket.v5 h)
  (cond [(<= h ROCKET-CENTER-TO-TOP)
         (place-image ROCKET 50 h MTSCN)]
        [(> h ROCKET-CENTER-TO-TOP)
         (place-image ROCKET 50 ROCKET-CENTER-TO-TOP MTSCN)]))

(overlay (circle 10 "solid" "green")
         (rectangle 40 4 "solid" "green"))

(animate picture-of-rocket.v5)



;; One More Definition

; properties of the "world" and the descending rocket
;; (define WIDTH  100)
;; (define HEIGHT  60)
(define V 3)
(define X 50)

; graphical constants
;; (define MTSCN (empty-scene WIDTH HEIGHT))
;; (define ROCKET ...)
;; (define ROCKET-CENTER-TO-TOP
;;   (- HEIGHT (/ (image-height ROCKET) 2)))

; functions
(define (picture-of-rocket.v6 t)
  (cond [(<= (distance t) ROCKET-CENTER-TO-TOP)
         (place-image ROCKET X (distance t) MTSCN)]
        [(> (distance t) ROCKET-CENTER-TO-TOP)
         (place-image ROCKET X ROCKET-CENTER-TO-TOP MTSCN)]))

(define (distance t)
  (* V t))

(animate picture-of-rocket.v6)



;; You Are a Programmer Now



;; Not!
