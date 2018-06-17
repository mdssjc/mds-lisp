;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 1-Arithmetic) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 1-Arithmetic.rkt
;; I - Fixed-Size Data
;; 1 - Arithmetic

(require 2htdp/image)


;; 1.1 - The Arithmetic of Numbers

;; Exercise 1

(define x 3)
(define y 4)

(define (distance x y)
  (sqrt (+ (sqr x) (sqr y))))

(check-expect (distance x y)  5)
(check-expect (distance 12 5) 13)



;; 1.2 - The Arithmetic of Strings

;; Exercise 2

(define prefix "hello")
(define suffix "world")

(define (glue prefix suffix)
  (string-append prefix "_" suffix))

(check-expect (glue prefix suffix) "hello_world")



;; 1.3 - Mixing It Up

;; Exercise 3

(define str "helloworld")
(define ind "0123456789")
(define i 5)

(define (insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))

(check-expect (insert str i) "hello_world")

;; Exercise 4

(define (delete str i)
  (string-append (substring str 0 i)
                 (substring str (add1 i))))

(check-expect (delete str i) "helloorld")
(check-expect (delete str 0) "elloworld")
;;(check-expect (delete str 10) "helloorld")



;; 1.4 - The Arithmetic of Images

;; Exercise 5

;; Tree
(define (my-scale value) (* value 1))
(define leaf   (circle (my-scale 10) "solid" "green"))
(define trunk  (rectangle (my-scale 10) (my-scale 20) "solid" "brown"))
(define sheets (overlay/offset leaf 0 (my-scale 5) (overlay/offset leaf (my-scale 10) 0 leaf)))

(define tree (overlay/offset sheets 0 (my-scale 15) trunk))

;; Print
tree

;; Exercise 6

(define cat (circle 11 "solid" "brown"))
(define pixels (* (image-width cat) (image-height cat)))

(check-expect pixels (* (* 11 2) (* 11 2)))



;; 1.5 - The Arithmetic of Booleans

;; Exercise 7

(define sunny  #true)
(define friday #false)

(check-expect (or (not sunny) friday) #false)



;; 1.6 - Mixing It Up with Booleans

;; Exercise 8

(define rh (rectangle 10 20 "solid" "black"))
(define rw (rectangle 20 10 "solid" "black"))
(define rs (rectangle 20 20 "solid" "black"))

(define (tall-or-wide? img)
  (cond [(> (image-height img) (image-width  img)) "tall"]
        [(> (image-width  img) (image-height img)) "wide"]
        [else "square"]))

(check-expect (tall-or-wide? cat) "square")
(check-expect (tall-or-wide? rh)  "tall")
(check-expect (tall-or-wide? rw)  "wide")
(check-expect (tall-or-wide? rs)  "square")



;; 1.7 - Predicates: Know Thy Data

;; Exercise 9

(define in "hello")

(define (convert x)
  (cond [(string? x)  (string-length x)]
        [(image? x)   (* (image-width x) (image-height x))]
        [(number? x)  (if (> x 0) (- x 1) x)]
        [(boolean? x) (if x 10 20)]))

(check-expect (convert in) 5)
(check-expect (convert (rectangle 10 10 "solid" "black")) 100)
(check-expect (convert 10)  9)
(check-expect (convert 0)   0)
(check-expect (convert -1) -1)
(check-expect (convert #true)  10)
(check-expect (convert #false) 20)

;; Exercise 10

"Now relax, eat, sleep, and then tackle the next chapter."
