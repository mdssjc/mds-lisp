;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |15-Designing Abstractions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 15-Designing Abstractions.rkt
;; III - Abstraction
;; 15  - Designing Abstractions

(require 2htdp/image)


;; 15.1 - Abstractions from Examples


;; =================
;; Data definitions:

(define-struct IR [name price])
; An IR is a structure:
;   (make-IR String Number)
; An Inventory is one of:
; - '()
; - (cons IR Inventory)


;; =================
;; Functions:

; List-of-numbers -> List-of-numbers
; converts a list of Celsius
; temperatures to Fahrenheit
(check-expect (cf* '(100 0 -40))
              '(212 32 -40))

#;
(define (cf* l)
  (cond [(empty? l) '()]
        [else
         (cons (C2F (first l))
               (cf* (rest  l)))]))
#;
(define (cf* l g)
  (cond [(empty? l) '()]
        [else
         (cons (g (first l))
               (cf* (rest l) g))]))
(define (cf* l)
  (map1 l C2F))

; Number -> Number
; converts one Celsius
; temperature to Fahrenheit
(define (C2F c)
  (+ (* 9/5 c) 32))

; Inventory -> List-of-strings
; extracts the names of
; toys from an inventory
(check-expect (names (list (make-IR "doll" 21.0) (make-IR "bear" 13.0)))
              '("doll" "bear"))

#;
(define (names i)
  (cond [(empty? i) '()]
        [else
         (cons (IR-name (first i))
               (names   (rest  i)))]))
#;
(define (names i g)
  (cond [(empty? i) '()]
        [else
         (cons (g (first i))
               (names (rest i) g))]))
(define (names i)
  (map1 i IR-name))

; List-of-numbers [Number -> Number] -> List-of-numbers
; Inventory [IR -> String] -> List-of-strings
(define (map1 k g)
  (cond [(empty? k) '()]
        [else
         (cons (g (first k))
               (map1 (rest k) g))]))

;; Exercise 250

; Number -> [List-of Number]
; tabulates sin between n
; and 0 (incl.) in a list
(define (tab-sin n)
  (cond [(= n 0) (list (sin 0))]
        [else
         (cons (sin n)
               (tab-sin (sub1 n)))]))

; Number -> [List-of Number]
; tabulates sqrt between n
; and 0 (incl.) in a list
(define (tab-sqrt n)
  (cond [(= n 0) (list (sqrt 0))]
        [else
         (cons (sqrt n)
               (tab-sqrt (sub1 n)))]))

; [Number -> Number] Number -> [List-of Number]
; creates a list by applying each number in the range (n..0) for the function f
(check-expect (tabulate sqr 0) (list (sqr 0)))
(check-expect (tabulate sqr 2) (list (sqr 2) (sqr 1) (sqr 0)))
(check-expect (tabulate tan 0) (list (tan 0)))
(check-within (tabulate tan 2) (list (tan 2) (tan 1) (tan 0)) 0.1)

(define (tabulate f n)
  (cond [(= n 0) (list (f 0))]
        [else
         (cons (f n)
               (tabulate f (sub1 n)))]))

;; Exercise 251


;; =================
;; Functions:

; [List-of Number] -> Number
; computes the sum of
; the numbers on l
(define (sum l)
  (cond [(empty? l) 0]
        [else
         (+ (first l)
            (sum (rest l)))]))

; [List-of Number] -> Number
; computes the product of
; the numbers on l
(define (product l)
  (cond [(empty? l) 1]
        [else
         (* (first l)
            (product (rest l)))]))

; [Number Number -> Number] [List-of Number] Number -> Number
; summarizes the list l with the function f and identity i
(check-expect (fold1 + '() 0) 0)
(check-expect (fold1 + '(1 2 3) 0) 6)
(check-expect (fold1 * '() 1) 1)
(check-expect (fold1 * '(1 2 3) 1) 6)

(define (fold1 f l i)
  (cond [(empty? l) i]
        [else
         (f (first l)
            (fold1 f (rest l) i))]))

;; Exercise 252


;; =================
;; Constants:

; graphical constants:
(define emt (empty-scene 100 100))
(define dot (circle 3 "solid" "red"))


;; =================
;; Functions:

; [List-of Posn] -> Image
(define (image* l)
  (cond [(empty? l) emt]
        [else
         (place-dot (first l)
                    (image* (rest l)))]))

; Posn Image -> Image
(define (place-dot p img)
  (place-image dot (posn-x p) (posn-y p) img))

; [A B] [A B -> B] [List-of A] B -> B
; summarizes the list l with the function f and identity i
(check-expect (fold2 * '() 1) 1)
(check-expect (fold2 * '(1 2 3) 1) 6)
(check-expect (fold2 * '(1 2 3) 1) 6)
(check-expect (fold2 place-dot (list (make-posn 3 5) (make-posn 7 2)) emt)
              (place-dot (make-posn 3 5) (place-dot (make-posn 7 2) emt)))

(define (fold2 f l i)
  (cond [(empty? l) i]
        [else
         (f (first l)
            (fold2 f (rest l) i))]))



;; 15.2 - Similarities in Signatures

; Number Boolean -> String
(define (f n b) "hello world")

; [X Y] [List-of X] -> [List-of Y]

; [List-of Number] -> [List-of Number]
; [List-of IR] -> [List-of String]

; [X Y] [List-of X] [X -> Y] -> [List-of Y]

; [List-of Number] -> Number
; [List-of Posn]   -> Image


; [List-of Number] Number [Number Number -> Number]
; -> Number
(define (pr* l bs jn)
  (cond [(empty? l) bs]
        [else
         (jn (first l)
             (pr* (rest l) bs jn))]))

; [List-of Posn] Image [Posn Image -> Image]
; -> Image
(define (im* l bs jn)
  (cond [(empty? l) bs]
        [else
         (jn (first l)
             (im* (rest l) bs jn))]))

; [X Y] [List-of X] Y [X Y -> Y] -> Y

;; Exercise 253

; [Number -> Boolean]
(zero? 0)

; [Boolean String -> Boolean]
(eq? #true "world")

; [Number Number Number -> Number]
(+ 1 2 3)

; [Number -> [List-of Number]]
(list 1)

; [[List-of Number] -> Boolean]
(number? (list 1 2 3))

;; Exercise 254

; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; !!!
(define (sort-n lon f) empty)

; [List-of String] [String String -> Boolean] -> [List-of String]
; !!!
(define (sort-s los f) empty)


; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; !!!
(define (sort-x lst f) empty)

;; Exercise 255

; [List-of Number] [Number -> Number] -> [List-of Number]
; !!!
(define (map-n lon f) empty)

; [List-of String] [String -> String] -> [List-of String]
; !!!
(define (map-s lon f) empty)


; [X] [List-of X] [X -> X] -> [List-of X]
; !!!
(define (map-x lst f) empty)



;; 15.3 - Single Point of Control



;; 15.4 - Abstractions from Templates

; [X Y] [List-of X] Y [X Y -> Y] -> Y
(define (reduce l base combine)
  (cond [(empty? l) base]
        [else
         (combine (first l)
                  (reduce (rest l) base combine))]))

; [List-of Number] -> Number
(define (sum.v2 lon)
  (reduce lon 0 +))

; [List-of Number] -> Number
(define (product.v2 lon)
  (reduce lon 1 *))
