;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Intermezzo 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Intermezzo 3.rkt
;; Intermezzo 3: Scope and Abstraction

(require 2htdp/abstraction)


;; Scope

(define (f x) (+ (* x x) 25))
(define (g x) (+ (f (+ x 1)) (f (- x 1))))

;; Exercise 300

(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))

(define (p2 x)
  (+ (* 55 x) (+ x 11)))

(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))

;; Exercise 301

(define (insertion-sort alon)
  (local ((define (sort alon)
            (cond [(empty? alon) '()]
                  [else
                   (add (first alon)
                        (sort (rest alon)))]))
          (define (add an alon)
            (cond [(empty? alon) (list an)]
                  [else
                   (cond [(> an (first alon)) (cons an alon)]
                         [else
                          (cons (first alon)
                                (add an (rest alon)))])])))
    (sort alon)))

(define (sort2 alon)
  (local ((define (sort alon)
            (cond [(empty? alon) '()]
                  [else
                   (add (first alon)
                        (sort (rest alon)))]))
          (define (add an alon)
            (cond [(empty? alon) (list an)]
                  [else
                   (cond [(> an (first alon)) (cons an alon)]
                         [else
                          (cons (first alon)
                                (add an (rest alon)))])])))
    (sort alon)))

;; Exercise 302

; x is used here before its definition
;(define x (cons 1 x))

;; Exercise 303

(lambda (x y)
  (+ x (* x y)))

(lambda (x y)
  (+ x
     (local ((define x (* y y)))
       (+ (* 3 x)
          (/ 1 x)))))

(lambda (x y)
  (+ x
     ((lambda (x)
        (+ (* 3 x)
           (/ 1 x)))
      (* y y))))



;; ISL for Loops

; [List-of X] -> [List-of [List N X]]
; pairs each item in lx with its index
(check-expect (enumerate '(a b c))
              '((1 a) (2 b) (3 c)))

(define (enumerate lx)
  (for/list ([x lx] [ith (length lx)])
    (list (+ ith 1) x)))

;; Exercise 304
(check-expect (for/list ([i 2] [j '(a b)]) (list i j))
              '((0 a) (1 b)))

(check-expect (for*/list ([i 2] [j '(a b)]) (list i j))
              '((0 a) (0 b) (1 a) (1 b)))

; [List-of X] [List-of Y] -> [List-of [List X Y]]
; generates all pairs of items from l1 and l2
(check-satisfied (cross '(a b c) '(1 2))
                 (lambda (c) (= (length c) 6)))

(define (cross l1 l2)
  (for*/list ([x1 l1][x2 l2])
    (list x1 x2)))

; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(check-satisfied (arrangements '("r" "a" "t"))
                 all-words-from-rat?)

(define (arrangements w)
  (cond [(empty? w) '(())]
        [else
         (for*/list ([item w]
                     [arrangement-without-item (arrangements (remove item w))])
           (cons item arrangement-without-item))]))

; [List-of X] -> Boolean
(define (all-words-from-rat? w)
  (and (member? (explode "rat") w)
       (member? (explode "art") w)
       (member? (explode "tar") w)))

; N -> sequence?
; constructs the infinite sequence of natural numbers,
; starting from n
;; (define (in-naturals n) ...)

; N N N -> sequence?
; constructs the following finite sequence of natural numbers:
;   start
;   (+ start step)
;   (+ start step step)
;   ...
;  until the number exceeds end
;; (define (in-range start end step) ...)

(define (enumerate.v2 lx)
  (for/list ([item lx] [ith (in-naturals 1)])
    (list ith item)))

; N -> Number
; adds the even numbers between 0 and n (exclusive)
(check-expect (sum-evens 2) 0)
(check-expect (sum-evens 4) 2)

(define (sum-evens n)
  (for/sum ([i (in-range 0 n 2)]) i))

;; Exercise 305


;; =================
;; Constants:

(define US-DOLLAR-RATE 1.06)


;; =================
;; Functions:

; [List-of Number] -> [List-of Number]
; converts a list of US$ amounts into a list of € amounts
; based on an exchange rate of US$ US-DOLLAR-RATE per €
(check-expect (convert-euro '()) '())
(check-expect (convert-euro '(1.00 5.00 12.34))
              (list (* 1.00  US-DOLLAR-RATE)
                    (* 5.00  US-DOLLAR-RATE)
                    (* 12.34 US-DOLLAR-RATE)))

(define (convert-euro lon)
  (for/list ([n lon])
    (* n US-DOLLAR-RATE)))

;; Exercise 306


;; =================
;; Functions:

; Number -> [List-of Number]
; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (f1 5) '(0 1 2 3 4))

(define (f1 n)
  (for/list ([x n])
    x))

; Number -> [List-of Number]
; creates the list (list 1 ... n) for any natural number n
(check-expect (f2 5) '(1 2 3 4 5))

(define (f2 n)
  (for/list ([x n])
    (add1 x)))

; Number -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (f3 5) '(1/1 1/2 1/3 1/4 1/5))

(define (f3 n)
  (for/list ([x n])
    (/ 1 (add1 x))))

; Number -> [List-of Number]
; creates the list of the first n even numbers
(check-expect (f4 5) '(0 2 4 6 8))

(define (f4 n)
  (for/list ([x n])
    (* x 2)))

; Number -> [List-of [List-of Number]]
; creates a diagonal square of 0s and 1s
(check-expect (f5 3) '((1 0 0)
                       (0 1 0)
                       (0 0 1)))

(define (f5 n)
  (local ((define (numbers n s)
            (cond [(zero? n) '()]
                  [else
                   (cons (if (= n s) 1 0)
                         (numbers (sub1 n) s))])))
    (for/list ([x n])
      (numbers n (- n x)))))

; [Number -> X] Number -> [List-of X]
; tabulates a f function n times
(check-expect (tabulate add1 5)
              '(6 5 4 3 2 1))
(check-expect (tabulate number->string 5)
              '("5" "4" "3" "2" "1" "0"))

(define (tabulate f n)
  (reverse (for/list ([x (add1 n)])
             (f x))))

;; Exercise 307


;; =================
;; Functions:

; String [List-of String] -> String or false
; retrieves the first name on the latter that is equal to, or an extension of, the former
(check-expect (find-name "John" '()) #false)
(check-expect (find-name "John" '("Marie" "Paul")) #false)
(check-expect (find-name "John" '("Marie" "Paul" "John")) "John")
(check-expect (find-name "John" '("Marie" "Paul" "Jones")) #false)
(check-expect (find-name "John" '("Marie" "Paul" "Jones" "Johnson")) "Johnson")
(check-expect (find-name "Jo"   '("Marie" "Paul" "Jones")) "Jones")

(define (find-name n lon)
  (for/or ([name lon])
    (if (string-contains? n name) name #false)))

; Number [List-of String] -> Boolean
; ensures that no name on some list exceeds some given width
(check-expect (exceed-width? 5 '())                        #false)
(check-expect (exceed-width? 5 '("alf"))                   #false)
(check-expect (exceed-width? 5 '("alf" "marie"))           #false)
(check-expect (exceed-width? 5 '("alf" "marie" "johnson")) #true)

(define (exceed-width? n lon)
  (for/or ([name lon])
    (> (string-length name) n)))



;; Pattern Matching


;; =================
;; Data definitions:

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999
; A Four is a Number between 1000 and 9999
(define P1 (make-phone 713 123 4567))
(define P2 (make-phone 281 123 4567))
(define P3 (make-phone 123 123 4567))


(match 4
  ['four  1]
  ["four" 2]
  [#true  3]
  [4      "hello world"])

(match 2
  [3 "one"]
  [x (+ x 3)])

(match (cons 1 '())
  [(cons 1    tail) tail]
  [(cons head tail) head])

(match (cons 2 '())
  [(cons 1    tail) tail]
  [(cons head tail) head])

(define p (make-posn 3 4))
(match p
  [(posn x y) (sqrt (+ (sqr x) (sqr y)))])

(match (make-phone 713 664 9993)
  [(phone x y z) (+ x y z)])

(match (cons (make-phone 713 664 9993) '())
  [(cons (phone area-code 664 9993) tail) area-code])

(match (cons 1 '())
  [(cons (?  symbol?) tail) tail]
  [(cons head tail) head])


;; =================
;; Data definitions:

; A [Non-empty-list X] is one of:
; - (cons X '())
; - (cons X [Non-empty-list X])


;; =================
;; Functions:

; [Non-empty-list X] -> X
; retrieves the last item of ne-l
(check-expect (last-item '(a b c)) 'c)
(check-error  (last-item '()))

(define (last-item ne-l)
  (match ne-l
    [(cons lst '()) lst]
    [(cons fst rst) (last-item rst)]))


;; =================
;; Data definitions:

(define-struct layer [color doll])
; An RD.v2 (short for Russian doll) is one of:
; - "doll"
; - (make-layer String RD.v2)


;; =================
;; Functions:

; RD.v2 -> N
; how many dolls are a part of an-rd
(check-expect (depth (make-layer "red" "doll")) 1)

(define (depth a-doll)
  (match a-doll
    ["doll" 0]
    [(layer c inside) (+ (depth inside) 1)]))

; [List-of Posn] -> [List-of Posn]
; moves each object right by delta-x pixels
(define input  (list (make-posn 1 1) (make-posn 10 14)))
(define expect (list (make-posn 4 1) (make-posn 13 14)))

(check-expect (move-right input 3) expect)

(define (move-right lop delta-x)
  (for/list ((p lop))
    (match p
      [(posn x y) (make-posn (+ x delta-x) y)])))

;; Exercise 308

; [List-of Phone] -> [List-of Phone]
; replaces all occurrence of area code 713 with 281
(check-expect (replace '()) '())
(check-expect (replace (list P1 P3))    (list P2 P3))
(check-expect (replace (list P1 P3 P2)) (list P2 P3 P2))

(define (replace lop)
  (for/list ((p lop))
    (match p
      [(phone 713 s f) (make-phone 281 s f)]
      [_ p])))

;; Exercise 309

; [List-of [List-of String]] -> [List-of Number]
; determines the number of words on each line
(check-expect (words-on-line '()) 0)
(check-expect (words-on-line '(("hello" "hello"))) 2)

(define (words-on-line lls)
  (match lls
    ['() 0]
    [(cons line rest) (+ (length line)
                         (words-on-line rest))]))
