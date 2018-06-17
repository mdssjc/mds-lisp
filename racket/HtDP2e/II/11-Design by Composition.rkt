;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |11-Design by Composition|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 11-Design by Composition.rkt
;; II - Arbitrarily Large Data
;; 11 - Design by Composition

(require 2htdp/image)
(require 2htdp/universe)


;; 11.1 - The list Function

;; Exercise 181

(check-expect (cons "a" (cons "b" (cons "c" (cons "d" '()))))
              (list "a" "b" "c" "d"))

(check-expect (cons (cons 1 (cons 2 '())) '())
              (list (list 1 2)))

(check-expect (cons "a" (cons (cons 1 '()) (cons #false '())))
              (list "a" (list 1) #false))

(check-expect (cons (cons "a" (cons 2 '())) (cons "hello" '()))
              (list (list "a" 2) "hello"))

(check-expect (cons (cons 1 (cons 2 '()))
                    (cons (cons 2 '()) '()))
              (list (list 1 2) (list 2)))

;; Exercise 182

(check-expect (list 0 1 2 3 4 5)
              (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))

(check-expect (list (list "he" 0) (list "it" 1) (list "lui" 14))
              (cons (cons "he" (cons 0 '()))
                    (cons (cons "it" (cons 1 '()))
                          (cons (cons "lui" (cons 14 '())) '()))))

(check-expect (list 1 (list 1 2) (list 1 2 3))
              (cons 1 (cons (cons 1 (cons 2 '()))
                            (cons (cons 1 (cons 2 (cons 3 '()))) '()))))

;; Exercise 183

(check-expect (cons "a" (list 0 #false))
              (list "a" 0 #false))

(check-expect (list (cons 1 (cons 13 '())))
              (list (list 1 13)))

(check-expect (cons (list 1 (list 13 '())) '())
              (list (list 1 (list 13 '()))))

(check-expect (list '() '() (cons 1 '()))
              (list '() '() (list 1)))

(check-expect (cons "a" (cons (list 1) (list #false '())))
              (list "a" (list 1) #false '()))

;; Exercise 184

(check-expect (list (string=? "a" "b") #false)
              (list #false #false))

(check-expect (list (+ 10 20) (* 10 20) (/ 10 20))
              (list 30 200 0.5))

(check-expect (list "dana" "jane" "mary" "laura")
              (list "dana" "jane" "mary" "laura"))

;; Exercise 185

(check-expect (first (list 1 2 3)) 1)

(check-expect (rest (list 1 2 3)) (list 2 3))

(check-expect (second (list 1 2 3)) 2)



;; 11.2 - Composing Functions



;; 11.3 - Auxiliary Functions that Recur


;; =================
;; Functions:

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-expect (sort> '()) '())
(check-expect (sort> (list 3 2 1))    (list 3 2 1))
(check-expect (sort> (list 1 2 3))    (list 3 2 1))
(check-expect (sort> (list 12 20 -5)) (list 20 12 -5))

(define (sort> l)
  (cond [(empty? l) '()]
        [(cons? l) (insert (first l) (sort> (rest l)))]))

; Number List-of-numbers -> List-of-numbers
; inserts n into the sorted list of numbers l
(check-expect (insert 5 '()) (list 5))
(check-expect (insert 5 (list 6)) (list 6 5))
(check-expect (insert 5 (list 4)) (list 5 4))
(check-expect (insert 12 (list 20 -5)) (list 20 12 -5))

(define (insert n l)
  (cond [(empty? l) (cons n '())]
        [else (if (>= n (first l))
                  (cons n l)
                  (cons (first l) (insert n (rest l))))]))

;; Exercise 186


;; =================
;; Functions:

; List-of-numbers -> Boolean
; produces true if the numbers are sorted in descending order
(check-expect (sorted>? '()) #true)
(check-expect (sorted>? (cons 1 (cons 2 '()))) #false)
(check-expect (sorted>? (cons 3 (cons 2 '()))) #true)
(check-expect (sorted>? (cons 0 (cons 3 (cons 2 '())))) #false)

(define (sorted>? l)
  (cond [(empty? l) #true]
        [(empty? (rest l)) #true]
        [else (and (>= (first l) (first (rest l)))
                   (sorted>? (rest l)))]))

; List-of-numbers -> List-of-numbers
; produces a sorted version of l
(check-satisfied (sort>/bad '()) empty?)
(check-satisfied (sort>/bad (list 3 2 1)) sorted>?)

(define (sort>/bad l)
  '(9 8 7 6 5 4 3 2 1 0))

;; Exercise 187


;; =================
;; Data definitions:

(define-struct gp [name score])
; A GamePlayer is a structure:
;    (make-gp String Number)
; interpretation (make-gp p s) represents player p who
; scored a maximum of s points
(define GP1 (make-gp "MDS" 1234))
(define GP2 (make-gp "Joseph" 12))
(define GP3 (make-gp "Peter"  -5))


;; =================
;; Functions:

; List-of-GamePlayer -> List-of-GamePlayer
; produces a sorted version of l
(check-expect (sort>.v1 '()) '())
(check-expect (sort>.v1 (list GP1 GP2 GP1)) (list GP1 GP1 GP2))
(check-expect (sort>.v1 (list GP3 GP2 GP1)) (list GP1 GP2 GP3))

(define (sort>.v1 l)
  (cond [(empty? l) '()]
        [(cons? l) (insert.v1 (first l) (sort>.v1 (rest l)))]))

; GamePlayer List-of-GamePlayer -> List-of-GamePlayer
; inserts gp into the sorted list of GamePlayer l
(check-expect (insert.v1 GP2 '()) (list GP2))
(check-expect (insert.v1 GP2 (list GP3)) (list GP2 GP3))
(check-expect (insert.v1 GP2 (list GP1)) (list GP1 GP2))
(check-expect (insert.v1 GP2 (list GP1 GP3)) (list GP1 GP2 GP3))

(define (insert.v1 gp l)
  (cond [(empty? l) (cons gp '())]
        [else (if (gte gp (first l))
                  (cons gp l)
                  (cons (first l) (insert.v1 gp (rest l))))]))

; GamePlayer GamePlayer -> Boolean
; compares two GamePlayer gp by score (gp1 >= gp2)
(check-expect (gte GP1 GP2) #true)
(check-expect (gte GP1 GP1) #true)
(check-expect (gte GP2 GP1) #false)

(define (gte gp1 gp2)
  (>= (gp-score gp1) (gp-score gp2)))

;; Exercise 188


;; =================
;; Data definitions:

(define-struct email [from date message])
; A Email Message is a structure:
;   (make-email String Number String)
; interpretation (make-email f d m) represents text m
; sent by f, d seconds after the beginning of time
(define E1 (make-email "MDS" 12 "Hello man!"))
(define E2 (make-email "Joseph" 30 "Hi bro!"))
(define E3 (make-email "Peter"  20 "Hi!"))


;; =================
;; Functions:

; List-of-Email -> List-of-Email
; produces a sorted version of l
(check-expect (sort1> '()) '())
(check-expect (sort1> (list E3 E2 E1)) (list E2 E3 E1))
(check-expect (sort1> (list E1 E2 E3)) (list E2 E3 E1))

(define (sort1> l)
  (cond [(empty? l) '()]
        [(cons? l) (insert1 (first l) (sort1> (rest l)))]))

; List-of-Email -> List-of-Email
; produces a sorted version of l
(check-expect (sort2> '()) '())
(check-expect (sort2> (list E2 E3 E1)) (list E1 E2 E3))
(check-expect (sort2> (list E1 E3 E2)) (list E1 E2 E3))

(define (sort2> l)
  (cond [(empty? l) '()]
        [(cons? l) (insert2 (first l) (sort2> (rest l)))]))

; Email List-of-Email -> List-of-Email
; inserts e into the sorted list of Email l
(check-expect (insert1 E1 '()) (list E1))
(check-expect (insert1 E2 (list E1)) (list E2 E1))
(check-expect (insert1 E2 (list E3)) (list E2 E3))
(check-expect (insert1 E3 (list E2 E1)) (list E2 E3 E1))

(define (insert1 e l)
  (cond [(empty? l) (cons e '())]
        [else (if (compare-by-date e (first l))
                  (cons e l)
                  (cons (first l) (insert1 e (rest l))))]))

; Email List-of-Email -> List-of-Email
; inserts e into the sorted list of Email l
(check-expect (insert2 E1 '()) (list E1))
(check-expect (insert2 E2 (list E1)) (list E1 E2))
(check-expect (insert2 E2 (list E3)) (list E2 E3))
(check-expect (insert2 E2 (list E1 E3)) (list E1 E2 E3))

(define (insert2 e l)
  (cond [(empty? l) (cons e '())]
        [else (if (compare-by-message e (first l))
                  (cons e l)
                  (cons (first l) (insert2 e (rest l))))]))

; Email Email -> Boolean
; compares two Email e by date (e1 >= e2)
(check-expect (compare-by-date E1 E2) #false)
(check-expect (compare-by-date E1 E1) #true)
(check-expect (compare-by-date E2 E1) #true)

(define (compare-by-date e1 e2)
  (>= (email-date e1) (email-date e2)))

; Email Email -> Boolean
; compares two Email e by message (e < e2)
(check-expect (compare-by-message E1 E2) #true)
(check-expect (compare-by-message E1 E1) #false)
(check-expect (compare-by-message E2 E1) #false)

(define (compare-by-message e1 e2)
  (string<? (email-message e1) (email-message e2)))

;; Exercise 189


;; =================
;; Functions:

; Number List-of-numbers -> Boolean
; determines whether some number occurs in a list of numbers
(check-expect (search 1 '()) #false)
(check-expect (search 1 (list 5 8 3 7 1)) #true)
(check-expect (search 2 (list 5 8 3 7 1)) #false)

(define (search n alon)
  (cond [(empty? alon) #false]
        [else (or (= (first alon) n)
                  (search n (rest alon)))]))

; Number List-of-numbers -> Boolean
; determines whether a number n occurs in a sorted list of numbers l
(check-expect (search-sorted 0 '()) #false)
(check-expect (search-sorted 0 (list 1 2 3 4 5)) #false)
(check-expect (search-sorted 1 (list 1 2 3 4 5)) #true)
(check-expect (search-sorted 5 (list 1 2 3 4 5)) #true)
(check-expect (search-sorted 6 (list 1 2 3 4 5)) #false)

(define (search-sorted n l)
  (search n l))

;; Exercise 190


;; =================
;; Functions:

; List-of-1Strings -> List-of-List-of-1Strings
; produces the list of all prefixes
(check-expect (prefixes '()) '())
(check-expect (prefixes (list "a"))         (list (list "a")))
(check-expect (prefixes (list "a" "b"))     (list (list "a" "b")
                                                  (list "a")))
(check-expect (prefixes (list "a" "b" "c")) (list (list "a" "b" "c")
                                                  (list "a" "b")
                                                  (list "a")))
(check-expect (prefixes (list "a" "b" "c" "d")) (list (list "a" "b" "c" "d")
                                                      (list "a" "b" "c")
                                                      (list "a" "b")
                                                      (list "a")))

(define (prefixes lo1s)
  (cond [(empty? lo1s) '()]
        [else (cons lo1s
                    (prefixes (reverse (rest (reverse lo1s)))))]))

; List-of-1Strings -> List-of-List-of-1Strings
; produces the list of all suffixes
(check-expect (suffixes '()) '())
(check-expect (suffixes (list "a"))         (list (list "a")))
(check-expect (suffixes (list "a" "b"))     (list (list "a" "b")
                                                  (list "b")))
(check-expect (suffixes (list "a" "b" "c")) (list (list "a" "b" "c")
                                                  (list "b" "c")
                                                  (list "c")))
(check-expect (suffixes (list "b" "c" "d")) (list (list "b" "c" "d")
                                                  (list "c" "d")
                                                  (list "d")))

(define (suffixes lo1s)
  (cond [(empty? lo1s) '()]
        [else (cons lo1s
                    (suffixes (rest lo1s)))]))



;; 11.4 - Auxiliary Functions that Generalize

;; Exercise 191
;; Exercise 192
;; Exercise 193
;; Exercise 194


;; =================
;; Constants:

; a plain background image
(define MT (empty-scene 50 50))


;; =================
;; Data definitions:

; A Polygon is one of:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)
(define triangle-p (list (make-posn 20 10)
                         (make-posn 20 20)
                         (make-posn 30 20)))
(define square-p   (list (make-posn 10 10)
                         (make-posn 20 10)
                         (make-posn 20 20)
                         (make-posn 10 20)))

; An NELoP is one of:
; - (cons Posn '())
; - (cons Posn NELoP)


;; =================
;; Functions:

; Image Polygon -> Image
; renders the given polygon p into img
(check-expect (render-poly MT triangle-p)
              (scene+line (scene+line
                           (scene+line MT 20 10 20 20 "red")
                           20 20 30 20 "red")
                          30 20 20 10 "red"))
(check-expect (render-poly MT square-p)
              (connect-dots MT square-p (first square-p)))

(define (render-poly img p)
  (connect-dots img p (first p)))

;; (define (render-polygon.v1 img p)
;;   (render-line (connect-dots img p) (first p) (last p)))

;; (define (render-polygon.v2 img p)
;;   (connect-dots img (cons (last p) p)))

;; (define (render-polygon.v3 img p)
;;   (connect-dots img (add-at-end p (first p))))

; Image NELoP -> Image
; connects the Posns in p in an image
(check-expect (connect-dots MT triangle-p (first triangle-p))
              (scene+line (scene+line
                           (scene+line MT 20 10 20 20 "red")
                           20 20 30 20 "red")
                          30 20 20 10 "red"))
(check-expect (connect-dots MT square-p (first square-p))
              (scene+line (scene+line
                           (scene+line
                            (scene+line MT 10 10 20 10 "red")
                            20 10 20 20 "red")
                           20 20 10 20 "red")
                          10 20 10 10 "red"))

(define (connect-dots img p q)
  (cond [(empty? (rest p)) (render-line img (first p) q)]
        [else (render-line (connect-dots img (rest p) q)
                           (first p)
                           (second p))]))

; Image Posn Posn -> Image
; renders a line from p to q into img
(check-expect (render-line MT (make-posn 2 10) (make-posn 20 20))
              (scene+line MT 2 10 20 20 "red"))

(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p) (posn-x q) (posn-y q)
              "red"))

; NELoP -> Posn
; extracts the last item from p
(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p)   (make-posn 10 20))

(define (last p)
  (cond [(empty? (rest (rest (rest p)))) (third p)]
        [else (last (rest p))]))

; NELoP Posn -> NELoP
; creates a new list by adding q to the end of p
(check-expect (add-at-end triangle-p (make-posn 0 0))
              (reverse (cons (make-posn 0 0) (reverse triangle-p))))

(define (add-at-end p q)
  (cond [(empty? p) (cons q '())]
        [else (cons (first p)
                    (add-at-end (rest p) q))]))
