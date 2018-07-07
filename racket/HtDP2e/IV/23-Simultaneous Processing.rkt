;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |23-Simultaneous Processing|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 23-Simultaneous Processing.rkt
;; IV - Intertwined Data
;; 23 - Simultaneous Processing



;; 23.1 Processing Two Lists Simultaneously: Case 1


;; =================
;; Functions:

; [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with '() '(a b))
              '(a b))
(check-expect (replace-eol-with (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

(define (replace-eol-with front end)
  (cond [(empty? front) end]
        [else
         (cons (first front)
               (replace-eol-with (rest front) end))]))

;; Exercise 387

; [List-of Symbol] [List-of Number] -> [List-of (list Symbol Number)]
; produces all possible ordered pairs of symbols and numbers
(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(check-expect (cross '() '(1 2))   '())
(check-expect (cross '(a b c) '()) '())

(define (cross los lon)
  (local (; Symbol [List-of Number] -> [List-of (list Symbol Number)
          (define (cross-one s lon)
            (cond [(empty? lon) '()]
                  [else
                   (cons (list s      (first lon))
                         (cross-one s (rest  lon)))])))
    (cond [(empty? los) '()]
          [else
           (append (cross-one (first los) lon)
                   (cross     (rest  los) lon))])))



;; 23.2 Processing Two Lists Simultaneously: Case 2


;; =================
;; Functions:

; [List-of Number] [List-of Number] -> [List-of Number]
; multiplies the corresponding items on
; hours and wages/h
; assume the two lists are of equal length
(check-expect (wages*.v2 '() '()) '())
(check-expect (wages*.v2 '(5.65) '(40))
              '(226.0))
(check-expect (wages*.v2 '(5.65 8.75) '(40.0 30.0))
              '(226.0 262.5))

(define (wages*.v2 hours wages/h)
  (cond [(empty? hours) '()]
        [else
         (cons (weekly-wage (first hours) (first wages/h))
               (wages*.v2   (rest  hours) (rest  wages/h)))]))

; Number Number -> Number
; computes the weekly wage from pay-rate and hours
(define (weekly-wage pay-rate hours)
  (* pay-rate hours))

;; Exercise 388


;; =================
;; Data definitions:

(define-struct employee [name ssn pay-rate])
; An Employee is a structure:
;   (make-employee String Number Number)
; interpretation (make-employee s n1 n2) specifies an employee
;   s: employee’s name;
;  n1: social security number; and
;  n2: pay rate
(define E0 (make-employee "John"  123 51.52))
(define E1 (make-employee "Marie" 456 62.39))

(define-struct work-record [name hours])
; A Work-Record is a structure:
;   (make-work-record String Number)
; interpretation (make-work-record s n) specifies an work record to an employee
;  s: employee’s name; and
;  n: number of hours worked in a week
(define WR0 (make-work-record "John"  12))
(define WR1 (make-work-record "Marie" 10))

(define-struct result [name weekly-wage])
; A Result is a structure:
;   (make-result String Number)
; interpretation (make-result s n) specifies the result of weekly wage for the employee
;  s: employee; and
;  n: weekly wage


(define one-list-employee    (list E0 E1))
(define one-list-work-record (list WR0 WR1))


;; =================
;; Functions:

; [List-of Employee] [List-of Work-Record] -> [List-of Result]
; computes the result of weekly wage for the employees
(check-expect (wages*.v3 one-list-employee one-list-work-record)
              (list (make-result (employee-name E0)
                                 (* (employee-pay-rate E0)
                                    (work-record-hours WR0)))
                    (make-result (employee-name E1)
                                 (* (employee-pay-rate E1)
                                    (work-record-hours WR1)))))

(define (wages*.v3 loe low)
  (cond [(empty? loe) '()]
        [else
         (cons (make-result (employee-name  (first loe))
                            (weekly-wage.v2 (first loe) (first low)))
               (wages*.v3 (rest loe) (rest low)))]))

; Employee [List-of Work-Record] -> Number
; computes the weekly wage of an employee
(define (weekly-wage.v2 e wr)
  (* (employee-pay-rate e)
     (work-record-hours wr)))

;; Exercise 389


;; =================
;; Data definitions:

(define-struct phone-record [name number])
; A PhoneRecord is a structure:
;   (make-phone-record String String)


;; =================
;; Functions:

; [List-of String] [List-of String] -> [List-of PhoneRecord]
; combines those equally long lists into a list of phone records
(check-expect (zip '() '("123-123-123" "234-234-234")) '())
(check-expect (zip '("John" "Marie") '()) '())
(check-expect (zip '("John" "Marie") '("123-123-123" "234-234-234"))
              (list (make-phone-record "John"  "123-123-123")
                    (make-phone-record "Marie" "234-234-234")))
(check-expect (zip '("John" "Marie" "Paul") '("123-123-123" "234-234-234"))
              (list (make-phone-record "John"  "123-123-123")
                    (make-phone-record "Marie" "234-234-234")))
(check-expect (zip '("John" "Marie") '("123-123-123" "234-234-234" "345-345-345"))
              (list (make-phone-record "John"  "123-123-123")
                    (make-phone-record "Marie" "234-234-234")))

(define (zip los1 los2)
  (cond [(or (empty? los1)
             (empty? los2)) '()]
        [else
         (cons (make-phone-record (first los1)
                                  (first los2))
               (zip (rest los1)
                    (rest los2)))]))



;; 23.3 Processing Two Lists Simultaneously: Case 3


;; =================
;; Data definitions:

; N is one of:
; - 0
; - (add1 N)


;; =================
;; Functions:

; [List-of Symbol] N -> Symbol
; extracts the nth symbol from l;
; signals an error if there is no such symbol
(check-expect (list-pick '(a b c) 2) 'c)
(check-error  (list-pick '() 0) "list-pick: list too short")
(check-expect (list-pick (cons 'a '()) 0) 'a)
(check-error  (list-pick '() 3) "list-pick: list too short")
(check-expect (list-pick '(a b) 1) 'b)

(define (list-pick l n)
  (cond [(and (= n 0) (empty? l))
         (error 'list-pick "list too short")]
        [(and (> n 0) (empty? l))
         (error 'list-pick "list too short")]
        [(and (= n 0) (cons? l)) (first l)]
        [(and (> n 0) (cons? l)) (list-pick (rest l) (sub1 n))]))

;; Exercise 390


;; =================
;; Data definitions:

(define-struct branch [left right])
; A Branch is a structure:
;   (make-branch Symbol Symbol)

; A TOS is one of:
; - Symbol
; - (make-branch TOS TOS)
(define TOS0 (make-branch (make-branch (make-branch 'a 'e)
                                       (make-branch 'b 'c))
                          (make-branch 'd 'e)))

; A Direction is one of:
; - 'left
; - 'right

; A list of Directions is also called a path.
(define PATH0 '(left right left))


;; =================
;; Functions:

; TOS Path -> Symbol
; extracts the symbol of the TOS for the given path
; signals an error when given a symbol and a non-empty path
(check-expect (tree-pick 'a   '()) 'a)
(check-error  (tree-pick TOS0 '()) "end of path")
(check-error  (tree-pick TOS0 '(left left left left left)) "end of tos")
(check-expect (tree-pick TOS0 PATH0) 'b)

(define (tree-pick tos path)
  (cond [(and (empty?  path)
              (symbol? tos)) tos]
        [(and (empty?  path)
              (branch? tos)) (error "end of path")]
        [(and (cons?   path)
              (symbol? tos)) (error "end of tos")]
        [(and (cons?   path)
              (branch? tos))
         (tree-pick (if (equal? 'left (first path))
                        (branch-left  tos)
                        (branch-right tos))
                    (rest path))]))
