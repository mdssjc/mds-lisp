;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |23-Simultaneous Processing|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 23-Simultaneous Processing.rkt
;; IV - Intertwined Data
;; 23 - Simultaneous Processing

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)


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



;; 23.4 Function Simplification


;; =================
;; Functions:

; list-pick: [List-of Symbol] N[>= 0] -> Symbol
; determines the nth symbol from alos, counting from 0;
; signals an error if there is no nth symbol
(check-expect (list-pick.v2 '(a b c) 2) 'c)
(check-error  (list-pick.v2 '() 0) "list-pick: list too short")
(check-expect (list-pick.v2 (cons 'a '()) 0) 'a)
(check-error  (list-pick.v2 '() 3) "list-pick: list too short")
(check-expect (list-pick.v2 '(a b) 1) 'b)

(define (list-pick.v2 alos n)
  (cond [(empty? alos) (error 'list-pick "list too short")]
        [(= n 0) (first alos)]
        [(> n 0) (list-pick.v2 (rest alos) (sub1 n))]))

;; Exercise 391

; replace-eol-with: [List-of Number] [List-of Number] -> [List-of Number]
; replaces the final '() in front with end
(check-expect (replace-eol-with.v2 '() '())
              '())
(check-expect (replace-eol-with.v2 '() '(a b))
              '(a b))
(check-expect (replace-eol-with.v2 (cons 1 '()) '())
              (cons 1 '()))
(check-expect (replace-eol-with.v2 (cons 1 '()) '(a))
              (cons 1 '(a)))
(check-expect (replace-eol-with.v2 (cons 2 (cons 1 '())) '(a))
              (cons 2 (cons 1 '(a))))

#;
(define (replace-eol-with.v2 front end)
  (cond [(and (empty? front)
              (empty? end)) '()]
        [(and (empty? front)
              (cons?  end)) end]
        [(and (cons?  front)
              (empty? end)) front]
        [(and (cons?  front)
              (cons?  end))
         (cons (first front)
               (replace-eol-with.v2 (rest front) end))]))

#;
(define (replace-eol-with.v2 front end)
  (cond [(empty? front) end]
        [(and (cons?  front)
              (empty? end)) front]
        [else
         (cons (first front)
               (replace-eol-with.v2 (rest front) end))]))

(define (replace-eol-with.v2 front end)
  (cond [(empty? front) end]
        [else
         (cons (first front)
               (replace-eol-with.v2 (rest front) end))]))

;; Exercise 392

; tree-pick: TOS Path -> Symbol
; extracts the symbol of the TOS for the given path
; signals an error when given a symbol and a non-empty path
(check-expect (tree-pick.v2 'a   '()) 'a)
(check-error  (tree-pick.v2 TOS0 '()) "end of path")
(check-error  (tree-pick.v2 TOS0 '(left left left left left)) "end of tos")
(check-expect (tree-pick.v2 TOS0 PATH0) 'b)

(define (tree-pick.v2 tos path)
  (cond [(empty? path) (if (symbol? tos)
                           tos
                           (error "end of path"))]
        [(symbol? tos) (error "end of tos")]
        [else
         (tree-pick.v2 (if (equal? 'left (first path))
                           (branch-left  tos)
                           (branch-right tos))
                       (rest path))]))



;; 23.5 Designing Functions that Consume Two Complex Inputs


;; =================
;; Data definitions:

; An LOD is one of:
; - '()
; - (cons Direction LOD)

; A TID is one of:
; - Symbol
; - (make-binary TID TID)
; - (make-with TID Symbol TID)
(define-struct with [lft info rght])
(define-struct binary [lft rght])



;; 23.6 Finger Exercises: Two Inputs

;; Exercise 393


;; =================
;; Data definitions:

; A Son.L is one of:
; - empty
; - (cons Number Son.L)
;
; Son is used when it
; applies to Son.L and Son.R

; A Son.R is one of:
; - empty
; - (cons Number Son.R)
;
; Constraint If s is a Son.R,
; no number occurs twice in s

(define S0 '())
(define S1 '(1 3 5))
(define S2 '(2 4 6))


;; =================
;; Functions:

; union: Son Son -> Son
; produces one that contains the elements of both
(check-expect (union S0 S0) S0)
(check-expect (union S1 S0) S1)
(check-expect (union S0 S2) S2)
(check-expect (union S1 S2) (append S1 S2))
(check-expect (union S1 S1) (append S1 S1))
(check-expect (union S2 S2) (append S2 S2))

(define (union s1 s2)
  (append s1 s2))

; intersect: Son Son -> Son
; produces the set of exactly those elements that occur in both
(check-expect (intersect S0 S0) S0)
(check-expect (intersect S1 S0) S1)
(check-expect (intersect S0 S2) S2)
(check-expect (intersect S1 S2) (append S1 S2))
(check-expect (intersect S1 S1) S1)
(check-expect (intersect S2 S2) S2)

(define (intersect s1 s2)
  (local (; Son Son -> Son
          (define (diff x l)
            (cond [(empty? x) '()]
                  [else
                   (if (member? (first x) l)
                       (diff    (rest  x) l)
                       (cons (first x)
                             (diff (rest x) l)))])))
    (cond [(or (empty? s1)
               (empty? s2)) (append s1 s2)]
          [else
           (append s1 (diff s2 s1))])))

;; Exercise 394

; merge: [List-of Number] [List-of Number] -> [List-of Number]
; produces a single sorted list of numbers that contains all the numbers on both inputs lists
; Invariant: sorted in ascending order
(check-expect (merge S0 S0) '())
(check-expect (merge S1 S0) S1)
(check-expect (merge S0 S2) S2)
(check-expect (merge S1 S2) '(1 2 3 4 5 6))
(check-expect (merge S1 S1) '(1 1 3 3 5 5))
(check-expect (merge S2 S2) '(2 2 4 4 6 6))

(define (merge lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(empty? lon2) lon1]
        [else
         (local ((define x (first lon1))
                 (define y (first lon2)))
           (cond [(<= x y)
                  (cons x (merge (rest lon1) lon2))]
                 [else
                  (cons y (merge lon1 (rest lon2)))]))]))

;; Exercise 395

; take: [List-of X] Natural -> [List-of X]
; produces the first n items from l or all of l if it is too short
(check-expect (take '() 0) '())
(check-expect (take '() 5) '())
(check-expect (take '(1 2 3 4 5) 0) '())
(check-expect (take '(1 2 3 4 5) 1) '(1))
(check-expect (take '(1 2 3 4 5) 2) '(1 2))
(check-expect (take '(1 2 3 4 5) 5) '(1 2 3 4 5))
(check-expect (take '(1 2 3 4 5) 6) '(1 2 3 4 5))
(check-expect (take '(a b c) 3) '(a b c))
(check-expect (take '(a b c) 4) '(a b c))

(define (take l n)
  (cond [(or (empty? l)
             (zero?  n)) '()]
        [else
         (cons (first l)
               (take (rest l) (sub1 n)))]))

; drop: [List-of X] Natural -> [List-of X]
; result is l with the first n items removed or just ’() if l is too short
(check-expect (drop '() 0) '())
(check-expect (drop '() 5) '())
(check-expect (drop '(1 2 3 4 5) 0) '(1 2 3 4 5))
(check-expect (drop '(1 2 3 4 5) 1) '(2 3 4 5))
(check-expect (drop '(1 2 3 4 5) 2) '(3 4 5))
(check-expect (drop '(1 2 3 4 5) 5) '())
(check-expect (drop '(1 2 3 4 5) 6) '())
(check-expect (drop '(a b c) 2) '(c))
(check-expect (drop '(a b c) 1) '(b c))

(define (drop l n)
  (cond [(or (empty? l)
             (zero?  n)) l]
        [else
         (drop (rest l) (sub1 n))]))

;; Exercise 396


;; =================
;; Constants:

; On OS X: /usr/share/dict/words
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
(define LOCATION "/usr/share/dict/words")


;; =================
;; Data definitions:

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))

; A LoL is one of:
; '()
; (cons Letter LoL)
; interpretation the list of Letters is a collection of Letter
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; An HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed


;; =================
;; Functions:

; play: HM-Word N -> String
; runs a simplistic hangman game, produces the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key  checked-compare]))))

; render-word: HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; compare-word: LoL HM-Word Letter -> HM-Word
; produces s with all "_" where the guess revealed a letter
(check-expect (compare-word (explode "START") (explode "_____") "S") (explode "S____"))
(check-expect (compare-word (explode "START") (explode "S____") "T") (explode "ST__T"))
(check-expect (compare-word (explode "START") (explode "ST__T") "G") (explode "ST__T"))

(define (compare-word w g s)
  (cond [(empty? w) '()]
        [else
         (cons (if (and (string=? (first g) "_")
                        (string=? (first w) s))
                   s (first g))
               (compare-word (rest w) (rest g) s))]))


;; Test Drive

;; (local ((define SIZE (length AS-LIST)))
;;   (play (list-ref AS-LIST (random SIZE)) 10))

;; Exercise 397


;; =================
;; Functions:

; wages*.v3b: [List-of Employee] [List-of Work-Record] -> [List-of Result]
; produces a list of wage records, which contain the name and weekly wage of an
; employee. The function signals an error if it cannot find an employee record
; for a time card or vice versa
(check-expect (wages*.v3b one-list-employee one-list-work-record)
              (list (make-result "John"  (* 51.52 12))
                    (make-result "Marie" (* 62.39 10))))
(check-error (wages*.v3b one-list-employee (list (make-work-record "Jones" 6)))
             "cannot find an employee record")

(define (wages*.v3b loe low)
  (local (;; [List-of Employee] Work-Record -> Wage
          (define (get-wage loe w)
            (cond [(empty? loe) (error "cannot find an employee record")]
                  [else
                   (local ((define e (first loe)))
                     (if (equal? (employee-name e) (work-record-name w))
                         (make-result (employee-name e)
                                      (* (work-record-hours w)
                                         (employee-pay-rate e)))
                         (get-wage (rest loe) w)))])))
    (cond [(empty? low) '()]
          [else
           (cons (get-wage   loe (first low))
                 (wages*.v3b loe (rest  low)))])))
