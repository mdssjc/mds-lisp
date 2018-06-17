;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |8-Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 8-Lists.rkt
;; II - Arbitrarily Large Data
;; 8 - Lists


;; 8.1 - Creating Lists

;; Exercise 129

; a list of celestial bodies
(cons "Mercury"
      (cons "Venus"
            (cons "Earth"
                  (cons "Mars"
                        (cons "Jupiter"
                              (cons "Saturn"
                                    (cons "Uranus"
                                          (cons "Neptune" '()))))))))

; a list of items for a meal
(cons "steak"
      (cons "frenck fries"
            (cons "beans"
                  (cons "bread"
                        (cons "water"
                              (cons "brie cheese"
                                    (cons "ice cream" '())))))))

; a list of colors
(cons "white"
      (cons "black"
            (cons "red"
                  (cons "green"
                        (cons "blue" '())))))

;; Exercise 130

; A List-of-names is one of:
; - '()
; - (cons String List-of-names)
; interpretation a list of invitees, by last name
(cons "Findler"
      (cons "Flatt"
            (cons "Felleisen"
                  (cons "Krishnamurthi"
                        (cons "dos Santos" '())))))

(cons "1" (cons "2" '())) ; "1" and "2" are strings, match with List-of-names
(cons 2 '())              ; 2 is a number, doesn't match with List-of-names

;; Exercise 131

; A List-of-booleans is one of:
; - '()
; - (cons Boolean List-of-booleans)
; interpretation a list of boolean values
(cons #true
      (cons #false
            (cons #true '())))



;; 8.2 - What Is '(), What Is cons



;; 8.3 - Programming with Lists


;; =================
;; Constants:

(define NAMES1 '())
(define NAMES2 (cons "Find" '()))
(define NAMES3 (cons "Flatt" '()))
(define NAMES4 (cons "X" (cons "Y" (cons "Z" '()))))
(define NAMES5 (cons "A" (cons "Flatt" (cons "C" '()))))
(define NAMES6 (cons "Fagan" (cons "Findler" (cons "Fisler" (cons "Flanagan" (cons "Flatt" (cons "Felleisen" (cons "Friedman" '()))))))))


;; =================
;; Functions:

;; Exercise 132

; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(check-expect (contains-flatt? NAMES1) #false)
(check-expect (contains-flatt? NAMES2) #false)
(check-expect (contains-flatt? NAMES3) #true)
(check-expect (contains-flatt? NAMES4) #false)
(check-expect (contains-flatt? NAMES5) #true)
(check-expect (contains-flatt? NAMES6) #true)

(define (contains-flatt? alon)
  (cond
    [(empty? alon) #false]
    [(cons?  alon)
     (or (string=? (first alon) "Flatt")
         (contains-flatt? (rest alon)))]))

;; Exercise 133

; List-of-names -> Boolean
; determines whether "Flatt" occurs on alon
(check-expect (contains-flatt?.v2 NAMES1) (contains-flatt? NAMES1))
(check-expect (contains-flatt?.v2 NAMES2) (contains-flatt? NAMES2))
(check-expect (contains-flatt?.v2 NAMES3) (contains-flatt? NAMES3))
(check-expect (contains-flatt?.v2 NAMES4) (contains-flatt? NAMES4))
(check-expect (contains-flatt?.v2 NAMES5) (contains-flatt? NAMES5))
(check-expect (contains-flatt?.v2 NAMES6) (contains-flatt? NAMES6))

(define (contains-flatt?.v2 alon)
  (cond
    [(empty? alon) #false]
    [(string=? (first alon) "Flatt") #true]
    [else (contains-flatt?.v2 (rest alon))]))

;; Exercise 134

; String List-of-Strings -> Boolean
; determines whether some given string occurs on a given list of strings
(check-expect (contains? "Flatt" NAMES1) #false)
(check-expect (contains? "Flatt" NAMES2) #false)
(check-expect (contains? "Flatt" NAMES3) #true)
(check-expect (contains? "Flatt" NAMES4) #false)
(check-expect (contains? "Flatt" NAMES5) #true)
(check-expect (contains? "Flatt" NAMES6) #true)

(define (contains? s los)
  (cond
    [(empty? los) #false]
    [else
     (or (string=? (first los) s)
         (contains? s (rest los)))]))



;; 8.4 - Computing with Lists

;; Exercise 135

(contains-flatt?.v2 (cons "Flatt" (cons "C" '())))
(cond [(empty? (cons "Flatt" (cons "C" '()))) #false]
      [(string=? (first (cons "Flatt" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [#false #false]
      [(string=? (first (cons "Flatt" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [(string=? (first (cons "Flatt" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [(string=? "Flatt" "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [#true #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
#true

; ---

(contains-flatt?.v2 (cons "A" (cons "Flatt" (cons "C" '()))))
(cond [(empty? (cons "A" (cons "Flatt" (cons "C" '())))) #false]
      [(string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))])
(cond [#false #false]
      [(string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))])
(cond [(string=? (first (cons "A" (cons "Flatt" (cons "C" '())))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))])
(cond [(string=? "A" "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))])
(cond [#false #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))])
(cond [else (contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))])
(contains-flatt?.v2 (rest (cons "A" (cons "Flatt" (cons "C" '())))))
(contains-flatt?.v2 (cons "Flatt" (cons "C" '())))
(cond [(empty? (cons "Flatt" (cons "C" '()))) #false]
      [(string=? (first (cons "Flatt" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [#false #false]
      [(string=? (first (cons "Flatt" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [(string=? (first (cons "Flatt" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [(string=? "Flatt" "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
(cond [#true #true]
      [else (contains-flatt?.v2 (rest (cons "Flatt" (cons "C" '()))))])
#true

; ---

(contains-flatt?.v2 (cons "A" (cons "B" (cons "C" '()))))
(cond [(empty? (cons "A" (cons "B" (cons "C" '())))) #false]
      [(string=? (first (cons "A" (cons "B" (cons "C" '())))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))])
(cond [#false #false]
      [(string=? (first (cons "A" (cons "B" (cons "C" '())))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))])
(cond [(string=? (first (cons "A" (cons "B" (cons "C" '())))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))])
(cond [(string=? "A" "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))])
(cond [#false #true]
      [else (contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))])
(cond [else (contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))])
(contains-flatt?.v2 (rest (cons "A" (cons "B" (cons "C" '())))))
(contains-flatt?.v2 (cons "B" (cons "C" '())))
(cond [(empty? (cons "B" (cons "C" '()))) #false]
      [(string=? (first (cons "B" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))])
(cond [#false #false]
      [(string=? (first (cons "B" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))])
(cond [(string=? (first (cons "B" (cons "C" '()))) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))])
(cond [(string=? "B" "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))])
(cond [#false #true]
      [else (contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))])
(cond [else (contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))])
(contains-flatt?.v2 (rest (cons "B" (cons "C" '()))))
(contains-flatt?.v2 (cons "C" '()))
(cond [(empty? (cons "C" '())) #false]
      [(string=? (first (cons "C" '())) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "C" '())))])
(cond [#false #false]
      [(string=? (first (cons "C" '())) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "C" '())))])
(cond [(string=? (first (cons "C" '())) "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "C" '())))])
(cond [(string=? "C" "Flatt") #true]
      [else (contains-flatt?.v2 (rest (cons "C" '())))])
(cond [#false #true]
      [else (contains-flatt?.v2 (rest (cons "C" '())))])
(cond [else (contains-flatt?.v2 (rest (cons "C" '())))])
(contains-flatt?.v2 (rest (cons "C" '())))
(contains-flatt?.v2 '())
(cond [(empty? '()) #false]
      [(string=? (first '()) "Flatt") #true]
      [else (contains-flatt?.v2 (rest '()))])
(contains-flatt?.v2 '())
(cond [#true #false]
      [(string=? (first '()) "Flatt") #true]
      [else (contains-flatt?.v2 (rest '()))])
#false

;; Exercise 136


;; =================
;; Data definitions:

(define-struct pair [left right])
; A ConsPair is a structure:
;   (make-pair Any Any)

; A ConsOrEmpty is one of:
; - '()
; - (make-pair Any ConsOrEmpty)
; interpretation ConsOrEmpty is the class of all lists


;; =================
;; Functions:

; Any Any -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair?  a-list) (make-pair a-value a-list)]
    [else (error "cons: second argument ...")]))

; ConsOrEmpty -> Any
; extracts the left part of the given pair
(check-error  (our-first '()) "our-first: ...")
(check-expect (our-first (our-cons "a" '())) "a")

(define (our-first a-list)
  (if (empty? a-list)
      (error 'our-first "...")
      (pair-left a-list)))

; ConsOrEmpty -> Any
; extracts the right part of the given pair
(check-error  (our-rest '()) "our-rest: ...")
(check-expect (our-rest (our-cons "a" '())) '())

(define (our-rest a-list)
  (if (empty? a-list)
      (error 'our-rest "...")
      (pair-right a-list)))


(our-first (our-cons "a" '()))
(our-first (make-pair "a" '()))
(if (empty? (make-pair "a" '()))
    (error 'our-first "...")
    (pair-left (make-pair "a" '())))
(if #false
    (error 'our-first "...")
    (pair-left (make-pair "a" '())))
(pair-left (make-pair "a" '()))
"a"

; ---

(our-rest (our-cons "a" '()))
(our-rest (make-pair "a" '()))
(if (empty? (make-pair "a" '()))
    (error 'our-rest "...")
    (pair-right (make-pair "a" '())))
(if #false
    (error 'our-rest "...")
    (pair-right (make-pair "a" '())))
(pair-right (make-pair "a" '()))
'()
