;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |16-Using Abstractions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 16-Using Abstractions.rkt
;; III - Abstraction
;; 16  - Using Abstractions

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require 2htdp/itunes)


;; 16.1 - Existing Abstractions

; [X] N [N -> X] -> [List-of X]
; constructs a list by applying f to 0, 1, ..., (sub1 n)
; (build-list n f) == (list (f 0) ... (f (- n 1)))
; (define (build-list n f) ...)

(build-list 3 add1)

; [X] [X -> Boolean] [List-of X] -> [List-of X]
; produces a list from those items on lx for which p holds
; (define (filter p lx) ...)

(filter odd? (list 1 2 3 4 5))

; [X] [List-of X] [X X -> Boolean] -> [List-of X]
; produces a version of lx that is sorted according to cmp
; (define (sort lx cmp) ...)

(sort (list 3 2 1 4 5) >)

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying f to each item on lx
; (map f (list x-1 ... x-n)) == (list (f x-1) ... (f x-n))
; (define (map f lx) ...)

(map add1 (list 1 2 2 3 3 3))

; [X] [X -> Boolean] [List-of X] -> Boolean
; determines whether p holds for every item on lx
; (andmap p (list x-1 ... x-n)) == (and (p x-1) ... (p x-n))
; (define (andmap p lx) ...)

(andmap odd? (list 1 2 3 4 5))

; [X] [X -> Boolean] [List-of X] -> Boolean
; determines whether p holds for at least one item on lx
; (ormap p (list x-1 ... x-n)) == (or (p x-1) ... (p x-n))
; (define (ormap p lx) ...)

(ormap odd? (list 1 2 3 4 5))


; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from right to left to each item in lx and b
; (foldr f b (list x-1 ... x-n)) == (f x-1 ... (f x-n b))
; (define (foldr f b lx) ...)

(foldr + 0 '(1 2 3 4 5))

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; applies f from left to right to each item in lx and b
; (foldl f b (list x-1 ... x-n)) == (f x-n ... (f x-1 b))
; (define (foldl f b lx) ...)

(foldl + 0 '(1 2 3 4 5))


;; Exercise 256


;; =================
;; Data definitions:

; A [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])
; interpretation non-empty lists of X
(define NEL1 (list 5 2 4 7 1))
(define NEL2 (list (make-posn 2 4) (make-posn 3 1) (make-posn 1 5)))


;; =================
;; Functions:

; [X] [X -> Number] [NEList-of X] -> X
; finds the (first) item in lx that maximizes f
; if (argmax f (list x-1 ... x-n)) == x-i,
; then (>= (f x-i) (f x-1)), (>= (f x-i) (f x-2)), ...
(check-expect (argmax  add1 NEL1) 7)
(check-expect (argmax2 add1 NEL1) 7)
(check-expect (argmax  posn-x NEL2) (make-posn 3 1))
(check-expect (argmax2 posn-x NEL2) (make-posn 3 1))
(check-expect (argmax  posn-y NEL2) (make-posn 1 5))
(check-expect (argmax2 posn-y NEL2) (make-posn 1 5))

(define (argmax2 f lx)
  (cond [(empty? (rest lx)) (first lx)]
        [else
         (if (>= (f (first  lx))
                 (f (argmax2 f (rest lx))))
             (first lx)
             (argmax2 f (rest lx)))]))

; argmin: Finds the (first) element of the list that minimizes the output of the function.


(define-struct address [first-name last-name street])
; An Addr is a structure:
;   (make-address String String String)
; interpretation associates an address with a person's name

; [List-of Addr] -> String
; creates a string from first names,
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing l)
  (foldr string-append-with-space " "
         (sort (map address-first-name l) string<?)))

; String String -> String
; appends two strings, prefixes with " "
(define (string-append-with-space s t)
  (string-append " " s t))

(define ex0 (list (make-address "Robert"  "Findler" "South")
                  (make-address "Matthew" "Flatt"   "Canyon")
                  (make-address "Shriram" "Krishna" "Yellow")))

(check-expect (listing ex0) " Matthew Robert Shriram ")


;; Exercise 257


;; =================
;; Functions:

; [X Y] [X Y -> Y] Y [List-of X] -> Y
; f*oldl works just like foldl
(check-expect (f*oldl cons '() '(a b c)) (foldl cons '() '(a b c)))
(check-expect (f*oldl / 1 '(6 3 2))      (foldl / 1 '(6 3 2)))

(define (f*oldl f e l)
  (foldr f e (reverse l)))

; Natural [Natural -> X] -> [List-of X]
; build-l*st works just like build-list
(check-expect (build-l*st 0 add1) (build-list 0 add1))
(check-expect (build-l*st 5 add1) (build-list 5 add1))

(define (build-l*st n f)
  (cond [(zero? n) '()]
        [else
         (append (build-l*st (sub1 n) f)
                 (list (f (sub1 n))))]))



;; 16.2 - Local Definitions

; (local (def ...)
;   ; — IN —
;   body-expression)

; [List-of Addr] -> String
; creates a string of first names,
; sorted in alphabetical order,
; separated and surrounded by blank spaces
(define (listing.v2 l)
  (local (; 1. extract names
          (define names  (map address-first-name l))
          ; 2. sort the names
          (define sorted (sort names string<?))
          ; 3. append them, add spaces
          ; String String -> String
          ; appends two strings, prefix with " "
          (define (helper s t)
            (string-append " " s t))
          (define concat+spaces
            (foldr helper " " sorted)))
    concat+spaces))

; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
; produces a version of alon, sorted according to cmp
(define (sort-cmp alon0 cmp)
  (local (; [List-of Number] -> [List-of Number]
          ; produces the sorted version of alon
          (define (isort alon)
            (cond [(empty? alon) '()]
                  [else
                   (insert (first alon) (isort (rest alon)))]))

          ; Number [List-of Number] -> [List-of Number]
          ; inserts n into the sorted list of numbers alon
          (define (insert n alon)
            (cond [(empty? alon) (cons n '())]
                  [else
                   (if (cmp n (first alon))
                       (cons n alon)
                       (cons (first alon)
                             (insert n (rest alon))))])))
    (isort alon0)))

;; Exercise 258


;; =================
;; Constants:

; a plain background image
(define MT (empty-scene 50 50))


;; =================
;; Data definitions:

; A Polygon is one of:
; - (list Posn Posn Posn)
; - (cons Posn Polygon)
(define P1 (list (make-posn 20 20) (make-posn 30 20) (make-posn 30 30)))


;; =================
;; Functions:

; Image Polygon -> Image
; adds an image of p to MT
(define (render-polygon img p)
  (local (; Image Posn Posn -> Image
          ; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line im (posn-x p) (posn-y p) (posn-x q) (posn-y q) "red"))
          ; Polygon -> Posn
          ; extracts the last item from p
          (define (last p)
            (cond [(empty? (rest (rest (rest p)))) (third p)]
                  [else
                   (last (rest p))]))
          ; Image NELoP -> Image
          ; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond [(empty? (rest p)) MT]
                  [else
                   (render-line (connect-dots img (rest p))
                                (first p)
                                (second p))])))
    (render-line (connect-dots img p) (first p) (last p))))


(render-polygon MT P1)

;; Exercise 259


;; =================
;; Data definitions:

; A 1String is a String of length 1,
; including
; - "\\" (the backslash),
; - " " (the space bar),
; - "\t" (tab),
; - "\r" (return), and
; - "\b" (backspace).
; interpretation represents keys on the keyboard

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a String as a list of 1Strings (letters)

; A [List-of Word] is one of:
; - '() or
; - (cons Word [List-of Word])
; interpretation a collection of Word values


;; =================
;; Functions:

; [List-of String] -> Boolean
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

; String -> [List-of String]
; find all words that the letters of some given word spell
(check-member-of (alternative-words "cat") (list "act" "cat") (list "cat" "act"))
(check-satisfied (alternative-words "rat") all-words-from-rat?)
(check-expect    (alternative-words "")     '())
(check-expect    (alternative-words "test") '())

(define (alternative-words s)
  (local (; String -> Boolean
          ; verifies that the word is in the dictionary
          (define (in-dictionary? w)
            (member? w (list "rat" "art" "tar" "act" "cat" "hello" "world")))
          ; Word -> [List-of Word]
          ; creates all rearrangements of the letters in w
          (define (arrangements w)
            (cond [(empty? w) (list '())]
                  [else
                   (insert-everywhere/in-all-words (first w)
                                                   (arrangements (rest w)))]))
          ; 1String [List-of Word] -> [List-of Word]
          ; result is a list of words like its second argument,
          ; but with the first argument inserted at the beginning,
          ; between all letters, and at the end of all words of the given list
          (define (insert-everywhere/in-all-words 1s low)
            (cond [(empty? low) '()]
                  [else
                   (append (insert-everywhere/in-word 1s '()  (first low))
                           (insert-everywhere/in-all-words 1s (rest low)))]))
          ; 1String Word Word -> [List-of Word]
          ; arrangements the words (prefix sp and suffix ss) with 1String 1s
          (define (insert-everywhere/in-word 1s sp ss)
            (local ((define lst (append sp (list 1s) ss)))
              (cond [(empty? ss) (list lst)]
                    [else
                     (cons lst
                           (insert-everywhere/in-word 1s
                                                      (append sp (list (first ss)))
                                                      (rest ss)))])))
          ; Trampoline
          (define fn-for-alternative-words
            (filter in-dictionary? (map implode (arrangements (explode s))))))
    fn-for-alternative-words))


; Nelon -> Number
; determines the smallest number on l
(define (inf.v2 l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (local ((define smallest-in-rest (inf.v2 (rest l))))
           (if (< (first l) smallest-in-rest)
               (first l)
               smallest-in-rest))]))


;; Exercise 260


;; =================
;; Constants:

; A [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])
; interpretation non-empty lists of X
(define L1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(define L2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))


;; =================
;; Functions:

; Nelon -> Number
; determines the smallest number on l
#;
(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (if (< (first l)
                (inf (rest l)))
             (first l)
             (inf (rest l)))]))

(define (inf l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (min (first l)
              (inf (rest l)))]))


; Tests
(time (inf L1))
(time (inf L2))
(time (inf.v2 L1))
(time (inf.v2 L2))

;; Exercise 261


;; =================
;; Data definitions:

(define-struct ir [name price])
; An IR is a structure:
;   (make-ir String Number)
; An Inventory is one of:
; - '()
; - (cons IR Inventory)
(define IR1 '())
(define IR2  (list (make-ir "doll" 21.0)
                   (make-ir "bear" 13.0)
                   (make-ir "ball" 0.50)
                   (make-ir "tv"   5.0)
                   (make-ir "pen"  0.25)))
(define IR3 (append IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1 IR1))


;; =================
;; Functions:

; Inventory -> Inventory
; creates an Inventory from an-inv for all
; those items that cost less than a dollar
(check-expect (extract1 IR1) (extract2 IR1))
(check-expect (extract1 IR2) (extract2 IR2))
(check-expect (extract1 IR3) (extract2 IR3))

(define (extract1 an-inv)
  (cond [(empty? an-inv) '()]
        [else
         (cond [(<= (ir-price (first an-inv)) 1.0)
                (cons (first an-inv)
                      (extract1 (rest an-inv)))]
               [else
                (extract1 (rest an-inv))])]))

(define (extract2 an-inv)
  (cond [(empty? an-inv) '()]
        [else
         (local ((define ext (extract2 (rest an-inv)))
                 (define item (first an-inv)))
           (cond [(<= (ir-price item) 1.0) (cons item ext)]
                 [else ext]))]))


; Tests
(time (extract1 IR1))
(time (extract1 IR2))
(time (extract1 IR3))
(time (extract2 IR1))
(time (extract2 IR2))
(time (extract2 IR3))



;; 16.3 - Local Definitions Add Expressive Power


;; =================
;; Data definitions:

; An FSM is one of:
;   - '()
;   - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes


;; =================
;; Functions:

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate fsm s0)
  (local (; State of the World: FSM-State
          ; FSM-State KeyEvent -> FSM-State
          (define (find-next-state s key-event)
            (find fsm s)))
    (big-bang s0
              [to-draw state-as-colored-square]
              [on-key  find-next-state])))

; FSM-State -> Image
; renders current state as colored square
(define (state-as-colored-square s)
  (square 100 "solid" s))

; FSM FSM-State -> FSM-State
; finds the current state in fsm
(define (find transitions current)
  (cond [(empty? transitions) (error "not found")]
        [else
         (local ((define s (first transitions)))
           (if (state=? (transition-current s) current)
               (transition-next s)
               (find (rest transitions) current)))]))

; FSM-State FSM-State -> Boolean
; checks an equality predicate for states
(check-expect (state=? "white" "white")  #true)
(check-expect (state=? "white" "yellow") #false)
(check-expect (state=? "start" "start")  #true)
(check-expect (state=? "start" "expect") #false)

(define (state=? s1 s2)
  (string=? s1 s2))

;; Exercise 262


;; =================
;; Functions:

; Natural -> [List-of [List-of Natural]]
; creates diagonal squares of 0s and 1s
(check-expect (identityM 0) '())
(check-expect (identityM 1) (list (list 1)))
(check-expect (identityM 3) (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (identityM n)
  (local ((define (rows i)
            (cond [(zero? i) '()]
                  [else
                   (cons (numbers n i)
                         (rows (sub1 i)))]))
          (define (numbers n s)
            (cond [(zero? n) '()]
                  [else
                   (cons (if (= n s) 1 0)
                         (numbers (sub1 n) s))])))
    (rows n)))



;; 16.4 - Computing with local

;; Exercise 263

(= (inf.v2 (list 2 1 3)) 1)

;; Exercise 264


;; =================
;; Data definitions:

; A [NEList-of X] is one of:
; - (cons X '())
; - (cons X [NEList-of X])
; interpretation non-empty lists of X


;; =================
;; Functions:

; Nelon -> Number
; determines the largest number on l
(define (sup.v2 l)
  (cond [(empty? (rest l)) (first l)]
        [else
         (local ((define smallest-in-rest (sup.v2 (rest l))))
           (if (> (first l) smallest-in-rest)
               (first l)
               smallest-in-rest))]))

(= (sup.v2 (list 2 1 3)) 3)

;; Exercise 265

(define result.v1 (local ((define (f x)
                            (+ (* 4 (sqr x)) 3)))
                    f))
(result.v1 1)

;; Exercise 266

(define result.v2 (local ((define (f x) (+ x 3))
                          (define (g x) (* x 4)))
                    (if (odd? (f (g 1)))
                        f
                        g)))
(result.v2 2)



;; 16.5 - Using Abstractions, by Example


;; =================
;; Functions:

; [List-of Posn] -> [List-of Posn]
; adds 3 to each x-coordinate on the given list
(check-expect (add-3-to-all (list (make-posn 3 1) (make-posn 0 0)))
              (list (make-posn 6 1) (make-posn 3 0)))

#;
(define (add-3-to-all lop) '())

#;
(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; ...
          (define (fp p)
            ... p ...))
    (map fp lop)))

(define (add-3-to-all lop)
  (local (; Posn -> Posn
          ; adds 3 to the x-coordinate of p
          (define (add-3-to-1 p)
            (make-posn (+ (posn-x p) 3) (posn-y p))))
    (map add-3-to-1 lop)))

; [List-of Posn] -> [List-of Posn]
; eliminates Posns whose y-coordinate is > 100
(check-expect (keep-good (list (make-posn 0 110) (make-posn 0 60)))
              (list (make-posn 0 60)))

#;
(define (keep-good lop) '())

#;
(define (keep-good lop)
  (local (; Posn -> Boolean
          ; should this Posn stay on the list
          (define (good? p) #true))
    (filter good? lop)))

(define (keep-good lop)
  (local (; Posn -> Posn
          ; should this Posn stay on the list
          (define (good? p)
            (not (> (posn-y p) 100))))
    (filter good? lop)))

; Posn Posn Number -> Boolean
; is the distance between p and q less than d
(define (close-to p q d)
  (<= (sqrt (+ (sqr (- (posn-x q) (posn-x p)))
               (sqr (- (posn-y q) (posn-y p)))))
      d))

; [List-of Posn] Posn -> Boolean
; is any Posn on lop close to pt
(check-expect (close? (list (make-posn 47 54) (make-posn 0 60)) (make-posn 50 50))
              #true)

#;
(define (close? lop pt) #false)

#;
(define (close? lop pt)
  (local (; Posn -> Boolean
          ; ...
          (define (is-one-close? p)
            ...))
    (ormap close-to? lop)))

(define (close? lop pt)
  (local (; Posn -> Boolean
          ; is one shot close to pt
          (define (is-one-close? p)
            (close-to p pt CLOSENESS)))
    (ormap is-one-close? lop)))

(define CLOSENESS 5) ; in terms of pixels



;; 16.6 - Designing with Abstractions


;; =================
;; Constants:

(define WIDTH  100)
(define HEIGHT 180)
(define DOT (circle 4 "solid" "red"))
(define MT-SCENE (empty-scene WIDTH HEIGHT))


;; =================
;; Functions:

; [List-of Posn] -> Image
; adds the Posns on lop to the empty scene
(check-expect (dots (list (make-posn 12 31)))
              (place-image DOT 12 31 MT-SCENE))

#;
(define (dots lop)
  MT-SCENE)

#;
(define (dots lop)
  (local (; Posn Image -> Image
          (define (add-one-dot p scene) ...))
    (foldr add-one-dot MT-SCENE lop)))

(define (dots lop)
  (local (; Posn Image -> Image
          ; adds a DOT at p to scene
          (define (add-one-dot p scene)
            (place-image DOT
                         (posn-x p) (posn-y p)
                         scene)))
    (foldr add-one-dot MT-SCENE lop)))

; foldr : [X Y] [X Y -> Y] Y [List-of X] -> Y
; foldl : [X Y] [X Y -> Y] Y [List-of X] -> Y



;; 16.7 - Finger Exercises: Abstraction

;; Exercise 267


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
  (local ((define (exchange-rate n)
            (* n US-DOLLAR-RATE)))
    (map exchange-rate lon)))

; [List-of Number] -> [List-of Number]
; converts a list of Fahrenheit measurements to a list of Celsius measurements
(check-expect (convertFC '()) '())
(check-within (convertFC '(0 10 20 30 40 50))
              '(-17.78 -12.22 -6.67 -1.11 4.44 10) 0.01)

(define (convertFC lon)
  (local ((define (fahrenheit->celsius f)
            (/ (- f 32) 1.8)))
    (map fahrenheit->celsius lon)))

; [List-of Posn] -> [List-of [List-of Number]]
; translates a list of Posns into a list of list of pairs of numbers
(check-expect (translate '()) '())
(check-expect (translate (list (make-posn 1 2) (make-posn 5 3) (make-posn 9 6)))
              '((1 2) (5 3) (9 6)))

(define (translate lop)
  (local ((define (posn->list p)
            (list (posn-x p) (posn-y p))))
    (map posn->list lop)))

;; Exercise 268


;; =================
;; Data definitions:

(define-struct inventory [name description acq-price sales-price])
; An Inventory is a structure:
;   (make-inventory String String Number Number)
; interpretation (make-inventory n d ap sp) specifies
;   n: the name of an item;
;   d: a description;
;  ap: the acquisition price; and
;  sp: the recommended sales price
(define I1 (make-inventory "Inv1" "Description 1" 12.1 16.8))
(define I2 (make-inventory "Inv2" "Description 2" 14.1 15.8))
(define I3 (make-inventory "Inv3" "Description 3" 10.1 14.8))

; [List-of Inventory] is one of:
; - '()
; - (cons Inventory [List-of Inventory])
; interpretation is a list of inventories
(define LOI1 '())
(define LOI2 (list I1))
(define LOI3 (list I1 I2 I3))


;; =================
;; Functions:

; [List-of Inventory] -> [List-of Inventory]
; sorts a list of inventory records by the difference between the two prices
(check-expect (sort-loi LOI1) '())
(check-expect (sort-loi LOI2) LOI2)
(check-expect (sort-loi LOI3) (list I2 I1 I3))

(define (sort-loi loi)
  (local ((define (smaller? i1 i2)
            (< (abs (- (inventory-acq-price   i1)
                       (inventory-sales-price i1)))
               (abs (- (inventory-acq-price   i2)
                       (inventory-sales-price i2))))))
    (sort loi smaller?)))

;; Exercise 269

; Number [List-of Inventory] -> [List-of Inventory]
; produces a list of all those structures whose sales price is below ua
(check-expect (eliminate-expensive 16 LOI1) '())
(check-expect (eliminate-expensive 16 LOI2) '())
(check-expect (eliminate-expensive 16 LOI3) (list I2 I3))

(define (eliminate-expensive ua loi)
  (local ((define (below? i)
            (<= (inventory-sales-price i) ua)))
    (filter below? loi)))

; String [List-of Inventory] -> [List-of Inventory]
; produces a list of inventory records that do not use the name ty
(check-expect (recall "Inv2" LOI1) '())
(check-expect (recall "Inv1" LOI2) '())
(check-expect (recall "Inv2" LOI2) (list I1))
(check-expect (recall "Inv2" LOI3) (list I1 I3))

(define (recall ty loi)
  (local ((define (not-equals? i)
            (not (string=? (inventory-name i) ty))))
    (filter not-equals? loi)))

; [List-of String] [List-of String] -> [List-of String]
; selects all those from the second one that are also on the first
(check-expect (selection '() '()) '())
(check-expect (selection '("A" "B" "C") '()) '())
(check-expect (selection '() '("A" "B" "C")) '())
(check-expect (selection '("A" "C" "D") '("A" "B" "C")) '("A" "C"))

(define (selection los1 los2)
  (local ((define (contains? s)
            (member? s los2)))
    (filter contains? los1)))

;; Exercise 270


;; =================
;; Functions:

; Number -> [List-of Number]
; creates the list (list 0 ... (- n 1)) for any natural number n
(check-expect (f1 5) '(0 1 2 3 4))

(define (f1 n)
  (map sub1 (build-list n add1)))

; Number -> [List-of Number]
; creates the list (list 1 ... n) for any natural number n
(check-expect (f2 5) '(1 2 3 4 5))

(define (f2 n)
  (build-list n add1))

; Number -> [List-of Number]
; creates the list (list 1 1/2 ... 1/n) for any natural number n
(check-expect (f3 5) '(1/1 1/2 1/3 1/4 1/5))

(define (f3 n)
  (local ((define (reciprocal n)
            (/ 1 n)))
    (map reciprocal (build-list n add1))))

; Number -> [List-of Number]
; creates the list of the first n even numbers
(check-expect (f4 5) '(0 2 4 6 8))

(define (f4 n)
  (local ((define (doubles n)
            (* n 2)))
    (build-list n doubles)))

; Number -> [List-of [List-of Number]]
; creates a diagonal square of 0s and 1s
(check-expect (f5 3) '((1 0 0)
                       (0 1 0)
                       (0 0 1)))

(define (f5 n)
  (local ((define (identityM i)
            (numbers n (- n i)))
          (define (numbers n s)
            (cond [(zero? n) '()]
                  [else
                   (cons (if (= n s) 1 0)
                         (numbers (sub1 n) s))])))
    (build-list n identityM)))

; [Number -> X] Number -> [List-of X]
; tabulates a f function n times
(check-expect (tabulate add1 5)
              '(6 5 4 3 2 1))
(check-expect (tabulate number->string 5)
              '("5" "4" "3" "2" "1" "0"))

(define (tabulate f n)
  (reverse (build-list (add1 n) f)))

;; Exercise 271


;; =================
;; Functions:

; String [List-of String] -> Boolean
; determines whether any of the names on the latter are equal to or an extension of the former
(check-expect (find-name? "john" '())                  #false)
(check-expect (find-name? "john" '("marie"))           #false)
(check-expect (find-name? "john" '("john"))            #true)
(check-expect (find-name? "john" '("marie" "john"))    #true)
(check-expect (find-name? "jo"   '("marie" "john"))    #true)
(check-expect (find-name? "john" '("marie" "johnson")) #true)

(define (find-name? s los)
  (local ((define (name=? x)
            (string-contains? s x)))
    (ormap name=? los)))

; [List-of String] -> Boolean
; checks all names on a list of names start with the letter "a"
(check-expect (check-name? '())               #false)
(check-expect (check-name? '("marie"))        #false)
(check-expect (check-name? '("arie"))         #true)
(check-expect (check-name? '("marie" "arie")) #false)
(check-expect (check-name? '("alf" "arie"))   #true)

(define (check-name? los)
  (local ((define (check-names los)
            (cond [(empty? los) #false]
                  [else
                   (andmap start-with-a? los)]))
          (define (start-with-a? s)
            (string=? "a" (string-ith s 0))))
    (check-names los)))

; Number [List-of String] -> Boolean
; ensures that no name on some list exceeds some given width
(check-expect (exceed-width? 5 '())                        #false)
(check-expect (exceed-width? 5 '("alf"))                   #false)
(check-expect (exceed-width? 5 '("alf" "marie"))           #false)
(check-expect (exceed-width? 5 '("alf" "marie" "johnson")) #true)

(define (exceed-width? n los)
  (local ((define (larger-n? s)
            (> (string-length s) n)))
    (ormap larger-n? los)))

;; Exercise 272

(equal? (append '(1 2 3) '(4 5 6 7 8))
        '(1 2 3 4 5 6 7 8))


;; =================
;; Functions:

; [List-of Number] [List-of Number] -> [List-of Number]
; concatenates the items of two lists
(check-expect (append-from-fold '(1 2 3) '(4 5 6 7 8))
              '(1 2 3 4 5 6 7 8))

(define (append-from-fold lon1 lon2)
  (local ((define (append-item i lst)
            (cons i lst)))
    (foldr append-item lon2 lon1)))

; [List-of Number] -> Number
; computes the sum of a list of numbers
(check-expect (sum '())  0)
(check-expect (sum '(1)) 1)
(check-expect (sum '(1 2 3)) 6)

(define (sum lon)
  (foldr + 0 lon))

; [List-of Number] -> Number
; computes the product of a list of numbers
(check-expect (product '())  1)
(check-expect (product '(1)) 1)
(check-expect (product '(1 2 3)) 6)

(define (product lon)
  (foldr * 1 lon))

; [List-of Image] -> Image
; composes a list of Images horizontally
(check-expect (compose-images '()) empty-image)
(check-expect (compose-images (list (circle 5 "solid" "blue")))
              (circle 5 "solid" "blue"))
(check-expect (compose-images (list (circle 5 "solid" "blue") (square 10 "solid" "red")))
              (beside (circle 5 "solid" "blue") (square 10 "solid" "red")))

(define (compose-images loi)
  (foldr beside empty-image loi))

;; Exercise 273


;; =================
;; Functions:

(define (doubles n) (* n 2))

; [X Y] [X -> Y] [List-of X] -> [List-of Y]
; maps fn for each item in the list
(check-expect (map2 doubles '())  '())
(check-expect (map2 doubles '(1)) '(2))
(check-expect (map2 doubles '(1 2 3)) '(2 4 6))

(define (map2 fn lox)
  (local ((define (func x lst)
            (cons (fn x)
                  lst)))
    (foldr func '() lox)))

;; Exercise 274


;; =================
;; Functions:

; [[List-of X] -> [List-of X]] [List-of X] -> [List-of [List-of X]]
; produces the list of all fixes
(check-expect (fixes prefixes '())                '())
(check-expect (fixes prefixes '("a"))             '(("a")))
(check-expect (fixes prefixes '("a" "b"))         '(("a" "b") ("a")))
(check-expect (fixes prefixes '("a" "b" "c"))     '(("a" "b" "c") ("a" "b") ("a")))
(check-expect (fixes prefixes '("a" "b" "c" "d")) '(("a" "b" "c" "d")
                                                    ("a" "b" "c")
                                                    ("a" "b")
                                                    ("a")))
(check-expect (fixes suffixes '())                '())
(check-expect (fixes suffixes '("a"))             '(("a")))
(check-expect (fixes suffixes '("a" "b"))         '(("a" "b") ("b")))
(check-expect (fixes suffixes '("a" "b" "c"))     '(("a" "b" "c") ("b" "c") ("c")))
(check-expect (fixes suffixes '("b" "c" "d"))     '(("b" "c" "d")
                                                    ("c" "d")
                                                    ("d")))

(define prefixes (lambda (lst) (reverse (rest (reverse lst)))))
(define suffixes (lambda (lst) (rest lst)))

(define (fixes f lo1s)
  (cond [(empty? lo1s) '()]
        [else
         (cons lo1s
               (fixes f (f lo1s)))]))



;; 16.8 - Projects: Abstraction

;; Exercise 275


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
(define DICTIONARY-EMPTY   '())
(define DICTIONARY-AS-LIST '("alfa" "eco" "bravo" "erlang" "charlie" "zulu"))

; A LoL is one of:
; '()
; (cons Letter LoL)
; interpretation the list of Letters is a collection of Letter
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; A Letter-Count is one of:
; '()
; '(list Letter Number)
; interpretation the Letter-Count is a piece of data that combines letter and count

; A LoD is one of:
; - '()
; - (cons String LoD)
; interpretation the list of Dictionary is a collection of Dictionary


;; =================
;; Functions:

; Dictionary -> Letter-Count
; produces the Letter-Count for the letter that is occurs most often
; as the first one in the given Dictionary
(check-expect (most-frequent DICTIONARY-EMPTY)   '())
(check-expect (most-frequent DICTIONARY-AS-LIST) '("e" 2))

(define (most-frequent d)
  (local (;; LoD -> Letter-Count
          ;; counts the number of words in the list
          (define (count-by-letter w)
            (list (string-ith (first w) 0) (length w)))
          ;; Letter-Count Letter-Count -> Letter-Count
          ;; picks the pair with the maximum count
          (define (most it last)
            (if (or (empty? last)
                    (> (second it) (second last)))
                it
                last))
          ;; Trampoline
          (define (most-frequent d)
            (foldr most '() (map count-by-letter (words-by-first-letter d)))))
    (most-frequent d)))

; Dictionary -> LoD
; produces a list of Dictionarys, one per Letter
(check-expect (words-by-first-letter DICTIONARY-EMPTY)   '())
(check-expect (words-by-first-letter DICTIONARY-AS-LIST) '(("alfa")
                                                           ("bravo")
                                                           ("charlie")
                                                           ("eco" "erlang")
                                                           ("zulu")))

(define (words-by-first-letter d)
  (local (;; Letter LoD -> LoD
          ;; merges all words by letter
          (define (merge l lst)
            (local ((define (start-with? s)
                      (string=? l (string-ith s 0)))
                    (define words (filter start-with? d)))
              (if (empty? words) lst (cons words lst))))
          ;; Trampoline
          (define (words-by-first-letter d)
            (cond [(empty? d) '()]
                  (else
                   (foldr merge '() LETTERS)))))
    (words-by-first-letter d)))

;; Exercise 276


;; =================
;; Constants:

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")

; LTracks
; (define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))

; LLists
; (define list-tracks (read-itunes-as-lists ITUNES-LOCATION))


;; =================
;; Data definitions:

; Date
(define DATE1 (create-date 1 2 3 4 5 6))
(define DATE2 (create-date 1 2 3 4 5 5))
(define DATE3 (create-date 6 5 4 3 2 1))
(define DATE4 (create-date 1 2 3 "four" 5 6))

; Track
(define TRACK1 (create-track "title A" "two" "three" 4 5 DATE1 7 DATE1))
(define TRACK2 (create-track "title B" "two" "three" 4 5 DATE3 7 DATE3))
(define TRACK3 (create-track "title C" "two" "new-three" 4 5 DATE3 7 DATE3))
(define TRACK4 (create-track "title D" "two" "new-three" 4 5 DATE3 7 DATE3))
(define TRACK5 (create-track "title A" "two" "three" 4 5 "a date" 7 "another date"))

; LTrack
(define LTRACK1 '())
(define LTRACK2 (list TRACK1))
(define LTRACK3 (list TRACK1 TRACK2))
(define LTRACK4 (list TRACK1 TRACK2 TRACK1))
(define LTRACK5 (list TRACK4 TRACK2 TRACK3 TRACK1))

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; interpretation a list of String values

; A LoLT is one of:
; - '()
; - (cons LTracks LoLT)
; interpretation a collection of LTracks

; Association
(define A1 (list "Assoc A" "123ABC"))
(define A2 (list "Assoc B" 5))
(define A3 (list "Assoc C" 5.5))
(define A4 (list "Assoc D" (create-date 1 2 3 4 5 6)))
(define A5 (list "Assoc E" true))

; LAssoc
(define LASSOC1 '())
(define LASSOC2 (list A1 A2 A3 A4 A5))
(define LASSOC3 (list A5 A4 A3 A2 A1))

; LLists
(define LLIST1 '())
(define LLIST2 (list LASSOC2 LASSOC3))
(define LLIST3 (list LASSOC2 LASSOC3 LASSOC1))


;; =================
;; Functions:

; String String Date LTracks -> LTracks
; extracts from the latter the list of tracks that belong
; to the given album and have been played after the given date
(check-expect (select-album-date "title A"  "three" DATE1 LTRACK1) '())
(check-expect (select-album-date "title A"  "three" DATE3 LTRACK3) (list TRACK1))
(check-expect (select-album-date "title A"  "three" DATE1 LTRACK3) '())
(check-expect (select-album-date "title A"  "three" DATE3 LTRACK4) (list TRACK1 TRACK1))
(check-expect (select-album-date "title A"  "three" DATE1 LTRACK4) '())
(check-expect (select-album-date "title AB" "three" DATE3 LTRACK3) '())

(define (select-album-date t a d lt)
  (local (;; Date Date -> Boolean
          ;; compares if d1 is greater than d2
          (define (date>? d1 d2)
            (or (> (date-year   d1) (date-year   d2))
                (> (date-month  d1) (date-month  d2))
                (> (date-day    d1) (date-day    d2))
                (> (date-hour   d1) (date-hour   d2))
                (> (date-minute d1) (date-minute d2))
                (> (date-second d1) (date-second d2))))
          ;; LTracks -> Boolean
          ;; predicates for track selection
          (define (predicate? lt)
            (and (string=? (track-name   lt) t)
                 (string=? (track-album  lt) a)
                 (date>?   (track-played lt) d))))
    (filter predicate? lt)))

; LTracks -> LoLT
; produce a list of LTracks, one per album
(check-expect (select-albums LTRACK1) '())
(check-expect (select-albums LTRACK2) (list (list TRACK1)))
(check-expect (select-albums LTRACK3) (list (list TRACK1 TRACK2)))
(check-expect (select-albums LTRACK4) (list (list TRACK1 TRACK2)))
(check-expect (select-albums LTRACK5) (list (list TRACK4 TRACK3)
                                            (list TRACK2 TRACK1)))

(define (select-albums lt)
  (local ((define (duplicates t lst)
            (if (member? t lst) lst (cons t lst)))
          (define lts (reverse (foldl duplicates '() lt)))
          (define (member-t? t lts)
            (local ((define (member-t? lt)
                      (member? t lt)))
              (ormap member-t? lts)))
          (define (separate t result)
            (if (member-t? t result)
                result
                (cons (local ((define (predicate-ta? pt)
                                (string=? (track-album t) (track-album pt))))
                        (filter predicate-ta? lts)) result))))
    (foldr separate '() lts)))

;; Exercise 277


;; =================
;; Constants:

(define UFO (overlay (rectangle 70 10 "solid" "green")
                     (circle 20 "solid" "green")))
(define UFO-X (/ (image-width  UFO) 2))
(define UFO-Y (/ (image-height UFO) 2))
(define TANK-HEIGHT 20)
(define TANK     (rectangle 50 TANK-HEIGHT "solid" "blue"))
(define MISSILE  (triangle 10 "solid" "red"))
(define WIDTH-UFO  400)
(define HEIGHT-UFO 300)
(define CLOSE (/ HEIGHT-UFO 3))
(define BACKGROUND (empty-scene WIDTH-UFO HEIGHT-UFO))


;; =================
;; Data definitions:

; An UFO is a Posn:
;   (make-posn Natural Natural)
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)
(define U1 (make-posn 10 20))

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number)
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT-UFO) and the tank's speed: dx pixels/tick
(define T-0L (make-tank 0 -1))               ; Tank 0 Left
(define T-0R (make-tank 0 1))                ; Tank 0 Right
(define T-ML (make-tank (/ WIDTH-UFO 2) -1)) ; Tank Middle Left
(define T-MR (make-tank (/ WIDTH-UFO 2)  1)) ; Tank Middle Right
(define T-WL (make-tank WIDTH-UFO -1))       ; Tank Width Left
(define T-WR (make-tank WIDTH-UFO  1))       ; Tank Width Right

; A Missile is a Posn:
;   (make-posn Natural Natural)
; interpretation (make-posn x y) is the Missile's location
; (using the top-down, left-to-right convention)
(define M1 (make-posn 10 50))
(define M2 (make-posn 50 20))

; A [List-of Missile] is one of:
; '()
; (cons Missile [List-of Missile])
; interpretation represents the missiles launched
(define LOM1 '())
(define LOM2 (list M1 M2))

(define-struct sigs [ufo tank missiles])
; A SIGS is a structure:
;   (make-sigs UFO Tank [List-of Missile])
; interpretation represents the complete state of a space invader game
(define SIGS1 (make-sigs U1 T-MR LOM1))
(define SIGS2 (make-sigs U1 T-MR LOM2))
(define SIGS3 (make-sigs (make-posn 10 20)
                         (make-tank 28 -3)
                         (list (make-posn 32 (- HEIGHT-UFO TANK-HEIGHT 10)))))
(define SIGS4 (make-sigs (make-posn 20 100)
                         (make-tank 100 3)
                         (list (make-posn 22 120))))
(define SIGS5 (make-sigs (make-posn 10 (- HEIGHT-UFO CLOSE))
                         (make-tank 28 -3)
                         '()))


;; =================
;; Functions:

; SIGS -> World
; starts a world with (main SIGS1)
(define (main s)
  (big-bang s
            (on-tick   si-move)
            (on-draw   si-render)
            (on-key    si-control)
            (stop-when si-game-over?)))

; SIGS -> SIGS
; updates the position of objects
(define (si-move s)
  (local (;; UFO Number -> UFO
          ;; updates the position of UFO
          (define (update-ufo u)
            (make-posn (random (+ (posn-x u) (image-width UFO)))
                       (add1 (posn-y u))))
          ;; Tank -> Tank
          ;; updates the position of Tank
          (define (update-tank t)
            (local ((define sum-loc-vel (+ (tank-loc t) (tank-vel t))))
              (make-tank (cond [(<   sum-loc-vel 0) 0]
                               [(>   sum-loc-vel WIDTH-UFO) WIDTH-UFO]
                               [else sum-loc-vel])
                         (tank-vel t))))
          ;; Missile -> Missile
          ;; updates the position of missile
          (define (update-missile m)
            (make-posn (posn-x m) (sub1 (posn-y m)))))
    (make-sigs (update-ufo         (sigs-ufo s))
               (update-tank        (sigs-tank s))
               (map update-missile (sigs-missiles s)))))

; SIGS -> Image
; renders the given game state on top of BACKGROUND
(check-expect (si-render SIGS1)
              (place-image UFO (posn-x (sigs-ufo SIGS1)) (posn-y (sigs-ufo SIGS1))
                           (place-image TANK (tank-loc (sigs-tank SIGS1)) HEIGHT-UFO BACKGROUND)))
(check-expect (si-render SIGS2)
              (place-image UFO (posn-x (sigs-ufo SIGS2)) (posn-y (sigs-ufo SIGS2))
                           (place-image TANK (tank-loc (sigs-tank SIGS2)) HEIGHT-UFO
                                        (place-image MISSILE (posn-x (first (sigs-missiles SIGS2))) (posn-y (first (sigs-missiles SIGS2)))
                                                     (place-image MISSILE (posn-x (second (sigs-missiles SIGS2))) (posn-y (second (sigs-missiles SIGS2))) BACKGROUND)))))

(define (si-render s)
  (local (;; UFO Image -> Image
          ;; adds u to the given image im
          (define (ufo-render u im)
            (place-image UFO (posn-x u) (posn-y u) im))
          ; Tank Image -> Image
          ; adds t to the given image im
          (define (tank-render t im)
            (place-image TANK (tank-loc t) HEIGHT-UFO im))
          ; Missile Image -> Image
          ; adds m to the given image im
          (define (missile-render m im)
            (place-image MISSILE (posn-x m) (posn-y m) im)))
    (ufo-render (sigs-ufo s)
                (tank-render (sigs-tank s)
                             (foldr missile-render BACKGROUND (sigs-missiles s))))))

; SIGS KeyEvent -> SIGS
; handles the main events:
; - pressing the left arrow ensures that the tank moves left;
; - pressing the right arrow ensures that the tank moves right; and
; - pressing the space bar fires a new missile.
(check-expect (si-control SIGS1 "up") SIGS1)
(check-expect (si-control SIGS1 "left")
              (make-sigs (sigs-ufo SIGS1)
                         (make-tank (tank-loc (sigs-tank SIGS1))
                                    (* -1 (tank-vel (sigs-tank SIGS1))))
                         (sigs-missiles SIGS1)))
(check-expect (si-control SIGS1 "right")
              (make-sigs (sigs-ufo SIGS1)
                         (make-tank (tank-loc (sigs-tank SIGS1))
                                    (tank-vel (sigs-tank SIGS1)))
                         (sigs-missiles SIGS1)))
(check-expect (si-control SIGS1 " ")
              (make-sigs (sigs-ufo SIGS1)
                         (sigs-tank SIGS1)
                         (list (make-posn (tank-loc (sigs-tank SIGS1))
                                          (- HEIGHT-UFO TANK-HEIGHT)))))

(define (si-control s ke)
  (local ((define tank (sigs-tank s))
          (define tank-location (tank-loc tank))
          (define tank-velocity (tank-vel tank))
          ;; Number -> Tank
          ;; changes the speed direction of tank
          (define (tank-direction d)
            (make-tank tank-location (* d (abs tank-velocity))))
          ;; [List-of Missile] -> [List-of Missile]
          ;; fires a missile at the coordinate: x and (- HEIGHT-UFO TANK-HEIGHT)
          (define (missiles-fire x lom)
            (append lom (cons (make-posn x (- HEIGHT-UFO TANK-HEIGHT)) '())))
          ;; Trampoline
          (define (si-control s ke)
            (make-sigs (sigs-ufo s)
                       (cond [(key=? ke "left")  (tank-direction -1)]
                             [(key=? ke "right") (tank-direction  1)]
                             [else
                              (sigs-tank s)])
                       (cond [(key=? ke " ")
                              (missiles-fire tank-location (sigs-missiles s))]
                             [else
                              (sigs-missiles s)]))))
    (si-control s ke)))

; SIGS -> Boolean
; returns true when the game stop;
; the game stops if the UFO lands or if the missile hits the UFO
(check-expect (si-game-over? SIGS1) #false)
(check-expect (si-game-over? SIGS2) #false)
(check-expect (si-game-over? SIGS3) #false)
(check-expect (si-game-over? SIGS4) #true)
(check-expect (si-game-over? SIGS5) #true)

(define (si-game-over? s)
  (local (;; [List-of Missile] UFO -> Boolean
          ;; checks if any missiles hit the UFO
          (define (hit-missile? lom u)
            (cond [(empty? lom) #false]
                  [(and (<= (- (posn-x u) UFO-X)
                            (posn-x (first lom))
                            (+ (posn-x u) UFO-X))
                        (<= (- (posn-y u) UFO-Y)
                            (posn-y (first lom))
                            (+ (posn-y u) UFO-Y))) #true]
                  [else
                   (hit-missile? (rest lom) u)])))
    (cond [(>= (posn-y (sigs-ufo s))
               (- HEIGHT-UFO CLOSE)) #true]
          [else
           (hit-missile? (sigs-missiles s) (sigs-ufo s))])))

;; Exercise 278


;; =================
;; Constants:

(define SIZE 10)
(define SEGMENT (circle (/ SIZE 2) "solid" "red"))
(define FOOD    (circle (/ SIZE 2) "solid" "green"))
(define WIDTH-WORM  600)
(define HEIGHT-WORM 400)
(define BACKGROUND-WORM (empty-scene WIDTH-WORM HEIGHT-WORM))


;; =================
;; Data definitions:

; A Direction is one of:
; - "left"
; - "up"
; - "right"
; - "down"
; interpretation these strings represent the directions on the screen

(define-struct tail (left top))
; A Tail is a structure:
;   (make-tail Number Number)
; interpretation (make-tail l t) represents a position left l and top t

; A Tails is one of:
; '()
; '(cons Tail Tails)
; interpretation represents the tail of the snake

(define-struct snake (left top direction tails food))
; A Snake is a structure:
;   (make-snake Number Number Direction Tails Posn)
; interpretation (make-snake l t d t f) represents a position left l and top t;
; the direction of movement d, the tails t and the food f position
(define S0 (make-snake (/ WIDTH-WORM 2) (/ HEIGHT-WORM 2) "down" '() (make-posn 100 10)))
(define S1 (make-snake 10 10 "left"  '() null))
(define S2 (make-snake 10 10 "up"    '() null))
(define S3 (make-snake 10 10 "right" '() null))
(define S4 (make-snake 10 10 "down"  '() null))
(define S5 (make-snake 10 10 "down"   (list (make-tail 0  0)
                                            (make-tail 10 0))
                       (make-posn 10 50)))
(define S6 (make-snake 10 10 "up" (list (make-tail 10 10)
                                        (make-tail 20 10)
                                        (make-tail 20 20)
                                        (make-tail 10 20))
                       null))
(define S7 (make-snake 10 10 "down" (list (make-tail 0  0)
                                          (make-tail 10 0))
                       (make-posn 10 20)))
(define S8 (make-snake 10 10 "down" '() (make-posn 10 20)))
(define SM S0)


;; =================
;; Functions:

; Snake -> Snake
; starts a world with (worm-main SM)
(define (worm-main s)
  (big-bang s
            (on-tick tock 1)
            (to-draw render)
            (on-key  event)
            (stop-when game-over? game-over)))

; Snake -> Snake
; updates the position of the snake with the current direction
(check-expect (tock S5)
              (make-snake (future-left S5)
                          (future-top S5)
                          (snake-direction S5)
                          (list (make-tail 10 0)
                                (make-tail 10 10))
                          (snake-food S5)))
(check-random (tock S7)
              (make-snake (future-left S7)
                          (future-top S7)
                          (snake-direction S7)
                          (list (make-tail 0  0)
                                (make-tail 10 0)
                                (make-tail 10 10))
                          (food-create (make-posn (future-left S7) (future-top S7)))))
(check-random (tock S8)
              (make-snake (future-left S8)
                          (future-top S8)
                          (snake-direction S8)
                          (list (make-tail 10 10))
                          (food-create (make-posn (future-left S8) (future-top S8)))))
(check-expect (tock S0)
              (make-snake (future-left S0)
                          (future-top S0)
                          (snake-direction S0)
                          '()
                          (snake-food S0)))

(define (tock s)
  (local (; Number Number Posn -> Boolean
          ; checks if the distance from the left and top position is near the food position in SIZE
          (define (eat? l t p)
            (<= (sqrt (+ (sqr (- (posn-x p) l))
                         (sqr (- (posn-y p) t))))
                SIZE))
          (define s-tails (snake-tails s))
          (define new-tails (list (make-tail (snake-left s)
                                             (snake-top  s))))
          (define eat (eat? (future-left s)
                            (future-top  s)
                            (snake-food  s))))
    (make-snake
     (future-left s)
     (future-top  s)
     (snake-direction s)
     (cond [eat (append s-tails new-tails)]
           [(empty? s-tails) '()]
           [else
            (append (rest s-tails) new-tails)])
     (cond [eat (food-create (make-posn (future-left s)
                                        (future-top s)))]
           [else
            (snake-food s)]))))

; Snake -> Number
; calculates the future value for the left position
(check-expect (future-left S1) (- (snake-left S1) SIZE))
(check-expect (future-left S2) (snake-left S2))
(check-expect (future-left S3) (+ (snake-left S3) SIZE))
(check-expect (future-left S4) (snake-left S4))

(define (future-left s)
  (future s "left" "right"))

; Snake -> Number
; calculates the future value for the top position
(check-expect (future-top S1) (snake-top S1))
(check-expect (future-top S2) (- (snake-top S2) SIZE))
(check-expect (future-top S3) (snake-top S3))
(check-expect (future-top S4) (+ (snake-top S4) SIZE))

(define (future-top s)
  (future s "up" "down"))

; Snake String String -> Number
; calculates the future value for the a and b position
(define (future s a b)
  (local ((define dir (snake-direction s))
          (define top (snake-top s)))
    (cond [(string=? dir a) (- top SIZE)]
          [(string=? dir b) (+ top SIZE)]
          [else top])))

; Snake -> Image
; renders the snake on the BACKGROUND-WORM
(define (render s)
  (local ((define half-size (/ SIZE 2))
          ; Tails -> Image
          ; renders the tails on the BACKGROUND-WORM
          (define (render-tails t)
            (cond [(empty? t) BACKGROUND-WORM]
                  [else
                   (place-image SEGMENT
                                (+ (tail-left (first t)) half-size)
                                (+ (tail-top  (first t)) half-size)
                                (render-tails (rest t)))])))
    (place-image FOOD
                 (+ (posn-x (snake-food s)) half-size)
                 (+ (posn-y (snake-food s)) half-size)
                 (place-image SEGMENT
                              (+ (snake-left s) half-size)
                              (+ (snake-top s)  half-size)
                              (render-tails (snake-tails s))))))

; Snake KeyEvent -> Snake
; updates the direction of snake with Direction
(check-expect (event S1 " ")     S1)
(check-expect (event S1 "left")  S1)
(check-expect (event S1 "up")    S2)
(check-expect (event S1 "right") S3)
(check-expect (event S1 "down")  S4)
(check-expect (event S7 "up")    S7)

(define (event s ke)
  (make-snake (snake-left s)
              (snake-top s)
              (cond [(and (not (empty? (snake-tails s)))
                          (or (and (key=? ke "left")
                                   (string=? (snake-direction s) "right"))
                              (and (key=? ke "right")
                                   (string=? (snake-direction s) "left"))
                              (and (key=? ke "up")
                                   (string=? (snake-direction s) "down"))
                              (and (key=? ke "down")
                                   (string=? (snake-direction s) "up"))))
                     (snake-direction s)]
                    [(or (key=? ke "left")
                         (key=? ke "up")
                         (key=? ke "right")
                         (key=? ke "down")) ke]
                    [else (snake-direction s)])
              (snake-tails s)
              (snake-food  s)))

; Snake -> Boolean
; stops if the snake has run into itself
(check-expect (hit-itself? S1) #false)
(check-expect (hit-itself? S5) #false)
(check-expect (hit-itself? S6) #false)
(check-expect (hit-itself? S6) #false)
(check-expect (hit-itself? (make-snake 0 0 "down" (list (make-tail 0  0)
                                                        (make-tail 10 0)
                                                        (make-tail 10 10)
                                                        (make-tail 0  10)
                                                        (make-tail 0  0))
                                       null))
              #true)

(define (hit-itself? s)
  (local ((define tails (snake-tails s)))
    (cond [(empty? tails) #false]
          [else
           (member? (first tails)
                    (rest  tails))])))

; Snake -> Boolean
; stops if the snake has run into the walls of the world or itself
(check-expect (game-over? (make-snake 0  10 "left"  '() null)) #true)
(check-expect (game-over? (make-snake 0  10 "up"    '() null)) #false)
(check-expect (game-over? (make-snake 0  10 "right" '() null)) #false)
(check-expect (game-over? (make-snake 0  10 "down"  '() null)) #false)
(check-expect (game-over? (make-snake 10 0 "up"     '() null)) #true)
(check-expect (game-over? (make-snake 10 0 "right"  '() null)) #false)
(check-expect (game-over? (make-snake 10 0 "down"   '() null)) #false)
(check-expect (game-over? (make-snake 10 0 "left"   '() null)) #false)
(check-expect (game-over? (make-snake (- WIDTH-WORM SIZE) 10 "right"  '() null)) #true)
(check-expect (game-over? (make-snake (- WIDTH-WORM SIZE) 10 "down"   '() null)) #false)
(check-expect (game-over? (make-snake (- WIDTH-WORM SIZE) 10 "left"   '() null)) #false)
(check-expect (game-over? (make-snake (- WIDTH-WORM SIZE) 10 "up"     '() null)) #false)
(check-expect (game-over? (make-snake 10 (- HEIGHT-WORM SIZE) "down"  '() null)) #true)
(check-expect (game-over? (make-snake 10 (- HEIGHT-WORM SIZE) "left"  '() null)) #false)
(check-expect (game-over? (make-snake 10 (- HEIGHT-WORM SIZE) "up"    '() null)) #false)
(check-expect (game-over? (make-snake 10 (- HEIGHT-WORM SIZE) "right" '() null)) #false)
(check-expect (game-over? S1) #false)
(check-expect (game-over? S6) #true)

(define (game-over? s)
  (local (; Snake -> Boolean
          ; stops if the snake has run into the walls of the world
          (define (hit-border? s)
            (local ((define dir (snake-direction s)))
              (or (and (zero? (snake-left s))
                       (string=? dir "left"))
                  (and (zero? (snake-top s))
                       (string=? dir "up"))
                  (and (= (snake-left s) (- WIDTH-WORM SIZE))
                       (string=? dir "right"))
                  (and (= (snake-top s)  (- HEIGHT-WORM SIZE))
                       (string=? dir "down"))
                  (member? (make-tail (snake-left s) (snake-top s)) (snake-tails s))))))
    (or (hit-border? s)
        (hit-itself? s))))

; Snake -> Image
; renders the game over message on the last render
(define (game-over s)
  (place-image
   (text (if (hit-itself? s)
             "worm hit itself"
             "worm hit the wall")
         12 "black")
   50 (- HEIGHT-WORM 10)
   (render s)))

; Posn -> Posn
; creates a food randomly between WIDTH-WORM and HEIGHT-WORM
(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)

(define (food-create p)
  (food-check-create p (make-posn (random WIDTH-WORM) (random HEIGHT-WORM))))

; Posn Posn -> Posn
; generative recursion
; checks if the food exists, otherwise it creates a new food
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))
