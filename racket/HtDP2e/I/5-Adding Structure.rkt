;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |5-Adding Structure|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 5-Adding Structure.rkt
;; I - Fixed-Size Data
;; 5 - Adding Structure

(require 2htdp/image)
(require 2htdp/universe)


;; 5.1 - From Positions to posn Structures



;; 5.2 - Computing with posns



;; 5.3 - Programming with posn


;; =================
;; Functions:

; computes the distance of ap to the origin
(check-expect (distance-to-0 (make-posn 0 5))  5)
(check-expect (distance-to-0 (make-posn 7 0))  7)
(check-expect (distance-to-0 (make-posn 3 4))  5)
(check-expect (distance-to-0 (make-posn 8 6))  10)
(check-expect (distance-to-0 (make-posn 5 12)) 13)

(define (distance-to-0 ap)
  (sqrt
   (+ (sqr (posn-x ap))
      (sqr (posn-y ap)))))

;; Exercise 63

(distance-to-0 (make-posn 3 4))
(sqrt
 (+ (sqr (posn-x (make-posn 3 4)))
    (sqr (posn-y (make-posn 3 4)))))
(sqrt
 (+ (sqr 3)
    (sqr (posn-y (make-posn 3 4)))))
(sqrt
 (+ 9
    (sqr (posn-y (make-posn 3 4)))))
(sqrt
 (+ 9
    (sqr 4)))
(sqrt
 (+ 9
    16))
(sqrt 25)
5

(distance-to-0 (make-posn 6 (* 2 4)))
(distance-to-0 (make-posn 6 8))
(sqrt
 (+ (sqr (posn-x (make-posn 6 8)))
    (sqr (posn-y (make-posn 6 8)))))
(sqrt
 (+ (sqr 6)
    (sqr (posn-y (make-posn 6 8)))))
(sqrt
 (+ 36
    (sqr (posn-y (make-posn 6 8)))))
(sqrt
 (+ 36
    (sqr 8)))
(sqrt
 (+ 36
    64))
(sqrt 100)
10

(+ (distance-to-0 (make-posn 12 5)) 10)
(+ (sqrt
    (+ (sqr (posn-x (make-posn 12 5)))
       (sqr (posn-y (make-posn 12 5))))) 10)
(+ (sqrt
    (+ (sqr 12)
       (sqr (posn-y (make-posn 12 5))))) 10)
(+ (sqrt
    (+ 144
       (sqr (posn-y (make-posn 12 5))))) 10)
(+ (sqrt
    (+ 144
       (sqr 5))) 10)
(+ (sqrt
    (+ 144
       25)) 10)
(+ (sqrt 169) 10)
(+ 13 10)
23

;; Exercise 64


;; =================
;; Functions:

;; measures the Manhattan distance of the given posn to the origin
(check-expect (manhattan-distance (make-posn 3 4)) 5)

(define (manhattan-distance p)
  (distance-to-0 p))



;; 5.4 - Defining Structure Types

;; Exercise 65

(define-struct movie [title producer year])
(define-struct person [name hair eyes phone])
(define-struct pet [name number])
(define-struct CD [artist title price])
(define-struct sweater [material size producer])

;; Exercise 66

; constructors
(define M  (make-movie "Title" "Producer" 2017))
(define P1 (make-person "Name" "Hair" "Eyes" "Phone"))
(define P2 (make-pet "Name" 123))
(define C  (make-CD "Artist" "Title" 1.99))
(define S  (make-sweater "Material" 4 "Producer"))

; selectors
(movie-title    M)
(movie-producer M)
(movie-year     M)

(person-name  P1)
(person-hair  P1)
(person-eyes  P1)
(person-phone P1)

(pet-name   P2)
(pet-number P2)

(CD-artist C)
(CD-title  C)
(CD-price  C)

(sweater-material S)
(sweater-size     S)
(sweater-producer S)

; predicates
(movie?   M)
(person?  P1)
(pet?     P2)
(CD?      C)
(sweater? S)

;; Exercise 67

(define SPEED 3)
(define-struct balld [location direction])
(make-balld 10 "up")

(make-balld 5 "left")
(make-balld 2 "right")
(make-balld 8 "down")


(define-struct ball [location velocity])
; A Ball-1d is a structure:
;   (make-ball Number Number)
; interpretation 1 distance to top and velocity
; interpretation 2 distance to left and velocity

(define-struct vel [deltax deltay])
; A Vel is a structure:
;   (make-vel Number Number)
; interpretation (make-vel dx dy) means a velocity of
; dx pixels [per tick] along the horizontal and
; dy pixels [per tick] along the vertical direction

(define ball1
  (make-ball (make-posn 30 40) (make-vel -10 5)))

;; Exercise 68

(define-struct ballf [x y deltax deltay])

(define ball2 (make-ballf 30 40 -10 5))



;; 5.5 - Computing with Structures

;; Exercise 69

; (define-struct movie [title producer year])
; (make-movie "Title" "Producer" 2017)
;                  +-----+
;                  |movie|
;+-------+---------++----+
;|title  |producer  |year|
;| ----- | -------- | -- |
;|"Title"|"Producer"|2017|
;+-------+----------+----+

; (define-struct person [name hair eyes phone])
; (make-person "Name" "Hair" "Eyes" "Phone")
;                      +------+
;                      |person|
;+------+------+------++------+
;|name  |hair  |eyes  |phone  |
;| ---- | ---- | ---- | ----- |
;|"Name"|"Hair"|"Eyes"|"Phone"|
;+------+------+------+-------+

; (define-struct pet [name number])
; (make-pet "Name" 123)
;          +---+
;          |pet|
;+------+--+---+
;|name  |number|
;| ---- | ---- |
;|"Name"|123   |
;+------+------+

; (define-struct CD [artist title price])
; (make-CD "Artist" "Title" 1.99)
;                    +--+
;                    |CD|
;+--------+-------+--+--+
;|artist  |title  |price|
;| ------ | ----- | --- |
;|"Artist"|"Title"|1.99 |
;+--------+-------+-----+

; (define-struct sweater [material size producer])
; (make-sweater "Material" 4 "Producer")
;                   +-------+
;                   |sweater|
;+----------+----+--+-------+
;|material  |size|producer  |
;| -------- | -- | -------- |
;|"Material"|4   |"Producer"|
;+----------+----+----------+

;; Exercise 70

(define-struct centry [name home office cell])
(define-struct phone [area number])

(phone-area
 (centry-office
  (make-centry
   "Shriram Fisler"
   (make-phone 207 "363-2421")
   (make-phone 101 "776-1099")
   (make-phone 208 "112-9981"))))
(phone-area
 (make-phone 101 "776-1099"))
101

;; Exercise 71

; distances in terms of pixels:
(define HEIGHT 200)
(define MIDDLE (quotient HEIGHT 2))
(define WIDTH  400)
(define CENTER (quotient WIDTH 2))

(define-struct game [left-player right-player ball])

(define game0
  (make-game MIDDLE MIDDLE (make-posn CENTER CENTER)))


(game-ball game0)
(game-ball (make-game 100 100 (make-posn 200 200)))
(make-posn 200 200)

(posn? (game-ball game0))
(posn? (game-ball (make-game 100 100 (make-posn 200 200))))
(posn? (make-posn 200 200))
#true

(game-left-player game0)
(game-left-player (make-game 100 100 (make-posn 200 200)))
100



;; 5.6 - Programming with Structures

;; Exercise 72

; (define-struct phone [area number])
; A Phone is a structure:
;   (make-phone Number[1..999] Number[1..9999])
; interpretation area means first three digits of phone, between 1 and 999
;                number means last four number of phone, between 1 and 9999

(define-struct phone# [area switch num])
; A Phone# is a structure:
;   (make-phone# Number[1..999] Number[1..999] Number[1..9999])
; interpretation area means first three digits of phone, between 1 and 999
;                switch means next three code of phone,  between 1 and 999
;                number means last four number of phone, between 1 and 9999

;; Exercise 73
;; Exercise 74


;; =================
;; Constants:

(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))


;; =================
;; Data definitions:

; A Posn represents the state of the world.


;; =================
;; Functions:

; Posn -> World
; starts the world with (main (make-posn 0 0))
(define (main p0)
  (big-bang p0
            [on-tick  x+]
            [on-mouse reset-dot]
            [to-draw  scene+dot]))

; Posn -> Posn
; increases the x-coordinate of p by 3
(check-expect (x+ (make-posn 10 0)) (make-posn 13 0))

(define (x+ p)
  (posn-up-x p (+ (posn-x p) 3)))

; Posn Number -> Posn
; produces a posn like p with n in the x field
(check-expect (posn-up-x (make-posn 2 3) 4) (make-posn 4 3))

(define (posn-up-x p n)
  (make-posn n (posn-y p)))

; Posn Number Number MouseEvt -> Posn
; for mouse clicks, (make-posn x y); otherwise p
(check-expect (reset-dot (make-posn 10 20) 29 31 "button-down") (make-posn 29 31))
(check-expect (reset-dot (make-posn 10 20) 29 31 "button-up")   (make-posn 10 20))

(define (reset-dot p x y me)
  (cond
    [(mouse=? me "button-down") (make-posn x y)]
    [else p]))

; Posn -> Image
; adds a red spot to MTS at p
(check-expect (scene+dot (make-posn 10 20))
              (place-image DOT 10 20 MTS))
(check-expect (scene+dot (make-posn 88 73))
              (place-image DOT 88 73 MTS))

(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

;; Exercise 75


;; =================
;; Data definitions:

(define-struct ufo [loc vel])
; A UFO is a structure:
;   (make-ufo Posn Vel)
; interpretation (make-ufo p v) is at location
; p moving at velocity v.
(define v1 (make-vel 8  -3))
(define v2 (make-vel -5 -3))

(define p1 (make-posn 22 80))
(define p2 (make-posn 30 77))

(define u1 (make-ufo p1 v1))
(define u2 (make-ufo p1 v2))
(define u3 (make-ufo p2 v1))
(define u4 (make-ufo p2 v2))


;; =================
;; Functions:

; UFO -> UFO
; determines where u moves in one clock tick;
; leaves the velocity as is
(check-expect (ufo-move-1 u1) u3)
(check-expect (ufo-move-1 u2)
              (make-ufo (make-posn 17 77) v2))

(define (ufo-move-1 u)
  (make-ufo (posn+ (ufo-loc u) (ufo-vel u))
            (ufo-vel u)))

; Posn Vel -> Posn
; adds v to p
(check-expect (posn+ p1 v1) p2)
(check-expect (posn+ p1 v2) (make-posn 17 77))

(define (posn+ p v)
  (make-posn (+ (posn-x p) (vel-deltax v))
             (+ (posn-y p) (vel-deltay v))))



;; 5.7 - The Universe of Data

;; Exercise 76

; (define-struct movie [title producer year])
; Movie is (make-movie String String Number[0..9999])
; interpretation title is a string
;                producer is a string
;                year is a number, between 0 and 9999

; (define-struct person [name hair eyes phone])
; Person is (make-person String String String String)
; interpretation name is a string
;                hair is a string
;                eyes is a string
;                phone is a string in format ###-####

; (define-struct pet [name number])
; Pet is (make-pet String Number)
; interpretation name is a string
;                number is a identification number

; (define-struct CD [artist title price])
; CD is (make-CD String String Number[0..])
; interpretation artist is a string
;                title is a string
;                price is a positive number

; (define-struct sweater [material size producer])
; Sweater is (make-sweater String Number[0..] String)
; interpretation material is a string
;                size is a positive number
;                producer is a string

;; Exercise 77

(define-struct point-time (hours minutes seconds))
; Point-Time is a structure:
;  (make-point-time Number[0..23] Number[0..59] Number[0..59])
; interpretation hours means a number between 0 and 23
;                minutes means a number between 0 and 59
;                seconds means a number between 0 and 59

;; Exercise 78

(define-struct word-3 (l1 l2 l3))
; Word-3 is a structure:
;  (make-word-3 String String String)
; interpretation l1 is a lower-case letter
;                l2 is a lower-case letter
;                l3 is a lower-case letter
; Each letter is represented by "a" through "z" plus #false

;; Exercise 79

; A Color is one of:
; - "white"
; - "yellow"
; - "orange"
; - "green"
; - "red"
; - "blue"
; - "black"
(define C1 "white")
(define C2 "yellow")
(define C3 "orange")
(define C4 "green")
(define C5 "red")
(define C6 "blue")
(define C7 "black")

; H is a Number between 0 and 100.
; interpretation represents a “happiness value”
(define H1 0)
(define H2 50)
(define H3 100)

(define-struct person-v2 [fstname lstname male?])
; A Person-v2 is a structure:
;   (make-person-v2 String String Boolean)
(define P1V2 (make-person-v2 "Joseph" "WGW" #true))

(define-struct dog [owner name age happiness])
; A Dog is a structure:
;   (make-dog Person-v2 String PositiveInteger H)
; interpretation owner means a Person-v2
;                name means a string
;                age means a positive number
;                happiness means a H
(define D1 (make-dog P1V2 "Totó" 2 H3))

; A Weapon is one of:
; - #false
; - Posn
; interpretation #false means the missile hasn't
; been fired yet; a Posn means it is in flight
(define W1 #false)
(define W2 (make-posn 20 20))



;; 5.8 - Designing with Structures

;; Exercise 80

; (define-struct movie [title director year])

; Movie -> String
; format movie information for a string
(define (format-movie m)
  (... (movie-title m) ... (movie-director m) ... (movie-year m) ...))


; (define-struct person [name hair eyes phone])

; Person -> String
; produces the information of a person
(define (do-person p)
  (... (person-name p) ... (person-hair p) ... (person-eyes p) ... (person-phone p) ...))


; (define-struct pet [name number])

; Pet -> String
; produces the identification of pet
(define (pet-id p)
  (... (pet-name p) ... (pet-number p) ...))


; (define-struct CD [artist title price])

; CD -> String
; produces the information of a CD
(define (do-price c)
  (... (CD-artist c) ... (CD-title c) ... (CD-price c) ...))


; (define-struct sweater [material size color])

; Sweater -> String
; produces the information of a Sweater
(define (do-sweater s)
  (... (sweater-material s) ... (sweater-size s) ... (sweater-color s) ...))

;; Exercise 81

; Point-Time -> Number
; produces the number of seconds that have passed since midnight
(check-expect (time->seconds (make-point-time 12 30 2)) 45002)

(define (time->seconds r)
  (+ (* (point-time-hours   r) 60 60)
     (* (point-time-minutes r) 60)
     (point-time-seconds    r)))

;; Exercise 82

; Word-3 Word-3 -> Word-3
; produces a word that indicates where the given ones agree and disagree
(check-expect (compare-word (make-word-3 "a" "b" "c")
                            (make-word-3 "a" "b" "d"))
              (make-word-3 "a" "b" #false))

(define (compare-word w1 w2)
  (make-word-3 (res (word-3-l1 w1) (word-3-l1 w2))
               (res (word-3-l2 w1) (word-3-l2 w2))
               (res (word-3-l3 w1) (word-3-l3 w2))))

; String String -> String/Boolean
; helper function to compare-word
(check-expect (res "a" "a")    "a")
(check-expect (res "a" "b")    #false)
(check-expect (res "a" #false) #false)
(check-expect (res #false "a") #false)

(define (res w1 w2)
  (cond [(and (string? w1)
              (string? w2)
              (string=? w1 w2)) w1]
        [else #false]))



;; 5.9 - Structure in the World



;; 5.10 - A Graphical Editor

;; Exercise 83

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t


; Editor -> Image
; produces an image with the editor
(check-expect (render (make-editor "Hello " "World"))
              (overlay/align "left" "center"
                             (beside
                              (text "Hello " 16 "black")
                              (rectangle 1 20 "solid" "red")
                              (text "World" 16 "black"))
                             (empty-scene 200 20)))

(define (render e)
  (overlay/align "left" "center"
                 (beside
                  (text (editor-pre e)  16 "black")
                  (rectangle 1 20 "solid" "red")
                  (text (editor-post e) 16 "black"))
                 (empty-scene 200 20)))

;; Exercise 84

; Editor KeyEvent -> Editor
; produces a new editor with base in key event
(check-expect (edit (make-editor "abc" "def") "a")     (make-editor "abca" "def"))
(check-expect (edit (make-editor "abc" "def") "\b")    (make-editor "ab" "def"))
(check-expect (edit (make-editor "" "def")    "\b")    (make-editor "" "def"))
(check-expect (edit (make-editor "abc" "def") "\t")    (make-editor "abc" "def"))
(check-expect (edit (make-editor "abc" "def") "\r")    (make-editor "abc" "def"))
(check-expect (edit (make-editor "abc" "def") "left")  (make-editor "ab" "cdef"))
(check-expect (edit (make-editor "" "abcdef") "left")  (make-editor "" "abcdef"))
(check-expect (edit (make-editor "abc" "def") "right") (make-editor "abcd" "ef"))
(check-expect (edit (make-editor "abcdef" "") "right") (make-editor "abcdef" ""))

(define (edit e ke)
  (cond
    [(key=? ke  "left")
     (make-editor (string-remove-last (editor-pre e))
                  (string-append (string-last (editor-pre e))
                                 (editor-post e)))]
    [(key=? ke "right")
     (make-editor (string-append (editor-pre e)
                                 (string-first (editor-post e)))
                  (string-rest (editor-post e)))]
    [(or (key=? ke "\t") (key=? ke "\r")) e]
    [(= (string-length ke) 1)
     (make-editor
      (cond
        [(key=? ke "\b")          (string-remove-last (editor-pre e))]
        [(= (string-length ke) 1) (string-append (editor-pre e) ke)]
        [else (editor-pre e)])
      (editor-post e))]
    [else e]))

;; String -> String
;; extracts the first character from a non-empty string
(check-expect (string-first "Hello") "H")
(check-expect (string-first "apple") "a")
(check-expect (string-first "")      "")

(define (string-first s)
  (if (= (string-length s) 0)
      ""
      (substring s 0 1)))

;; String -> String
;; produces a string like the given one with the first character removed
(check-expect (string-rest "Hello") "ello")
(check-expect (string-rest "apple") "pple")
(check-expect (string-rest "")      "")

(define (string-rest s)
  (if (= (string-length s) 0)
      ""
      (substring s 1)))

;; String -> String
;; extracts the last character from a non-empty string
(check-expect (string-last "Hello") "o")
(check-expect (string-last "apple") "e")
(check-expect (string-last "")      "")

(define (string-last s)
  (if (= (string-length s) 0)
      ""
      (substring s (- (string-length s) 1))))

;; String -> String
;; produces a string like the given one with the last character removed
(check-expect (string-remove-last "Hello") "Hell")
(check-expect (string-remove-last "apple") "appl")
(check-expect (string-remove-last "")      "")

(define (string-remove-last s)
  (if (= (string-length s) 0)
      ""
      (substring s 0 (- (string-length s) 1))))

;; Exercise 85


;; =================
;; Functions:

; String -> World
; starts a world with (run "abc")
(define (run pre)
  (big-bang (make-editor pre "")
            (to-draw render)
            (on-key  edit)))

;; Exercise 86

; Editor KeyEvent -> Editor
; produces a new editor with base in key event
(check-expect (edit.v2 (make-editor "abc" "def") "a")     (make-editor "abca" "def"))
(check-expect (edit.v2 (make-editor "abc" "def") "\b")    (make-editor "ab" "def"))
(check-expect (edit.v2 (make-editor "" "def")    "\b")    (make-editor "" "def"))
(check-expect (edit.v2 (make-editor "abc" "def") "\t")    (make-editor "abc" "def"))
(check-expect (edit.v2 (make-editor "abc" "def") "\r")    (make-editor "abc" "def"))
(check-expect (edit.v2 (make-editor "abc" "def") "left")  (make-editor "ab" "cdef"))
(check-expect (edit.v2 (make-editor "" "abcdef") "left")  (make-editor "" "abcdef"))
(check-expect (edit.v2 (make-editor "abc" "def") "right") (make-editor "abcd" "ef"))
(check-expect (edit.v2 (make-editor "abcdef" "") "right") (make-editor "abcdef" ""))
(check-expect (edit.v2 (make-editor "abcdefghij" "") "k") (make-editor "abcdefghij" ""))

(define (edit.v2 e ke)
  (cond
    [(key=? ke  "left")
     (make-editor (string-remove-last (editor-pre e))
                  (string-append (string-last (editor-pre e))
                                 (editor-post e)))]
    [(key=? ke "right")
     (make-editor (string-append (editor-pre e)
                                 (string-first (editor-post e)))
                  (string-rest (editor-post e)))]
    [(or (key=? ke "\t") (key=? ke "\r")) e]
    [(= (string-length ke) 1)
     (make-editor
      (cond
        [(key=? ke "\b") (string-remove-last (editor-pre e))]
        [(and (= (string-length ke) 1)
              (< (string-length (editor-pre e)) 10))
         (string-append (editor-pre e) ke)]
        [else (editor-pre e)])
      (editor-post e))]
    [else e]))

;; Exercise 87

;; =================
;; Data definitions:

(define-struct editor-v3 [post index])
; An Editor is a structure:
;   (make-editor-v3 String Number)
; interpretation (make-editor-v3 s i) describes an editor
; whose visible text is s with the cursor displayed in t position


;; =================
;; Functions:

; String -> World
; starts a world with (run.v3 "abc")
(define (run.v3 pre)
  (big-bang (make-editor-v3 pre (string-length pre))
            (to-draw render.v3)
            (on-key  edit.v3)))

; Editor -> Image
; produces an image with the editor
(check-expect (render.v3 (make-editor-v3 "Hello World" 6))
              (overlay/align "left" "center"
                             (beside
                              (text "Hello " 16 "black")
                              (rectangle 1 20 "solid" "red")
                              (text "World" 16 "black"))
                             (empty-scene 200 20)))

(define (render.v3 e)
  (overlay/align "left" "center"
                 (beside
                  (text (substring (editor-v3-post e) 0 (editor-v3-index e)) 16 "black")
                  (rectangle 1 20 "solid" "red")
                  (text (substring (editor-v3-post e)   (editor-v3-index e)) 16 "black"))
                 (empty-scene 200 20)))


; Editor KeyEvent -> Editor
; produces a new editor with base in key event
(check-expect (edit.v3 (make-editor-v3 "abcdef" 3) "a")      (make-editor-v3 "abcadef" 4))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 3) "\b")     (make-editor-v3 "abdef" 2))
(check-expect (edit.v3 (make-editor-v3 "def" 0)    "\b")     (make-editor-v3 "def" 0))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 3) "\t")     (make-editor-v3 "abcdef" 3))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 3) "\r")     (make-editor-v3 "abcdef" 3))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 3) "left")   (make-editor-v3 "abcdef" 2))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 0) "left")   (make-editor-v3 "abcdef" 0))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 3) "right")  (make-editor-v3 "abcdef" 4))
(check-expect (edit.v3 (make-editor-v3 "abcdef" 6) "right")  (make-editor-v3 "abcdef" 6))
(check-expect (edit.v3 (make-editor-v3 "abcdefghij" 10) "k") (make-editor-v3 "abcdefghij" 10))

(define (edit.v3 e ke)
  (cond
    [(key=? ke "left")
     (make-editor-v3
      (editor-v3-post e)
      (if (> (editor-v3-index e) 0)
          (sub1 (editor-v3-index e))
          0))]
    [(key=? ke "right")
     (make-editor-v3
      (editor-v3-post e)
      (if (< (editor-v3-index e) (string-length (editor-v3-post e)))
          (add1 (editor-v3-index e))
          (string-length (editor-v3-post e))))]
    [(or (key=? ke "\t") (key=? ke "\r")) e]
    [(= (string-length ke) 1)
     (make-editor-v3
      (cond
        [(key=? ke "\b") (if (> (editor-v3-index e) 0)
                             (string-delete (editor-v3-post e) (editor-v3-index e))
                             (editor-v3-post e))]
        [(and (= (string-length ke) 1)
              (< (string-length (editor-v3-post e)) 10))
         (string-insert ke (editor-v3-post e) (editor-v3-index e))]
        [else (editor-v3-post e)])
      (cond
        [(key=? ke "\b") (if (> (editor-v3-index e) 0)
                             (sub1 (editor-v3-index e))
                             (editor-v3-index e))]
        [(and (= (string-length ke) 1)
              (< (editor-v3-index e) 10)) (add1 (editor-v3-index e))]
        [else (editor-v3-index e)]))]
    [else e]))

;; String -> String
;; inserts character c into string s in position i
(check-expect (string-insert "_" "helloworld" 5) "hello_world")

(define (string-insert c s i)
  (string-append (substring s 0 i) c (substring s i)))

;; String -> String
;; deletes the character at position i of string str
(check-expect (string-delete "helloworld" 6) "helloorld")
(check-expect (string-delete "abcdef" 3)     "abdef")

(define (string-delete str i)
  (if (= i 0)
      str
      (string-append
       (substring str 0 (sub1 i))
       (substring str i))))



;; 5.11 - More Virtual Pets

;; Exercise 88
;; Exercise 89
;; Exercise 90
;; Exercise 91


;; =================
;; Constants:

(define WIDTH-V4  400)
(define HEIGHT-V4 400)

(define CAT (circle 6 "solid" "brown"))
(define SPEED-V4 3)
(define OFFSET      (/ (image-width CAT) 2))
(define LIMIT-X-POS (+ WIDTH-V4 OFFSET))
(define Y-CAT       (/ HEIGHT-V4 2))

(define GAUGE-WIDTH  (/ WIDTH-V4  3))
(define GAUGE-HEIGHT (/ HEIGHT-V4 40))
(define MIN  0)
(define MAX  100)
(define DEC -0.1)
(define INC-DOWN 1/5)
(define INC-UP   1/3)

(define BACKGROUND (empty-scene WIDTH-V4 HEIGHT-V4))


;; =================
;; Data definitions:

(define-struct vcat (x h d))
; A VCat is a structure:
;   (make-vcat Number Number Number)
; interpretation (make-vcat x h) describes a virtual cat
;   where x means x-coordinate
;   and   h means your happiness
;   and   d means your direction, 0 left and 1 right
(define VC1 (make-vcat 10 100 1))
(define VC2 (make-vcat WIDTH-V4 100 1))
(define VC3 (make-vcat 0 MIN 1))
(define VC4 (make-vcat 0 0.1 1))
(define VC5 (make-vcat 0 0.3 1))
(define VC6 (make-vcat 0 10  1))
(define VC7 (make-vcat 0 (sub1 MAX) 1))
(define VC8 (make-vcat 0  100 0))
(define VC9 (make-vcat 10 100 0))


;; =================
;; Functions:

; VCat -> World
; starts a world with (happy-cat (make-vcat 0 100 0))
(define (happy-cat vcat)
  (big-bang vcat
            [on-tick   tock]
            [to-draw   render.v4]
            [on-key    increase]
            [stop-when sad?]))

; VCat -> VCat
; moves the virtual cat by SPEED pixels and
; decreases the happiness by DEC for every clock tick,
; turn the virtual cat around when it reaches either end of the scene
(check-expect (tock VC1) (make-vcat (+ (vcat-x VC1) SPEED-V4) (+ (vcat-h VC1) DEC) 1))
(check-expect (tock VC2) (make-vcat (- (vcat-x VC2) SPEED-V4) (+ (vcat-h VC2) DEC) 0))
(check-expect (tock VC3) (make-vcat (+ (vcat-x VC3) SPEED-V4) MIN 1))
(check-expect (tock VC4) (make-vcat (+ (vcat-x VC4) SPEED-V4) MIN 1))
(check-expect (tock VC5) (make-vcat (+ (vcat-x VC5) SPEED-V4) (+ (vcat-h VC5) DEC) 1))
(check-expect (tock VC8) (make-vcat (+ (vcat-x VC8) SPEED-V4) (+ (vcat-h VC8) DEC) 1))
(check-expect (tock VC9) (make-vcat (- (vcat-x VC9) SPEED-V4) (+ (vcat-h VC9) DEC) 0))

(define (tock vc)
  (make-vcat
   (cond [(>= (vcat-x vc) WIDTH-V4) (- (vcat-x vc) SPEED-V4)]
         [(<= (vcat-x vc) 0)        (+ (vcat-x vc) SPEED-V4)]
         [else (if (= (vcat-d vc) 1)
                   (+ (vcat-x vc) SPEED-V4)
                   (- (vcat-x vc) SPEED-V4))])
   (if (<= (+ (vcat-h vc) DEC) MIN)
       MIN
       (+ (vcat-h vc) DEC))
   (cond [(>= (vcat-x vc) WIDTH-V4) 0]
         [(<= (vcat-x vc) 0)        1]
         [else (vcat-d vc)])))

; VCat -> Image
; places the virtual cat into the BACKGROUND scene
(define (render.v4 vc)
  (place-image CAT (vcat-x vc) Y-CAT
               (overlay/align "middle" "top"
                              (rectangle (* (vcat-h vc) GAUGE-WIDTH .01)
                                         GAUGE-HEIGHT
                                         "solid" "red")
                              BACKGROUND)))

; VCat KeyEvent -> VCat
; produces a increase in happiness for:
; up is 1/3 and down is 1/5
(check-expect (increase VC6 "up")   (make-vcat (vcat-x VC6) (calculate (vcat-h VC6) INC-UP) (vcat-d VC6)))
(check-expect (increase VC6 "left") VC6)
(check-expect (increase VC6 "down") (make-vcat (vcat-x VC6) (calculate (vcat-h VC6) INC-DOWN) (vcat-d VC6)))
(check-expect (increase VC7 "up")   (make-vcat (vcat-x VC7) MAX (vcat-d VC7)))
(check-expect (increase VC7 "down") (make-vcat (vcat-x VC7) MAX (vcat-d VC7)))
(check-expect (increase VC3 "up")   (make-vcat (vcat-x VC3) 1 (vcat-d VC3)))
(check-expect (increase VC3 "down") (make-vcat (vcat-x VC3) 1 (vcat-d VC3)))

(define (increase vc ke)
  (make-vcat (vcat-x vc)
             (cond [(key=? ke "up")   (calculate (vcat-h vc) INC-UP)]
                   [(key=? ke "down") (calculate (vcat-h vc) INC-DOWN)]
                   [else (vcat-h vc)])
             (vcat-d vc)))

; Number -> Number
; help function for increase x by n, limited by MAX
(define (calculate x n)
  (cond [(= x MIN) 1]
        [(> (+ x (* x n)) MAX) MAX]
        [else (+ x (* x n))]))

; VCat -> Boolean
; stops when the virtual cat is sad
(check-expect (sad? VC3) #true)
(check-expect (sad? VC1) #false)

(define (sad? vc)
  (= (vcat-h vc) MIN))

;; Exercise 92
;; Exercise 93


;; =================
;; Constants:

(define CHAM (circle 6 "solid" "red"))
(define OFFSET-V5 (/ (image-width CHAM) 2))
(define Y-CHAM    (/ HEIGHT-V4 2))

(define BACKGROUND-V5
  (beside (empty-scene (/ WIDTH-V4 3) HEIGHT-V4 "green")
          (empty-scene (/ WIDTH-V4 3) HEIGHT-V4 "white")
          (empty-scene (/ WIDTH-V4 3) HEIGHT-V4 "red")))


;; =================
;; Data definitions:

(define-struct vcham (x h c))
; A VCham is a structure:
;   (make-vcham Number Number String)
; interpretation (make-vcham x h c) describes a virtual cham
;   where x means x-coordinate
;   and   h means your happiness
;   and   c means your color
(define VCH1 (make-vcham 0 100 "red"))
(define VCH2 (make-vcham LIMIT-X-POS 100 "red"))
(define VCH3 (make-vcham 0 MIN "red"))
(define VCH4 (make-vcham 0 0.1 "red"))
(define VCH5 (make-vcham 0 0.3 "red"))
(define VCH6 (make-vcham 0 10  "red"))
(define VCH7 (make-vcham 0 (sub1 MAX) "red"))
(define VCH8 (make-vcham 0 10 "green"))
(define VCH9 (make-vcham 0 10 "blue"))


;; =================
;; Functions:

; VCham -> World
; starts a world with (cham (make-vcham 0 100 "red"))
(define (cham vcham)
  (big-bang vcham
            [on-tick   tock.v5]
            [to-draw   render.v5]
            [on-key    interact]
            [stop-when sad?.v5]))

; VCham -> VCham
; moves the virtual cham by SPEED pixels and
; decreases the happiness by DEC for every clock tick,
; reset when the virtual cham disappears on the right
(check-expect (tock.v5 VCH1) (make-vcham   (+ (vcham-x VCH1) SPEED-V4) (+ (vcham-h VCH1) DEC) "red"))
(check-expect (tock.v5 VCH2) (make-vcham 0 (+ (vcham-h VCH2) DEC) "red"))
(check-expect (tock.v5 VCH3) (make-vcham   (+ (vcham-x VCH3) SPEED-V4) MIN "red"))
(check-expect (tock.v5 VCH4) (make-vcham   (+ (vcham-x VCH4) SPEED-V4) MIN "red"))
(check-expect (tock.v5 VCH5) (make-vcham   (+ (vcham-x VCH5) SPEED-V4) (+ (vcham-h VCH5) DEC) "red"))

(define (tock.v5 vc)
  (make-vcham
   (if (>= (+ (vcham-x vc) SPEED-V4) LIMIT-X-POS)
       0
       (+ (vcham-x vc) SPEED-V4))
   (if (<= (+ (vcham-h vc) DEC) MIN)
       MIN
       (+ (vcham-h vc) DEC))
   (vcham-c vc)))

; VCham -> Image
; places the virtual cham into the BACKGROUND scene
(define (render.v5 vc)
  (place-image (circle 6 "solid" (vcham-c vc)) (vcham-x vc) Y-CHAM
               (overlay/align "middle" "top"
                              (rectangle (* (vcham-h vc) GAUGE-WIDTH .01)
                                         GAUGE-HEIGHT
                                         "solid" "red")
                              BACKGROUND-V5)))

; VCham KeyEvent -> VCham
; interacts with the cham:
;  happiness: up is 1/3 and down is 1/5
;  color: r is "red", g is "green" and b is "blue"
(check-expect (interact VCH6 "up")   (make-vcham (vcham-x VCH6) (calculate (vcham-h VCH6) INC-UP) "red"))
(check-expect (interact VCH6 "left") VCH6)
(check-expect (interact VCH6 "down") (make-vcham (vcham-x VCH6) (calculate (vcham-h VCH6) INC-DOWN) "red"))
(check-expect (interact VCH7 "up")   (make-vcham (vcham-x VCH7) MAX "red"))
(check-expect (interact VCH7 "down") (make-vcham (vcham-x VCH7) MAX "red"))
(check-expect (interact VCH3 "up")   (make-vcham (vcham-x VCH3) 1 "red"))
(check-expect (interact VCH3 "down") (make-vcham (vcham-x VCH3) 1 "red"))
(check-expect (interact VCH9 "r") VCH6)
(check-expect (interact VCH6 "g") VCH8)
(check-expect (interact VCH8 "b") VCH9)

(define (interact vc ke)
  (make-vcham (vcham-x vc)
              (cond [(key=? ke "up")   (calculate (vcham-h vc) INC-UP)]
                    [(key=? ke "down") (calculate (vcham-h vc) INC-DOWN)]
                    [else (vcham-h vc)])
              (cond [(key=? ke "r") "red"]
                    [(key=? ke "g") "green"]
                    [(key=? ke "b") "blue"]
                    [else (vcham-c vc)])))

; VCham -> Boolean
; stops when the virtual cham is sad
(check-expect (sad?.v5 VCH3) #true)
(check-expect (sad?.v5 VCH1) #false)

(define (sad?.v5 vc)
  (= (vcham-h vc) MIN))
