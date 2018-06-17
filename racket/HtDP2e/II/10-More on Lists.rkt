;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |10-More on Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 10-More on Lists.rkt
;; II - Arbitrarily Large Data
;; 10 - More on Lists

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require racket/string)


;; 10.1 - Functions that Produce Lists

;; Exercise 161


;; =================
;; Constants:

(define WAGE-COST 14)


;; =================
;; Functions:

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage* '()) '())
(check-expect (wage* (cons 28 '())) (cons (* WAGE-COST 28) '()))
(check-expect (wage* (cons 4 (cons 2 '()))) (cons (* WAGE-COST 4) (cons (* WAGE-COST 2) '())))

(define (wage* whrs)
  (cond [(empty? whrs) '()]
        [else (cons (wage (first whrs))
                    (wage* (rest whrs)))]))

; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* WAGE-COST h))

;; Exercise 162

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(check-expect (wage*.v2 '()) '())
(check-expect (wage*.v2 (cons 28 '())) (cons (* WAGE-COST 28) '()))
(check-expect (wage*.v2 (cons 4 (cons 2 '()))) (cons (* WAGE-COST 4) (cons (* WAGE-COST 2) '())))
(check-error  (wage*.v2 (cons 12 (cons 100 '()))) "No employee could possibly work more than 100 hours per week")

(define (wage*.v2 whrs)
  (cond [(empty? whrs) '()]
        [(>= (first whrs) 100)
         (error "No employee could possibly work more than 100 hours per week")]
        [else (cons (wage (first whrs))
                    (wage*.v2 (rest whrs)))]))

;; Exercise 163


;; =================
;; Functions:

; List-of-numbers -> List-of-numbers
; converts a list of measurements in Fahrenheit to a list of Celsius measurements
(check-expect (convertFC '()) '())
(check-expect (convertFC (cons 98.6 '())) (cons 37 '()))
(check-within (convertFC (cons 98.6 (cons 85 '()))) (cons 37 (cons 29.4 '())) 0.1)

(define (convertFC t)
  (cond [(empty? t) '()]
        [else (cons (fahrenheit->celsius (first t))
                    (convertFC (rest t)))]))

; Number -> Number
; converts the temperature Fahrenheit in Celsius
(check-expect (fahrenheit->celsius 98.6) 37)
(check-expect (fahrenheit->celsius 212)  100)
(check-expect (fahrenheit->celsius -40)  -40)

(define (fahrenheit->celsius f)
  (* (- f 32) (/ 5 9)))

;; Exercise 164


;; =================
;; Constants:

(define RATE 0.920)


;; =================
;; Functions:

; List-of-numbers -> List-of-numbers
; converts a list of US$ amounts into a list of € amounts
(check-expect (convert-euro* '()) '())
(check-within (convert-euro* (cons 1 '())) (cons 0.92 '()) 0.1)
(check-within (convert-euro* (cons 1 (cons 3.5 '()))) (cons 0.92 (cons 3.22 '())) 0.1)

(define (convert-euro* d)
  (cond [(empty? d) '()]
        [else (cons (convert-euro (first d))
                    (convert-euro* (rest d)))]))

; Number -> Number
; converts the US$ in € with the current exchange rate
(check-within (convert-euro 1)   0.92 0.1)
(check-within (convert-euro 3.5) 3.22 0.1)

(define (convert-euro d)
  (* d RATE))

;; Exercise 165


;; =================
;; Functions:

; List-of-strings -> List-of-strings
; consumes a list of toy descriptions (one-word strings) and
; replaces all occurrences of "robot" with "r2d2"
(check-expect (subst-robot '()) '())
(check-expect (subst-robot (cons "robot-1" '())) (cons "r2d2-1" '()))
(check-expect (subst-robot (cons "robot-1" (cons ">>robot<<" '())))
              (cons "r2d2-1" (cons ">>r2d2<<" '())))

(define (subst-robot los)
  (substitute "r2d2" "robot" los))

; String String List-of-strings -> List-of-strings
; replaces all occurrences of new with old in the List-of-strings
(define (substitute new old los)
  (cond [(empty? los) '()]
        [else (cons (string-replace (first los) old new)
                    (substitute new old (rest los)))]))



;; 10.2 - Structures in Lists

;; Exercise 166


;; =================
;; Data definitions:

(define-struct work [employee number rate hours])
; A (piece of) Work is a structure:
;   (make-work String Number Number Number)
; interpretation (make-work n num r h) combines the name
; with the number num, pay rate r and the number of hours h

(define-struct pay-check (number name amount))
; A Pay-check is a structure:
;  (make-pay-check Number String Number)
; interpretation (make-pay-check num n a) contains
; the employee's number num, name n and an amount a

; A Low (short for list of works) is one of:
; - '()
; - (cons Work Low)
; interpretation an instance of Low represents the
; hours worked for a number of employees

; A Lop is one of:
; - '()
; - (cons Pay-check Lop)
; interpretation an instance of Lop represents the
; employee's name and an amount


;; =================
;; Functions:

; Low -> Lop
; consumes a list of work records and computes a list of pay checks
(check-expect (wage*.v3 '()) '())
(check-expect (wage*.v3 (cons (make-work "Robby" 1 11.95 39) '()))
              (cons (make-pay-check 1 "Robby" (* 11.95 39)) '()))
(check-expect (wage*.v3 (cons (make-work "Matthew" 1 12.95 45)
                              (cons (make-work "Robby" 2 11.95 39) '())))
              (cons (make-pay-check 1 "Matthew" (* 12.95 45))
                    (cons (make-pay-check 2 "Robby" (* 11.95 39)) '())))

(define (wage*.v3 an-low)
  (cond [(empty? an-low) '()]
        [(cons? an-low) (cons (create-paycheck (first an-low))
                              (wage*.v3 (rest an-low)))]))

; Work -> Pay-check
; creates a paycheck for the given work w
(define (create-paycheck w)
  (make-pay-check (work-number w)
                  (work-employee w)
                  (wage.v3 w)))

; Work -> Number
; computes the wage for the given work record w
(define (wage.v3 w)
  (* (work-rate w) (work-hours w)))

;; Exercise 167


;; =================
;; Data definitions:

; A Lop is one of:
; - '()
; - (cons Posn Lop)
; interpretation an instance of Lop represents a Posn list


;; =================
;; Functions:

; Lop -> Number
; consumes a list of Posns and produces the sum of all of its x-coordinates
(check-expect (sum '()) 0)
(check-expect (sum (cons (make-posn 11 20) '())) 11)
(check-expect (sum (cons (make-posn 11 20) (cons (make-posn 48 66) '()))) (+ 11 48))

(define (sum lop)
  (cond [(empty? lop) 0]
        [else (+ (posn-x (first lop))
                 (sum (rest lop)))]))

;; Exercise 168


;; =================
;; Functions:

; Lop -> Lop
; produces lists of Posns with (make-posn x (+ y 1)) - known as translation
(check-expect (translate '()) '())
(check-expect (translate (cons (make-posn 10 10) '()))
              (cons (make-posn 10 11) '()))
(check-expect (translate (cons (make-posn 10 10) (cons (make-posn 20 20) '())))
              (cons (make-posn 10 11) (cons (make-posn 20 21) '())))

(define (translate lop)
  (cond [(empty? lop) '()]
        [else (cons (make-posn (posn-x (first lop))
                               (add1 (posn-y (first lop))))
                    (translate (rest lop)))]))

;; Exercise 169


;; =================
;; Functions:

; Lop -> Lop
; produces lists of Posns whose x-coordinates are between 0 and 100
; and whose y-coordinates are between 0 and 200
(check-expect (legal '()) '())
(check-expect (legal (cons (make-posn -1  100)  '())) '())
(check-expect (legal (cons (make-posn 0   100)  '())) (cons (make-posn 0 100)   '()))
(check-expect (legal (cons (make-posn 50  100)  '())) (cons (make-posn 50 100)  '()))
(check-expect (legal (cons (make-posn 100 100)  '())) (cons (make-posn 100 100) '()))
(check-expect (legal (cons (make-posn 101 100)  '())) '())
(check-expect (legal (cons (make-posn 50  -1)   '())) '())
(check-expect (legal (cons (make-posn 50  0)    '())) (cons (make-posn 50 0)   '()))
(check-expect (legal (cons (make-posn 50  100)  '())) (cons (make-posn 50 100) '()))
(check-expect (legal (cons (make-posn 50  200)  '())) (cons (make-posn 50 200) '()))
(check-expect (legal (cons (make-posn 50  201)  '())) '())
(check-expect (legal (cons (make-posn -1  100)
                           (cons (make-posn 50 100)
                                 (cons (make-posn 50 -1)
                                       (cons (make-posn 50 100) '())))))
              (cons (make-posn 50 100) (cons (make-posn 50 100) '())))

(define (legal lop)
  (cond [(empty? lop) '()]
        [(< (posn-x (first lop)) 0)   (legal (rest lop))]
        [(> (posn-x (first lop)) 100) (legal (rest lop))]
        [(< (posn-y (first lop)) 0)   (legal (rest lop))]
        [(> (posn-y (first lop)) 200) (legal (rest lop))]
        [else (cons (first lop)
                    (legal (rest lop)))]))

;; Exercise 170


;; =================
;; Data definitions:

(define-struct phone [area switch four])
; A Phone is a structure:
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999.
; A Four is a Number between 1000 and 9999.
(define P1 (make-phone 713 123 4567))
(define P2 (make-phone 281 123 4567))
(define P3 (make-phone 123 123 4567))

; A Lop is one of:
; - '()
; - (cons Phone Lop)
; interpretation an instance of Lop represents a Phone list


;; =================
;; Functions:

; Lop -> Lop
; replaces all occurrence of area code 713 with 281
(check-expect (replace* '()) '())
(check-expect (replace* (cons P1 '())) (cons P2 '()))
(check-expect (replace* (cons P1 (cons P3 '()))) (cons P2 (cons P3 '())))

(define (replace* lop)
  (cond [(empty? lop) '()]
        [else (cons (replace (first lop))
                    (replace* (rest lop)))]))

; Phone -> Phone
; replaces the area code 713 with 281
(define (replace p)
  (cond [(= (phone-area p) 713)
         (make-phone 281 (phone-switch p) (phone-four p))]
        [else p]))



;; 10.3 - Lists in Lists, Files

;; Exercise 171


;; =================
;; Data definitions:

; A Los is one of:
; - '()
; - (cons String Los)
; interpretation a list of Strings, each is a String
(define line0 (cons "hello" (cons "world" '())))
(define line1 '())
(define line2 (cons "the" (cons "car" '())))
(define line3 (cons "an" (cons "apple" '())))
(define line4 (cons "a" (cons "kite" '())))
(define line5 (cons "a" (cons "an" (cons "the" '()))))
(define line6 (cons "two" (cons "days" '())))

; A LLS is one of:
; - '()
; - (cons Los LLS)
; interpretation a list of lines, each is a list of Strings
(define lls0 '())
(define lls1 (cons line0 (cons line1 '())))
(define lls2 (cons line0 (cons line1 (cons line0 '()))))
(define lls3 (cons line0 (cons line0 (cons line0 '()))))
(define lls4 (cons line2 (cons line1 (cons line3 (cons line4 '())))))

;; Exercise 172


;; =================
;; Functions:

; LLS -> String
; converts a list of lines into a String
; the Strings should be separated by blank spaces (" ")
; the lines should be separated with a newline ("\n")
(check-expect (collapse lls0) "")
(check-expect (collapse lls1) "hello world\n")
(check-expect (collapse lls2) "hello world\n\nhello world")
(check-expect (collapse lls3) "hello world\nhello world\nhello world")

(define (collapse lls)
  (cond [(empty? lls) ""]
        [else (string-append (collapse-line (first lls))
                             (if (empty? (rest lls)) "" "\n")
                             (collapse (rest lls)))]))

; Los -> String
; converts a list of Strings into a String
; the Strings should be separated by blank spaces (" ")
(check-expect (collapse-line line0) "hello world")
(check-expect (collapse-line line1) "")

(define (collapse-line los)
  (cond [(empty? los) ""]
        [else (string-append (first los)
                             (if (empty? (rest los)) "" " ")
                             (collapse-line (rest los)))]))


; Test drive
(write-file "ttt.dat" (collapse (read-words/line "ttt.txt")))

;; Exercise 173


;; =================
;; Functions:

; String -> String
; consumes the name n of a file, reads the file, removes the articles,
; and writes the result out to a file whose name is the result of
; concatenating "no-articles-" with n
(define (remove-articles** n)
  (write-file (string-append "no-articles-" n)
              (remove-articles* (read-words/line "ttt.txt"))))

; LLS -> String
; converts a list of lines into a String without articles
; the Strings should be separated by blank spaces (" ")
; the lines should be separated with a newline ("\n")
(check-expect (remove-articles* lls0) "")
(check-expect (remove-articles* lls4) "car\n\napple\nkite")

(define (remove-articles* lls)
  (cond [(empty? lls) ""]
        [else (string-append (remove-articles (first lls))
                             (if (empty? (rest lls)) "" "\n")
                             (remove-articles* (rest lls)))]))

; Los -> String
; converts a list of Strings into a String without articles
; the Strings should be separated by blank spaces (" ")
; an article is one of the following three words: "a", "an", and "the"
(check-expect (remove-articles line1) "")
(check-expect (remove-articles line2) "car")
(check-expect (remove-articles line3) "apple")
(check-expect (remove-articles line4) "kite")
(check-expect (remove-articles line5) "")
(check-expect (remove-articles line6) "two days")

(define (remove-articles los)
  (cond [(empty? los) ""]
        [else (string-append (cond [(string=? (first los) "a")   ""]
                                   [(string=? (first los) "an")  ""]
                                   [(string=? (first los) "the") ""]
                                   [else (string-append (first los)
                                                        (if (empty? (rest los)) "" " "))])
                             (remove-articles (rest los)))]))


; Test drive
(remove-articles** "ttt")

;; Exercise 174


;; =================
;; Functions:

; LLS -> LLS
; encodes text files numerically
(check-expect (encode '()) '())
(check-expect (encode (cons (cons "a" (cons "b" '()))
                            (cons (cons  "1" (cons "2" '())) '())))
              (cons (cons "097" (cons "098" '()))
                    (cons (cons "049" (cons "050" '())) '())))

(define (encode lls)
  (cond [(empty? lls) '()]
        [else (cons (encode-lines (first lls))
                    (encode (rest lls)))]))

; Los -> Los
; encodes list of Strings numerically
(check-expect (encode-lines '()) '())
(check-expect (encode-lines (cons "z" (cons "a" '()))) (cons "122" (cons "097" '())))

(define (encode-lines los)
  (cond [(empty? los) '()]
        [else (cons (encode-letters (explode (first los)))
                    (encode-lines (rest los)))]))

; Los -> String
; encodes String numerically
(check-expect (encode-letters '()) "")
(check-expect (encode-letters (cons "z" (cons "a" '()))) "122097")

(define (encode-letters s)
  (cond [(empty? s) ""]
        [else (string-append (encode-letter (first s))
                             (encode-letters (rest s)))]))

; 1String -> String
; converts the given 1String to a 3-letter numeric String
(check-expect (encode-letter "z")  (code1 "z"))
(check-expect (encode-letter "\t") (string-append "00" (code1 "\t")))
(check-expect (encode-letter "a")  (string-append "0" (code1 "a")))

(define (encode-letter s)
  (cond [(>= (string->int s) 100) (code1 s)]
        [(< (string->int s)  10)  (string-append "00" (code1 s))]
        [(< (string->int s)  100) (string-append "0" (code1 s))]))

; 1String -> String
; convert the given 1String into a String
(check-expect (code1 "z") "122")

(define (code1 c)
  (number->string (string->int c)))


; Test drive
(encode (read-words/line "ttt.txt"))

;; Exercise 175


;; =================
;; Functions:

; String -> String
; consumes the name of a file and produces a value that consists of three numbers
(define (wc filename)
  (count (read-words/line filename)))

; LLS -> String
; counts the number of 1Strings, words, and lines
(check-expect (count lls0) "0 0 0")
(check-expect (count lls1) "10 2 2")

(define (count lls)
  (string-append (number->string (count-1Strings lls)) " "
                 (number->string (count-words lls)) " "
                 (number->string (count-lines lls))))

; LLS -> Number
; counts the number of 1Strings
(check-expect (count-1Strings lls0) 0)
(check-expect (count-1Strings lls1) 10)

(define (count-1Strings lls)
  (cond [(empty? lls) 0]
        [else (+ (count-1Strings* (first lls))
                 (count-1Strings (rest lls)))]))

(define (count-1Strings* los)
  (cond [(empty? los) 0]
        [else (+ (string-length (first los))
                 (count-1Strings* (rest los)))]))

; LLS -> String
; counts the number of words
(check-expect (count-words lls0) 0)
(check-expect (count-words lls1) 2)

(define (count-words lls)
  (cond [(empty? lls) 0]
        [else (+ (length (first lls))
                 (count-words (rest lls)))]))

; LLS -> String
; counts the number of lines
(check-expect (count-lines lls0) 0)
(check-expect (count-lines lls1) 2)

(define (count-lines lls)
  (length lls))


; Test drive
(wc "ttt.txt")

;; Exercise 176


;; =================
;; Data definitions:

; A Row is one of:
;  - '()
;  - (cons Number Row)
(define row1 (cons 11 (cons 12 '())))
(define row2 (cons 21 (cons 22 '())))

; A Matrix is one of:
;  - (cons Row '())
;  - (cons Row Matrix)
; constraint all rows in matrix are of the same length
(define mat1 (cons row1 (cons row2 '())))


;; =================
;; Functions:

; Matrix -> Matrix
; transposes the given matrix along the diagonal
(define wor1 (cons 11 (cons 21 '())))
(define wor2 (cons 12 (cons 22 '())))
(define tam1 (cons wor1 (cons wor2 '())))

(check-expect (transpose mat1) tam1)
(check-expect (transpose (cons (cons 1 (cons 2 '()))
                               (cons (cons 3 (cons 4 '())) '())))
              (cons (cons 1 (cons 3 '()))
                    (cons (cons 2 (cons 4 '())) '())))

(define (transpose lln)
  (cond [(empty? (first lln)) '()]
        [else (cons (first* lln)
                    (transpose (rest* lln)))]))

; Matrix -> Row
; consumes a matrix and produces the first column as a list of numbers
(check-expect (first* '()) '())
(check-expect (first* mat1) (cons 11 (cons 21 '())))

(define (first* m)
  (cond [(empty? m) '()]
        [else (cons (first (first m))
                    (first* (rest m)))]))

; Matrix -> Matrix
; consumes a matrix and removes the first column
(check-expect (rest* '()) '())
(check-expect (rest* mat1) (cons (cons 12 '()) (cons (cons 22 '()) '())))

(define (rest* m)
  (cond [(empty? m) '()]
        [else (cons (rest (first m))
                    (rest* (rest m)))]))



;; 10.4 - A Graphical Editor, Revisited

;; Exercise 177
;; Exercise 178
; Because its length is 1 (\t and \b are escape characters), the same as the
; alphanumeric characters for (string-length k).
;; Exercise 179
;; Exercise 180


;; =================
;; Constants:

(define HEIGHT 20) ; the height of the editor
(define WIDTH 200) ; its width
(define FONT-SIZE 16) ; the font size
(define FONT-COLOR "black") ; the font color

(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


;; =================
;; Data definitions:

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S)
; An Lo1S is one of:
; - '()
; - (cons 1String Lo1S)


;; =================
;; Functions:

; Lo1s -> Lo1s
; produces a reverse version of the given list
(check-expect (rev (cons "a" (cons "b" (cons "c" '()))))
              (cons "c" (cons "b" (cons "a" '()))))

(define (rev l)
  (cond [(empty? l) '()]
        [else (add-at-end (rev (rest l)) (first l))]))

; Lo1s 1String -> Lo1s
; creates a new list by adding s to the end of l
(check-expect (add-at-end (cons "c" (cons "b" '())) "a")
              (cons "c" (cons "b" (cons "a" '()))))
(check-expect (add-at-end '() "s")
              (cons "s" '()))

(define (add-at-end l s)
  (cond [(empty? l) (cons s '())]
        [else (cons (first l)
                    (add-at-end (rest l) s))]))

; String String -> Editor
; produces an Editor
(check-expect (create-editor "left" "right")
              (make-editor (rev (explode "left")) (explode "right")))

(define (create-editor s1 s2)
  (make-editor (rev (explode s1)) (explode s2)))

(define e1 (create-editor "" ""))
(define e2 (create-editor "left" "right"))
(define e3 (create-editor "" "right"))
(define e4 (create-editor "left" ""))

; main : String -> Editor
; launches the editor given some initial string
(define (main s)
  (big-bang (create-editor s "")
            [on-key  editor-kh]
            [to-draw editor-render]))

; Editor -> Image
; renders an editor as an image of the two texts
; separated by the cursor
(define (editor-render ed)
  (place-image/align
   (beside (editor-text (reverse (editor-pre ed)))
           CURSOR
           (editor-text (editor-post ed)))
   1 1
   "left" "top"
   MT))

; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (create-editor "cde" "fgh"))

(define (editor-kh ed k)
  (cond [(key=? k "left")  (editor-lft ed)]
        [(key=? k "right") (editor-rgt ed)]
        [(key=? k "\b")    (editor-del ed)]
        [(key=? k "\t") ed]
        [(key=? k "\r") ed]
        [(= (string-length k) 1) (editor-ins ed k)]
        [else ed]))

; Editor String -> Editor
; inserts the 1String k between pre and post
(check-expect (editor-ins (make-editor '() '()) "e")
              (make-editor (cons "e" '()) '()))

(check-expect (editor-ins (make-editor (cons "d" '())
                                       (cons "f" (cons "g" '())))
                          "e")
              (make-editor (cons "e" (cons "d" '()))
                           (cons "f" (cons "g" '()))))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor -> Editor
; moves the cursor position one 1String left,
; if possible
(check-expect (editor-lft e1) e1)
(check-expect (editor-lft e2) (create-editor "lef" "tright"))
(check-expect (editor-lft e3) e3)

(define (editor-lft ed)
  (cond [(empty? (editor-pre ed)) ed]
        [else (make-editor (rest (editor-pre ed))
                           (cons (first (editor-pre ed))
                                 (editor-post ed)))]))

; Editor -> Editor
; moves the cursor position one 1String right,
; if possible
(check-expect (editor-rgt e1) e1)
(check-expect (editor-rgt e2) (create-editor "leftr" "ight"))
(check-expect (editor-rgt e4) e4)

(define (editor-rgt ed)
  (cond [(empty? (editor-post ed)) ed]
        [else (make-editor (reverse (add-at-end (reverse (editor-pre ed))
                                                (first (editor-post ed))))
                           (rest (editor-post ed)))]))

; Editor -> Editor
; deletes a 1String to the left of the cursor
; if possible
(check-expect (editor-del e1) e1)
(check-expect (editor-del e2) (create-editor "lef" "right"))
(check-expect (editor-del e3) e3)

(define (editor-del ed)
  (cond [(empty? (editor-pre ed)) ed]
        [else (make-editor (rest (editor-pre ed))
                           (editor-post ed))]))

; Lo1s -> Image
; renders a list of 1Strings as a text image
(check-expect (editor-text (cons "p" (cons "o" (cons "s" (cons "t" '())))))
              (text "post" FONT-SIZE FONT-COLOR))

(define (editor-text s)
  (cond [(empty? s) empty-image]
        [else (beside (text (first s) FONT-SIZE FONT-COLOR)
                      (editor-text (rest s)))]))
