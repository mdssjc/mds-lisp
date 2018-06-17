;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Intermezzo 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Intermezzo 2.rkt
;; Intermezzo 2: Quote, Unquote

(require 2htdp/web-io)


;; Quote

;; Exercise 231

'(1 "a" 2 #false 3 "c")
(list 1 "a" 2 #false 3 "c")

'()
(list)

'(("alan" 1000)
  ("barb" 2000)
  ("carl" 1500))
(list (list "alan" 1000)
      (list "barb" 2000)
      (list "carl" 1500))



;; Quasiquote and Unquote

; String String -> ... deeply nested list ...
; produces a web page with given author and title
(define (my-first-web-page author title)
  `(html
    (head
     (title ,title)
     (meta ((http-equiv "content-type")
            (content "text-html"))))
    (body
     (h1 ,title)
     (p "I, " ,author ", made this page."))))

(my-first-web-page "Matthias" "Hello World")
(show-in-browser (my-first-web-page "Matthias" "Hello World"))

;; Exercise 232

`(1 "a" 2 #false 3 "c")
(list 1 "a" 2 #false 3 "c")

`(("alan" ,(* 2 500))
  ("barb" 2000)
  (,(string-append "carl" " , the great") 1500)
  ("dawn" 2300))
(list (list "alan" (* 2 500))
      (list "barb" 2000)
      (list (string-append "carl" " , the great") 1500)
      (list "dawn" 2300))

(define title "ratings")
`(html
  (head
   (title ,title))
  (body
   (h1 ,title)
   (p "A second web page")))
(list 'html
      (list 'head
            (list 'title title))
      (list 'body
            (list 'h1 title)
            (list 'p "A second web page")))



;; Unquote Splice

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond [(empty? l) '()]
        [else (cons (make-cell (first l))
                    (make-row  (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(number->string n)))

; List-of-numbers List-of-numbers -> ... nested list ...
; creates an HTML table from two lists of numbers
(define (make-table row1 row2)
  `(table ((border "1"))
          (tr ,@(make-row row1))
          (tr ,@(make-row row2))))

;; Exercise 233

(check-expect `(0 ,@'(1 2 3) 4)
              (list 0 1 2 3 4))

(check-expect `(("alan" ,(* 2 500))
                ("barb" 2000)
                (,@'("carl" " , the great") 1500)
                ("dawn" 2300))
              (list (list "alan" (* 2 500))
                    (list "barb" 2000)
                    (list "carl" " , the great" 1500)
                    (list "dawn" 2300)))

(check-expect `(html
                (body
                 (table ((border "1"))
                        (tr ((width "200"))
                            ,@(make-row '( 1  2)))
                        (tr ((width "200"))
                            ,@(make-row '(99 65))))))
              (list 'html
                    (list 'body
                          (list 'table (list (list 'border "1"))
                                (cons 'tr (cons (list (list 'width "200"))
                                                (make-row '( 1  2))))
                                (cons 'tr (cons (list (list 'width "200"))
                                                (make-row '(99 65))))))))

;; Exercise 234

(define one-list '("Asia: Heat of the Moment"
                   "U2: One"
                   "The White Stripes: Seven Nation Army"))

; ListOfSong -> ListOfHTML
; produces a list representation of an HTML table
(define (make-ranking lor)
  `(html
    (body
     (table ((border "1"))
            ,@(make-row.v2 (ranking lor))))))

; ListOfSong -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row.v2 l)
  (cond [(empty? l) '()]
        [else (cons (make-cell.v2 (first l))
                    (make-row.v2  (rest  l)))]))

; Anything -> ... nested list ...
; creates a cell for an HTML table
(define (make-cell.v2 a)
  `(tr (td ,(number->string (first a)))
       (td ((style "text-align: center"))
           ,(second a))))

; ListOfSong -> ListOfSong
; reverses the list of song
(define (ranking los)
  (reverse (add-ranks (reverse los))))

; ListOfSong -> ListOfSong
; adds an index for each element in the list of song
(define (add-ranks los)
  (cond [(empty? los) '()]
        [else (cons (list (length los) (first los))
                    (add-ranks (rest los)))]))

(show-in-browser (make-ranking one-list))
