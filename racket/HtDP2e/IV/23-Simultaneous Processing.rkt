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
