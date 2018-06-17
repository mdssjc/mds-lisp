;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |22-Project: The Commerce of XML|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 22-Project: The Commerce of XML.rkt
;; IV - Intertwined Data
;; 22 - Project: The Commerce of XML



;; 22.1 - XML as S-expressions


;; =================
;; Data definitions:

; An Xexpr.v0 (short for X-expression) is a one-item list:
;   (cons Symbol '())

; An Xexpr.v1 is a list:
;   (cons Symbol [List-of Xexpr.v1])

; An Xexpr.v2 is a list:
; - (cons Symbol Body)
; - (cons Symbol (cons [List-of Attribute] Body))
; where Body is short for [List-of Xexpr.v2]
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

;; Exercise 363

; An Xexpr.v2 is a list:
;   (cons Symbol XL)
; An XL is one of:
; - '()
; - Xexpr.v2
; - (cons Xexpr.v2 XL)
; - (cons AL (cons Xexpr.v2 XL))
; An AL is one of:
; - '()
; - (cons Attribute AL)
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

;; Exercise 364

(define xml1 '(transition ((from "seen-e")
                           (to   "seen-f"))))

(define xml2 '(ul (li (word)
                      (word))
                  (li (word))))

;; Which one could be represented in Xexpr.v0 or Xexpr.v1?
;; Xexpr.v0 -> none
;; Xexpr.v1 -> xml2

;; Exercise 365

;; 1.
;; <server name="example.org" />
;;
;; 2.
;; <carcas>
;;   <board>
;;     <grass />
;;   </board>
;;   <player name="sam" />
;; </carcas>
;;
;; 3.
;; <start />

;; Which ones are elements of Xexpr.v0 or Xexpr.v1?
;; Xexpr.v0 -> 3
;; Xexpr.v1 -> 3

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))


;; =================
;; Functions:

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (xexpr-attr xe)
  (local ((define optional-loa+content (rest xe)))
    (cond [(empty? optional-loa+content) '()]
          [else
           (local ((define loa-or-x (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 loa-or-x
                 '()))])))

; [List-of Attribute] or Xexpr.v2 -> Boolean
; is x a list of attributes
(define (list-of-attributes? x)
  (cond [(empty? x) #true]
        [else
         (local ((define possible-attribute (first x)))
           (cons? possible-attribute))]))

;; Exercise 366


;; =================
;; Functions:

; Xexpr.v2 -> Symbol
; retrive the name of xe
(check-expect (xexpr-name e0)   'machine)
(check-expect (xexpr-name e1)   'machine)
(check-expect (xexpr-name e2)   'machine)
(check-expect (xexpr-name e3)   'machine)
(check-expect (xexpr-name e4)   'machine)
(check-expect (xexpr-name xml1) 'transition)
(check-expect (xexpr-name xml2) 'ul)

(define (xexpr-name xe)
  (first xe))

; Xexpr.v2 -> [List-of Symbol]
; retrives the contents of xe
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '(action))
(check-expect (xexpr-content e3) '(action))
(check-expect (xexpr-content e4) '(action action))
(check-expect (xexpr-content xml1) '())
(check-expect (xexpr-content xml2) '(li word word li word))

(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe))
          (define (flatten xe)
            (cond [(empty? xe) '()]
                  [(not (list? xe)) (list xe)]
                  [else
                   (append (flatten (first xe))
                           (flatten (rest xe)))])))
    (cond [(empty? optional-loa+content) '()]
          [else
           (local ((define loa-or-x (first optional-loa+content)))
             (flatten (if (list-of-attributes? loa-or-x)
                          (rest optional-loa+content)
                          optional-loa+content)))])))

;; Exercise 367


;; =================
;; Functions:

; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr.v2 e0) '())
(check-expect (xexpr-attr.v2 e1) '((initial "X")))
(check-expect (xexpr-attr.v2 e2) '())
(check-expect (xexpr-attr.v2 e3) '())
(check-expect (xexpr-attr.v2 e4) '((initial "X")))

(define (xexpr-attr.v2 xe)
  (cond [(empty? (rest xe)) '()]
        [else
         (local ((define loa-or-x (first (rest xe))))
           (if (list-of-attributes? loa-or-x)
               loa-or-x
               (xexpr-attr.v2 loa-or-x)))]))

;; Exercise 368


;; =================
;; Data definitions:

; An Token is one of:
; - [List-of Attribute]
; - Xexpr.v2

;; Exercise 369


;; =================
;; Functions:

; [List-of Attribute] Symbol -> String or #false
; If the attributes list associates the symbol with a string, the function
; retrieves this string; otherwise it returns #false
(check-expect (find-attr a0 'attr) #false)
(check-expect (find-attr a0 'initial) "X")

(define (find-attr x s)
  (local ((define result (assq s x)))
    (if (false? result)
        result
        (second result))))



;; 22.2 - Rendering XML Enumerations


;; =================
;; Data definitions:

; An XWord is '(word ((text String))).

;; Exercise 370

(define XW1 '(word ((text ""))))
(define XW2 '(word ((text "10"))))
(define XW3 '(word ((text "true"))))


;; =================
;; Functions:

; Any -> Boolean
; checks whether some ISL+ value is in XWord
(check-expect (word? XW1)                        #true)
(check-expect (word? XW2)                        #true)
(check-expect (word? XW3)                        #true)
(check-expect (word? 0)                          #false)
(check-expect (word? #true)                      #false)
(check-expect (word? '())                        #false)
(check-expect (word? '(word))                    #false)
(check-expect (word? '(word ()))                 #false)
(check-expect (word? '(word (())))               #false)
(check-expect (word? '(word ((text))))           #false)
(check-expect (word? '(word ((text 10))))        #false)
(check-expect (word? '(word ((text key value)))) #false)
(check-expect (word? '(words ((text "10"))))     #false)

(define (word? x)
  (and (cons? x)
       (= (length x) 2)
       (symbol=? (first x) 'word)
       (cons? (second x))
       (= (length (first (second x))) 2)
       (symbol=? (first (first (second x))) 'text)
       (string? (second (first (second x))))))

; XWord -> String
; extracts the value of the only attribute of an instance of XWord
(check-expect (word-text XW1) "")
(check-expect (word-text XW2) "10")
(check-expect (word-text XW3) "true")

(define (word-text xw)
  (second (first (second xw))))

;; Exercise 371


;; =================
;; Data definitions:

; An Xexpr is a list:
;   (cons Symbol XL)
; An XL is one of:
; - [List-of Xexpr]
; - (cons [List-of Attribute] [List-of Xexpr])
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))
