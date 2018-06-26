;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |22-Project: The Commerce of XML|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 22-Project: The Commerce of XML.rkt
;; IV - Intertwined Data
;; 22 - Project: The Commerce of XML

(require 2htdp/image)
(require 2htdp/universe)


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
; is xi a list of attributes
(define (list-of-attributes? xi)
  (cond [(empty? xi) #true]
        [else
         (local ((define possible-attribute (first xi)))
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
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(check-expect (xexpr-content xml1) '())
(check-expect (xexpr-content xml2) '((li (word) (word)) (li (word))))

(define (xexpr-content xe)
  (local ((define optional-loa+content (rest xe)))
    (cond [(empty? optional-loa+content) '()]
          [else
           (local ((define loa-or-x (first optional-loa+content)))
             (if (list-of-attributes? loa-or-x)
                 (rest optional-loa+content)
                 optional-loa+content))])))

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

(define (find-attr xi s)
  (local ((define result (assq s xi)))
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

(define (word? xi)
  (and (cons? xi)
       (= (length xi) 2)
       (symbol=? (first xi) 'word)
       (cons? (second xi))
       (= (length (first (second xi))) 2)
       (symbol=? (first (first (second xi))) 'text)
       (string? (second (first (second xi))))))

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

; An XEnum.v1 is one of:
; - (cons 'ul [List-of XItem.v1])
; - (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons Attributes (cons XWord '())))

(define xe0 '(ul (li (word ((text "one"))))
                 (li (word ((text "two"))))))

(define BT (circle 2 "solid" "black"))
(define xe0-rendered (above/align
                     'left
                     (beside/align 'center BT (text "one" 12 'black))
                     (beside/align 'center BT (text "two" 12 'black))))


;; =================
;; Functions:

;; Exercise 372

; XItem.v1 -> Image
; renders an item as a "word" prefixed by a bullet
(check-expect (render-item1 '(li (word ((text "one")))))
              (beside/align 'center BT (text "one" 12 'black)))
(check-expect (render-item1 '(li (word ((text "two")))))
              (beside/align 'center BT (text "two" 12 'black)))
(check-expect (render-item1 '(li ((attr "2")) (word ((text "one")))))
              (beside/align 'center BT (text "one" 12 'black)))

(define (render-item1 i)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word  (word-text element))
          (define item    (text a-word 12 'black)))
    (beside/align 'center BT item)))

; XEnum.v1 -> Image
; renders a simple enumeration as an image
(check-expect (render-enum1 xe0) xe0-rendered)

(define (render-enum1 xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v1 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left
                         (render-item1 item)
                         so-far)))
    (foldr deal-with-one empty-image content)))


;; =================
;; Data definitions:

; An XItem.v2 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons [List-of Attribute] (list XWord)))
; - (cons 'li (cons XEnum.v2 '()))
; - (cons 'li (cons [List-of Attribute] (list XEnum.v2)))
;
; An XEnum.v2 is one of:
; - (cons 'ul [List-of XItem.v2])
; - (cons 'ul (cons [List-of Attribute] [List-of XItem.v2]))

(define xi0 '(li (word ((text "one")))))
(define xi1 '(li ((attr "true")) (word ((text "one")))))
(define xi2 '(li (ul (li (word ((text "one"))))
                     (li (word ((text "two")))))))

(define xe1 '(ul (li (word ((text "hello"))))))
(define xe2 '(ul (li (word ((text "hello"))))
                 (li (word ((text "hello"))))))


;; =================
;; Constants:

(define SIZE 12)       ; font size
(define COLOR "black") ; font color
(define BT.V2          ; a graphical constant
  (beside (circle 1 'solid 'black) (text " " SIZE COLOR)))


;; =================
;; Functions:

;; Exercise 373

; Image -> Image
; marks item with bullet
(check-expect (bulletize empty-image)
              (beside/align 'center BT.V2 empty-image))
(check-expect (bulletize (text "my text" 12 "black"))
              (beside/align 'center BT.V2 (text "my text" 12 "black")))

(define (bulletize item)
  (beside/align 'center BT.V2 item))

; XEnum.v2 -> Image
; renders an XEnum.v2 as an image
(check-expect (render-enum xe0)
              (above/align
               'left
               (beside/align 'center BT.V2 (text "one" 12 'black))
               (beside/align 'center BT.V2 (text "two" 12 'black))))

(define (render-enum xe)
  (local ((define content (xexpr-content xe))
          ; XItem.v2 Image -> Image
          (define (deal-with-one item so-far)
            (above/align 'left (render-item item) so-far)))
    (foldr deal-with-one empty-image content)))

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item xi0)
              (beside/align 'center BT.V2 (text "one" 12 'black)))
(check-expect (render-item xi1)
              (beside/align 'center BT.V2 (text "one" 12 'black)))
(check-expect (render-item xi2)
              (beside/align
               'center BT.V2
               (above/align
                'left
                (beside/align 'center BT.V2 (text "one" 12 'black))
                (beside/align 'center BT.V2 (text "two" 12 'black)))))

(define (render-item an-item)
  (local ((define content (first (xexpr-content an-item))))
    (bulletize
     (cond [(word? content)
            (text (word-text content) SIZE 'black)]
           [else
            (render-enum content)]))))

;; Exercise 374


;; =================
;; Data definitions:

; An XItem.v3 is one of:
; - (cons 'li (cons XWord '()))
; - (cons 'li (cons AL LXW)
; - (cons 'li (cons XEnum.v3 '()))
; - (cons 'li (cons AL LXE))
; An XEnum.v3 is one of:
; - (cons 'ul LXI)
; - (cons 'ul (cons AL LXI))
; An AL is one of:
; - '()
; - (cons Attribute AL)
; An LXW is one of:
; - '()
; - (cons XWord LXW)
; An LXE is one of:
; - '()
; - (cons XEnum.v3 LXE)
; An LXI is one of:
; - '()
; - (cons XItem.v3 LXI)

;; Exercise 375


;; =================
;; Functions:

; XItem.v2 -> Image
; renders one XItem.v2 as an image
(check-expect (render-item.v2 xi0)
              (beside/align 'center BT.V2 (text "one" 12 'black)))
(check-expect (render-item.v2 xi1)
              (beside/align 'center BT.V2 (text "one" 12 'black)))
(check-expect (render-item.v2 xi2)
              (beside/align
               'center BT.V2
               (above/align
                'left
                (beside/align 'center BT.V2 (text "one" 12 'black))
                (beside/align 'center BT.V2 (text "two" 12 'black)))))

(define (render-item.v2 an-item)
  (local ((define content (first (xexpr-content an-item))))
    (cond [(word? content)
           (bulletize (text (word-text content) SIZE 'black))]
          [else
           (bulletize (render-enum content))])))

;; Exercise 376

; XEnum.v2 -> Number
; counts all "hello"s in an instance of XEnum.v2
(check-expect (count-hello xe0) 0)
(check-expect (count-hello xe1) 1)
(check-expect (count-hello xe2) 2)

(define (count-hello xe)
  (local ((define (count-hello xi acc)
            (+ (count-hello-item xi) acc))
          (define (count-hello-item xi)
            (local ((define element (first (xexpr-content xi))))
              (cond [(word? element)
                     (if (string=? (word-text element) "hello") 1 0)]
                    [else
                     (count-hello element)]))))
    (foldr count-hello 0 (xexpr-content xe))))

;; Exercise 377

; XEnum.v2 -> XEnum.v2
; replaces all "hello"s with "bye" in an enumeration
(check-expect (replace-hello-bye xe0) xe0)
(check-expect (replace-hello-bye xe1)
              '(ul (li (word ((text "bye"))))))
(check-expect (replace-hello-bye xe2)
              '(ul (li (word ((text "bye"))))
                   (li (word ((text "bye"))))))

(define (replace-hello-bye xe)
  (local ((define (make-element x content)
            (local ((define attr (xexpr-attr x)))
              (cons (xexpr-name x)
                    (cond [(empty? attr) content]
                          [else
                           (cons attr content)]))))
          (define (replace-hello-item xi)
            (local ((define (replace-element element)
                      (cond [(word? element)
                             (if (string=? (word-text element) "hello")
                                 '(word ((text "bye")))
                                 element)]
                            [else
                             (replace-hello-bye element)])))
              (make-element xi (list (replace-element (first (xexpr-content xi))))))))
    (make-element xe (map replace-hello-item (xexpr-content xe)))))



;; 22.3 Domain-Specific Languages


;; =================
;; Data definitions:

; An FSM is a [List-of 1Transition]
; A 1Transition is a list of two items:
;   (cons FSM-State (cons FSM-State '()))
; An FSM-State is a String that specifies a color

; data examples
(define fsm-traffic
  '(("red" "green") ("green" "yellow") ("yellow" "red")))


;; =================
;; Functions:

;; Exercise 378

; FSM FSM-State -> FSM-State
; matches the keys pressed by a player with the given FSM
(define (simulate state0 transitions)
  (big-bang state0 ; FSM-State
            [to-draw (lambda (current)
                       (overlay
                        (text current 30 "white")
                        (square 100 "solid" current)))]
            [on-key (lambda (current key-event)
                      (find transitions current))]))

; [X Y] [List-of [List X Y]] X -> Y
; finds the matching Y for the given X in alist
(define (find alist x)
  (local ((define fm (assoc x alist)))
    (if (cons? fm)
        (second fm)
        (error "not found"))))
