;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |4-Intervals, Enumerations, and Itemizations|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 4-Intervals, Enumerations, and Itemizations.rkt
;; I - Fixed-Size Data
;; 4 - Intervals, Enumerations, and Itemizations

(require 2htdp/image)
(require 2htdp/universe)


;; 4.1 - Programming with Conditionals



;; 4.2 - Computing Conditionally

;; Exercise 48


;; =================
;; Data definitions:

; A PositiveNumber is a Number greater than/equal to 0.


;; =================
;; Functions:

; PositiveNumber -> String
; computes the reward level from the given score s
(define (reward s)
  (cond
    [(<= 0 s 10) "bronze"]
    [(and (< 10 s) (<= s 20)) "silver"]
    [else "gold"]))


(reward 18)

(cond
  [(<= 0 18 10) "bronze"]
  [(and (< 10 18) (<= 18 20)) "silver"]
  [else "gold"])

(cond
  [#false "bronze"]
  [(and (< 10 18) (<= 18 20)) "silver"]
  [else "gold"])

(cond
  [(and #true (<= 18 20)) "silver"]
  [else "gold"])

(cond
  [(and #true #true) "silver"]
  [else "gold"])

(cond
  [#true "silver"]
  [else "gold"])

"silver"

;; Exercise 49

; (- 200 (cond [(> y 200) 0] [else y]))

; y as 100
(- 200 (cond [(> 100 200) 0] [else 100]))

(- 200 (cond [#false 0] [else 100]))

(- 200 (cond [else 100]))

(- 200 100)

100

; y as 210
(- 200 (cond [(> 210 200) 0] [else 210]))

(- 200 (cond [#true 0] [else 210]))

(- 200 0)

200

; --

;; =================
;; Constants:

(define WIDTH-V1  100)
(define HEIGHT-V1 60)
(define MTSCN  (empty-scene WIDTH-V1 HEIGHT-V1))
(define ROCKET-V1 (rectangle 10 20 "solid" "blue"))
(define ROCKET-CENTER-TO-TOP (- HEIGHT-V1 (/ (image-height ROCKET-V1) 2)))


;; =================
;; Functions:

(define (create-rocket-scene.v5a h)
  (cond
    [(<= h ROCKET-CENTER-TO-TOP)
     (place-image ROCKET-V1 50 h MTSCN)]
    [(> h ROCKET-CENTER-TO-TOP)
     (place-image ROCKET-V1 50 ROCKET-CENTER-TO-TOP MTSCN)]))

(define (create-rocket-scene.v5b h)
  (place-image ROCKET-V1
               50 (cond [(<= h ROCKET-CENTER-TO-TOP) h]
                        [(>  h ROCKET-CENTER-TO-TOP) ROCKET-CENTER-TO-TOP])
               MTSCN))



;; 4.3 - Enumerations

;; Exercise 50
;; Exercise 51


;; =================
;; Constants:

(define WIDTH-V2  400)
(define HEIGHT-V2 200)
(define MTS (empty-scene WIDTH-V2 HEIGHT-V2))


;; =================
;; Data definitions:

; A TrafficLight is one of the following Strings:
; - "red"
; - "green"
; - "yellow"
; interpretation the three strings represent the three
; possible states that a traffic light may assume


;; =================
;; Functions:

; TrafficLight -> TrafficLight
; start the world with (main "red")
(define (main tl)
  (big-bang tl
            (on-tick traffic-light-next 3)
            (on-draw render)))

; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red")    "green")
(check-expect (traffic-light-next "green")  "yellow")
(check-expect (traffic-light-next "yellow") "red")

(define (traffic-light-next s)
  (cond
    [(string=? "red"    s) "green"]
    [(string=? "green"  s) "yellow"]
    [(string=? "yellow" s) "red"]))

; TrafficLight -> Image
; produces the traffic light image with world state
(define (render tl)
  (place-image (circle (/ HEIGHT-V2 3) "solid" tl)
               (/ WIDTH-V2  2)
               (/ HEIGHT-V2 2)
               MTS))



;; 4.4 - Intervals

;; Exercise 52

"1: 3 4 5"
"2: 4 5"
"3: 3 4"
"4: 4"



;; 4.5 - Itemizations


;; =================
;; Constants:

(define HEIGHT-V3 300) ; distances in pixels
(define WIDTH-V3  100)
(define YDELTA 3)

(define BACKG  (empty-scene WIDTH-V3 HEIGHT-V3))
(define ROCKET-V3 (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET-V3) 2))


;; =================
;; Data definitions:

;; Exercise 53

; A LR (short for launching rocket) is one of:
; - "resting"
; - NonnegativeNumber
; interpretation "resting" represents a grounded rocket
; a number denotes the height of a rocket in flight
(define LR1 "resting")
(define LR2 HEIGHT-V3)
(define LR3 0)

; An LRCD (for launching rocket countdown) is one of:
; - "resting"
; - a Number between -3 and -1
; - a NonnegativeNumber
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)


;; =================
;; Functions:

;; Exercise 54

; (string=? "resting" x) Error when x is a number

(check-expect (string? "resting") #true)
(check-expect (string? -2) #false)
(check-expect (string? 10) #false)

;; Exercise 55

; LRCD -> Image
; produces a rocket at height h
(define (draw h)
  (place-image ROCKET-V3 10 (- h CENTER) BACKG))

;; Exercise 56

; LRCD -> LRCD
; launches the program from some initial state (main.v3 "resting")
(define (main.v3 s)
  (big-bang s
            [on-tick   fly]
            [to-draw   show]
            [on-key    launch]
            [stop-when end?]))

; LRCD -> Image
; renders the state as a resting or flying rocket
(check-expect (show "resting") (draw HEIGHT-V3))
(check-expect (show -2) (place-image (text "-2" 20 "red")
                                     10 (* 3/4 WIDTH-V3)
                                     (draw HEIGHT-V3)))
(check-expect (show HEIGHT-V3) (draw HEIGHT-V3))
(check-expect (show 53) (draw 53))

(define (show x)
  (cond
    [(string? x)  (draw HEIGHT-V3)]
    [(<= -3 x -1) (place-image (text (number->string x) 20 "red")
                               10 (* 3/4 WIDTH-V3)
                               (draw HEIGHT-V3))]
    [(>= x 0) (draw x)]))

; LRCD KeyEvent -> LRCD
; starts the count-down when space bar is pressed,
; if the rocket is still resting
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already
(check-expect (fly "resting") "resting")
(check-expect (fly -3) -2)
(check-expect (fly -2) -1)
(check-expect (fly -1) HEIGHT-V3)
(check-expect (fly 10) (- 10 YDELTA))
(check-expect (fly 22) (- 22 YDELTA))

(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT-V3 (+ x 1))]
    [(>= x 0) (- x YDELTA)]))

; LRCD -> Boolean
; produces true if the rocket is out of sight
(check-expect (end? "resting") #false)
(check-expect (end? -3) #false)
(check-expect (end? -2) #false)
(check-expect (end? -1) #false)
(check-expect (end? 33) #false)
(check-expect (end? 0)  #true)

(define (end? x)
  (cond
    [(string? x)  #false]
    [(<= -3 x -1) #false]
    [(> x 0)      #false]
    [else         #true]))

;; Exercise 57


;; =================
;; Data definitions:

; A LRCD (for launching rocket count down) is one of:
; - "resting"
; - a Number between -3 and -1
; - a NonnegativeNumber
; interpretation a grounded rocket, in count-down mode,
; a number denotes the height in pixels of rocket at canvas


;; =================
;; Functions:

; LRCD -> LRCD
; launches the program from some initial state (main.v4 "resting")
(define (main.v4 s)
  (big-bang s
            [on-tick   fly.v4]
            [to-draw   show]
            [on-key    launch]
            [stop-when end?.v4]))

; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already
(check-expect (fly.v4 "resting") "resting")
(check-expect (fly.v4 -3) -2)
(check-expect (fly.v4 -2) -1)
(check-expect (fly.v4 -1) 0)
(check-expect (fly.v4 10) (+ 10 YDELTA))
(check-expect (fly.v4 22) (+ 22 YDELTA))

(define (fly.v4 x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> Boolean
; produces true if the rocket is out of sight
(check-expect (end?.v4 "resting") #false)
(check-expect (end?.v4 -3) #false)
(check-expect (end?.v4 -2) #false)
(check-expect (end?.v4 -1) #false)
(check-expect (end?.v4 33) #false)
(check-expect (end?.v4 HEIGHT-V3) #true)

(define (end?.v4 x)
  (cond
    [(string? x)     #false]
    [(<= -3 x -1)    #false]
    [(= x HEIGHT-V3) #true]
    [else            #false]))



;; 4.6 - Designing with Itemizations

;; Exercise 58

(define LOW-PRICE    1000)
(define LUXURY-PRICE 10000)


; A Price falls into one of three intervals:
; - 0 through LOW-PRICE
; - LOW-PRICE through LUXURY-PRICE
; - LUXURY-PRICE and above.
; interpretation the price of an item
(define P1 0)
(define P2 (/ LOW-PRICE 2))
(define P3 LOW-PRICE)
(define P4 (/ LUXURY-PRICE 2))
(define P5 LUXURY-PRICE)
(define P6 (* LUXURY-PRICE 1.2))


; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax P1) 0)
(check-expect (sales-tax P2) 0)
(check-expect (sales-tax P3) (* 0.05 P3))
(check-expect (sales-tax P4) (* 0.05 P4))
(check-expect (sales-tax P5) (* 0.08 P5))
(check-expect (sales-tax P6) (* 0.08 P6))

(define (sales-tax p)
  (cond
    [(and (<= 0 p)
          (< p LOW-PRICE)) 0]
    [(and (<= LOW-PRICE p)
          (< p LUXURY-PRICE)) (* 0.05 p)]
    [(>= p LUXURY-PRICE) (* 0.08 p)]))



;; 4.7 - Finite State Worlds

;; Exercise 59


;; =================
;; Constants:

(define BULB-SIZE 8)
(define SPACE (rectangle 5  2  "solid"   "white"))
(define BOARD (rectangle 80 30 "outline" "black"))
(define RED-STATE (overlay (beside SPACE
                                   (circle BULB-SIZE "solid"   "red")
                                   SPACE
                                   (circle BULB-SIZE "outline" "yellow")
                                   SPACE
                                   (circle BULB-SIZE "outline" "green")
                                   SPACE)
                           BOARD))
(define GREEN-STATE (overlay (beside SPACE
                                     (circle BULB-SIZE "outline" "red")
                                     SPACE
                                     (circle BULB-SIZE "outline" "yellow")
                                     SPACE
                                     (circle BULB-SIZE "solid"   "green")
                                     SPACE)
                             BOARD))
(define YELLOW-STATE (overlay (beside SPACE
                                      (circle BULB-SIZE "outline" "red")
                                      SPACE
                                      (circle BULB-SIZE "solid"   "yellow")
                                      SPACE
                                      (circle BULB-SIZE "outline" "green")
                                      SPACE)
                              BOARD))


;; =================
;; Data definitions:

; A TrafficLight is one of the following Strings:
; - "red"
; - "green"
; - "yellow"
; interpretation the three strings represent the three
; possible states that a traffic light may assume


;; =================
;; Functions:

; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
            [to-draw tl-render]
            [on-tick tl-next 1]))

; TrafficLight -> TrafficLight
; yields the next state given current state cs
(check-expect (tl-next "red")    "green")
(check-expect (tl-next "green")  "yellow")
(check-expect (tl-next "yellow") "red")

(define (tl-next cs)
  (cond [(string=? cs "red")    "green"]
        [(string=? cs "green")  "yellow"]
        [(string=? cs "yellow") "red"]))

; TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render "red")    RED-STATE)
(check-expect (tl-render "green")  GREEN-STATE)
(check-expect (tl-render "yellow") YELLOW-STATE)

(define (tl-render current-state)
  (cond [(string=? current-state "red")    RED-STATE]
        [(string=? current-state "green")  GREEN-STATE]
        [(string=? current-state "yellow") YELLOW-STATE]))

;; Exercise 60


;; =================
;; Data definitions:

; A N-TrafficLight is one of:
; - 0 interpretation the traffic light shows red
; - 1 interpretation the traffic light shows green
; - 2 interpretation the traffic light shows yellow


;; =================
;; Functions:

; N-TrafficLight -> N-TrafficLight
; simulates a clock-based American traffic light
(define (n-traffic-light-simulation initial-state)
  (big-bang initial-state
            [on-tick tl-next-numeric 1]
            [to-draw tl-render-numeric]))

; N-TrafficLight -> N-TrafficLight
; yields the next state given current state cs
(check-expect (tl-next-numeric 0) 1)
(check-expect (tl-next-numeric 1) 2)
(check-expect (tl-next-numeric 2) 0)

(define (tl-next-numeric cs) (modulo (+ cs 1) 3))

; N-TrafficLight -> Image
; renders the current state cs as an image
(check-expect (tl-render-numeric 0) RED-STATE)
(check-expect (tl-render-numeric 1) GREEN-STATE)
(check-expect (tl-render-numeric 2) YELLOW-STATE)

(define (tl-render-numeric current-state)
  (cond [(= current-state 0) RED-STATE]
        [(= current-state 1) GREEN-STATE]
        [(= current-state 2) YELLOW-STATE]))

;; Exercise 61


;; =================
;; Data definitions:

; A S-TrafficLight is one of:
; - RED
; - GREEN
; - YELLOW
;(define RED    0)
;(define GREEN  1)
;(define YELLOW 2)
(define RED    "red")
(define GREEN  "green")
(define YELLOW "yellow")


;; =================
;; Functions:

; S-TrafficLight -> S-TrafficLight
; yields the next state given current state cs
(check-expect (tl-next-symbolic RED) GREEN)
(check-expect (tl-next-symbolic YELLOW) RED)

;(define (tl-next-numeric cs)
;  (modulo (+ cs 1) 3))

(define (tl-next-symbolic cs)
  (cond
    [(equal? cs RED) GREEN]
    [(equal? cs GREEN) YELLOW]
    [(equal? cs YELLOW) RED]))

;; Exercise 62


;; =================
;; Data definitions:

; A DoorState is one of:
; - LOCKED
; -CLOSED
; -OPEN
(define LOCKED "locked")
(define CLOSED "closed")
(define OPEN   "open")


;; =================
;; Functions:

; DoorState -> DoorState
; simulates a door with an automatic door closer
(define (door-simulation initial-state)
  (big-bang initial-state
            [on-tick door-closer 3]
            [on-key  door-actions]
            [to-draw door-render]))

; DoorState -> DoorState
; closes an open door over the period of one tick
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN)   CLOSED)

(define (door-closer state-of-door)
  (cond
    [(string=? LOCKED state-of-door) LOCKED]
    [(string=? CLOSED state-of-door) CLOSED]
    [(string=? OPEN   state-of-door) CLOSED]))

; DoorState KeyEvent -> DoorState
; turn key event k into an action on state s
(check-expect (door-actions LOCKED "u") CLOSED)
(check-expect (door-actions CLOSED "u") CLOSED)
(check-expect (door-actions OPEN   "u") OPEN)
(check-expect (door-actions LOCKED "l") LOCKED)
(check-expect (door-actions CLOSED "l") LOCKED)
(check-expect (door-actions OPEN   "l") OPEN)
(check-expect (door-actions LOCKED " ") LOCKED)
(check-expect (door-actions CLOSED " ") OPEN)
(check-expect (door-actions OPEN   " ") OPEN)
(check-expect (door-actions LOCKED "a") LOCKED)
(check-expect (door-actions OPEN   "a") OPEN)
(check-expect (door-actions CLOSED "a") CLOSED)

(define (door-actions s k)
  (cond
    [(and (string=? LOCKED s) (string=? "u" k)) CLOSED]
    [(and (string=? CLOSED s) (string=? "l" k)) LOCKED]
    [(and (string=? CLOSED s) (string=? " " k)) OPEN]
    [else s]))

; DoorState -> Image
; translates the state s into a large text image
(check-expect (door-render CLOSED) (text CLOSED 40 "red"))

(define (door-render s)
  (text s 40 "red"))
