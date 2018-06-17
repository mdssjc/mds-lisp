;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3-How to Design Programs|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 3-How to Design Programs.rkt
;; I - Fixed-Size Data
;; 3 - How to Design Programs

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)


;; Exercise 33

"The Year 2000 problem is also known as the Y2K problem, the Millennium bug, the Y2K bug, or Y2K."



;; 3.1 - Designing Functions



;; 3.2 - Finger Exercises: Functions

;; Exercise 34

;; String -> String
;; extracts the first character from a non-empty string
(check-expect (string-first "Hello") "H")
(check-expect (string-first "apple") "a")

(define (string-first s)
  (substring s 0 1))

;; Exercise 35

;; String -> String
;; extracts the last character from a non-empty string
(check-expect (string-last "Hello") "o")
(check-expect (string-last "apple") "e")

(define (string-last s)
  (substring s (sub1 (string-length s))))

;; Exercise 36

;; Image -> Number
;; counts the number of pixels in a given image
(check-expect (image-area (square 100 "solid" "black")) (* 100 100))

(define (image-area i)
  (* (image-width i) (image-height i)))

;; Exercise 37

;; String -> String
;; produces a string like the given one with the first character removed
(check-expect (string-rest "Hello") "ello")
(check-expect (string-rest "apple") "pple")

(define (string-rest s)
  (substring s 1))

;; Exercise 38

;; String -> String
;; produces a string like the given one with the last character removed
(check-expect (string-remove-last "Hello") "Hell")
(check-expect (string-remove-last "apple") "appl")

(define (string-remove-last s)
  (substring s 0 (sub1 (string-length s))))



;; 3.3 - Domain Knowledge



;; 3.4 - From Functions to Programs



;; 3.5 - On Testing



;; 3.6 - Designing World Programs

;; Exercise 39
;; Exercise 40
;; Exercise 41
;; Exercise 42


;; =================
;; Constants:

(define WIDTH-OF-WORLD 200)

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 2))

(define WHEEL (circle WHEEL-RADIUS "solid" "black"))
(define SPACE (rectangle WHEEL-DISTANCE 0 "solid" "white"))
(define BOTH-WHEELS (beside WHEEL SPACE WHEEL))

(define BODY-A (rectangle (* WHEEL-RADIUS 8) (* WHEEL-RADIUS 3) "solid" "red"))
(define BODY-B (rectangle (* WHEEL-RADIUS 5) (* WHEEL-RADIUS 5) "solid" "red"))
(define CHASSIS (overlay/align "middle" "bottom" BODY-A BODY-B))

(define CAR (overlay/offset BOTH-WHEELS 0 (* (/ (image-height CHASSIS) 2) -1) CHASSIS))

(define TREE (underlay/xy (circle 10 "solid" "green")
                          9 15
                          (rectangle 2 20 "solid" "brown")))

(define HEIGHT (image-height CAR))

(define BACKGROUND (overlay/align
                    "right" "bottom"
                    TREE
                    (rectangle WIDTH-OF-WORLD HEIGHT "solid" "white")))
(define Y-CAR (/ HEIGHT 2))


;; =================
;; Data definitions:

; A WorldState is a Number.
; interpretation the number of pixels between
; the left border of the scene and the right-most edge of the car


;; =================
;; Functions:

; WorldState -> WorldState
; launches the program from some initial state
(define (main ws)
  (big-bang ws
            [on-tick   tock]
            [to-draw   render]
            [stop-when stop?]))

; WorldState -> WorldState
; moves the car by 3 pixels for every clock tick
(check-expect (tock 20) 23)
(check-expect (tock 78) 81)

(define (tock ws)
  (+ ws 3))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state
(check-expect (render 50)  (place-image CAR (+ 50  (image-width CAR)) Y-CAR BACKGROUND))
(check-expect (render 100) (place-image CAR (+ 100 (image-width CAR)) Y-CAR BACKGROUND))
(check-expect (render 150) (place-image CAR (+ 150 (image-width CAR)) Y-CAR BACKGROUND))
(check-expect (render 200) (place-image CAR (+ 200 (image-width CAR)) Y-CAR BACKGROUND))

(define (render ws)
  (place-image CAR (+ ws (image-width CAR)) Y-CAR BACKGROUND))

; WorldState -> Boolean
; stops the animation when the car has disappeared on the right side
(check-expect (stop? WIDTH-OF-WORLD)        #false)
(check-expect (stop? (add1 WIDTH-OF-WORLD)) #true)

(define (stop? ws)
  (> ws WIDTH-OF-WORLD))

;; Exercise 43


;; =================
;; Data definitions:

; An AnimationState is a Number
; interpretation the number of clock ticks
; since the animation started
(define AS1 10)


;; =================
;; Functions:

; AnimationState -> AnimationState
; launches the program from some initial state
(define (main.v2 as)
  (big-bang as
            [on-tick   tock.v2]
            [to-draw   render.v2]
            [stop-when stop.v2?]))

; AnimationState -> AnimationState
; moves the car by 3 pixels for every clock tick
(check-expect (tock.v2 20) 23)
(check-expect (tock.v2 78) 81)

(define (tock.v2 as)
  (+ as 3))

; AnimationState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state
(check-expect (render.v2 50)  (place-image CAR (+ 50  (image-width CAR)) (jump 50)  BACKGROUND))
(check-expect (render.v2 100) (place-image CAR (+ 100 (image-width CAR)) (jump 100) BACKGROUND))
(check-expect (render.v2 150) (place-image CAR (+ 150 (image-width CAR)) (jump 150) BACKGROUND))
(check-expect (render.v2 200) (place-image CAR (+ 200 (image-width CAR)) (jump 200) BACKGROUND))

(define (render.v2 as)
  (place-image CAR (+ as (image-width CAR)) (jump as) BACKGROUND))

;; AnimationState -> Number
;; produce the Y position of the car given an AnimationState
(check-within (jump 10) 9.55  0.01)
(check-within (jump 22) 14.91 0.01)

(define (jump as)
  (+ (/ HEIGHT 2) (* 10 (sin as))))

; AnimationState -> Boolean
; stops the animation when the car has disappeared on the right side
(check-expect (stop.v2? WIDTH-OF-WORLD)        #false)
(check-expect (stop.v2? (add1 WIDTH-OF-WORLD)) #true)

(define (stop.v2? as)
  (> as WIDTH-OF-WORLD))

;; Exercise 44


;; =================
;; Functions:

; AnimationState -> AnimationState
; launches the program from some initial state
(define (main.v3 as)
  (big-bang as
            [on-tick  tock.v2]
            [on-mouse hyper]
            [to-draw  render.v2]))

; AnimationState Number Number String -> AnimationState
; places the car at x-mouse
; if the given me is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") (- 10 (image-width CAR)))
(check-expect (hyper 42 10 20 "move") 42)

(define (hyper x-position-of-car x-mouse y-mouse me)
  (cond
    [(string=? "button-down" me) (- x-mouse (image-width CAR))]
    [else x-position-of-car]))



;; 3.7 - Virtual Pet Worlds

;; Exercise 45


;; =================
;; Constants:

(define CAT1 (circle 10 "solid" "brown"))
(define SPEED 3)
(define WIDTH-V4  400)
(define HEIGHT-V4 400)
(define Y-CAT (/ HEIGHT-V4 2))
(define BACKGROUND-V4 (empty-scene WIDTH-V4 HEIGHT-V4))


;; =================
;; Functions:

; VirtualCat -> VirtualCat
; launches the program from some initial state (cat-prog 0)
(define (cat-prog vc)
  (big-bang vc
            [on-tick tock.v4]
            [to-draw render.v4]))

; VirtualCat -> VirtualCat
; moves the CAT by SPEED for every clock tick,
; reset when the cat disappears on the WIDTH
(check-expect (tock.v4 0) SPEED)
(check-expect (tock.v4 5) 8)
(check-expect (tock.v4 WIDTH-V4) 3)
(check-expect (tock.v4 (- WIDTH-V4 SPEED)) WIDTH-V4)

(define (tock.v4 vc)
  (+ (modulo vc WIDTH-V4) SPEED))

; VirtualCat -> Image
; places the CAT into the BACKGROUND scene,
; according to the given world state
(define (render.v4 vc)
  (place-image CAT1 vc Y-CAT BACKGROUND-V4))

;; Exercise 46


;; =================
;; Constants:

(define CAT2 (circle 10 "solid" "olive"))


;; =================
;; Functions:

; VirtualCat -> VirtualCat
; launches the program from some initial state (cat-prog.v5 0)
(define (cat-prog.v5 vc)
  (big-bang vc
            [on-tick tock.v4]
            [to-draw render.v5]))

; VirtualCat -> Image
; places the CAT into the BACKGROUND scene,
; according to the given world state
; when odd is CAT1, else CAT2
(define (render.v5 vc)
  (place-image (cond [(odd? vc) CAT1]
                     [else      CAT2]) vc Y-CAT BACKGROUND-V4))

;; Exercise 47


;; =================
;; Constants:

(define INC-DOWN-KEY 1/5)
(define INC-UP-KEY   1/3)
(define DEC -0.1)
(define MAXIMUM   100)
(define MINIMUM   0)
(define WIDTH-V6  400)
(define HEIGHT-V6 200)
(define BACKGROUND-V6 (rectangle WIDTH-V6 HEIGHT-V6 "solid" "black"))


;; =================
;; Functions:

; VirtualCat -> VirtualCat
; launches the program from some initial state (gauge-prog 100)
(define (gauge-prog vc)
  (big-bang vc
            [on-tick tock.v6]
            [to-draw render.v6]
            [on-key  increase]))

; VirtualCat -> VirtualCat
; decreases by DEC for every clock tick
(check-expect (tock.v6 0)   MINIMUM)
(check-expect (tock.v6 0.1) MINIMUM)
(check-expect (tock.v6 0.3) (+ 0.3 DEC))

(define (tock.v6 vc)
  (cond [(<=  (+ vc DEC) MINIMUM) MINIMUM]
        [else (+ vc DEC)]))

; VirtualCat -> Image
; places the gauge into the scene, according to the given world state
(define (render.v6 vc)
  (place-image/align (rectangle (/ (* vc WIDTH-V6) 100) HEIGHT-V6 "solid" "red")
                     0 (/ HEIGHT-V6 2)
                     "left" "center"
                     BACKGROUND-V6))

; VirtualCat KeyEvent -> VirtualCat
; produces a increase for: up is 1/3 and down is 1/5
(check-expect (increase 10 "up")   (+ 10 (* 10 1/3)))
(check-expect (increase 10 "left") 10)
(check-expect (increase 10 "down") (+ 10 (* 10 1/5)))
(check-expect (increase 99 "up")   100)
(check-expect (increase MINIMUM "up")   1)
(check-expect (increase MINIMUM "down") 1)

(define (increase vc ke)
  (cond [(and (or (key=? ke "up")
                  (key=? ke "down"))
              (= vc MINIMUM)) 1]
        [(key=? ke "up")   (if (> (+ vc (* vc 1/3)) MAXIMUM)
                               MAXIMUM
                               (+ vc (* vc 1/3)))]
        [(key=? ke "down") (if (> (+ vc (* vc 1/5)) MAXIMUM)
                               MAXIMUM
                               (+ vc (* vc 1/5)))]
        [else vc]))
