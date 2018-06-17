;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |6-Itemizations and Structures|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 6-Itemizations and Structures.rkt
;; I - Fixed-Size Data
;; 6 Itemizations and Structures

(require 2htdp/image)
(require 2htdp/universe)


;; 6.1 - Designing with Itemizations, Again

;; Exercise 94
;; Exercise 95

;; The examples represent three states, being the movement of the Tank with its
;; aim, the firing of the missile and the collision with the UFO.

;; Exercise 96

;; Paper sketch of the three examples:
;; - aim: render the UFO and Tank;
;; - fired 1: render the UFO, Tank and Missile; and
;; - fired 2: render the Tank and collision between the UFO and the Missile.

;; Exercise 97
;; Exercise 98
;; Exercise 99
;; Exercise 100


;; =================
;; Constants:

(define UFO (overlay (rectangle 70 10 "solid" "green")
                     (circle 20 "solid" "green")))
(define UFO-X (/ (image-width  UFO) 2))
(define UFO-Y (/ (image-height UFO) 2))
(define TANK-HEIGHT 20)
(define TANK    (rectangle 50 TANK-HEIGHT "solid" "blue"))
(define MISSILE (triangle 10 "solid" "red"))
(define WIDTH  400)
(define HEIGHT 300)
(define CLOSE (/ HEIGHT 3))
(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:

; A UFO is a Posn.
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number).
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick

; A Missile is a Posn.
; interpretation (make-posn x y) is the missile's place

(define-struct aim   [ufo tank])
(define-struct fired [ufo tank missile])
; A SIGS is one of:
; - (make-aim UFO Tank)
; - (make-fired UFO Tank Missile)
; interpretation represents the complete state of a
; space invader game
(define S1 (make-aim   (make-posn 20 10) (make-tank 28 -3)))
(define S2 (make-fired (make-posn 20 10)
                       (make-tank 28 -3)
                       (make-posn 28 (- HEIGHT TANK-HEIGHT))))
(define S3 (make-fired (make-posn 20 100)
                       (make-tank 100 3)
                       (make-posn 22 103)))
(define S4 (make-aim   (make-posn 10 (- HEIGHT CLOSE))
                       (make-tank 28 -3)))


;; =================
;; Functions:

; SIGS -> World
; starts a world with (main S1)
(define (main s)
  (big-bang s
            (on-tick   si-move)
            (on-draw   si-render)
            (on-key    si-control)
            (stop-when si-game-over?)))

; SIGS -> Image
; renders the given game state on top of BACKGROUND
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKGROUND))]
    [(fired? s)
     (tank-render
      (fired-tank s)
      (ufo-render (fired-ufo s)
                  (missile-render (fired-missile s)
                                  BACKGROUND)))]))

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) HEIGHT im))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Missile Image -> Image
; adds m to the given image im
(define (missile-render m im)
  (place-image MISSILE (posn-x m) (posn-y m) im))

; Tests
(define T1 (tank-render
            (fired-tank S2)
            (ufo-render (fired-ufo S2)
                        (missile-render (fired-missile S2)
                                        BACKGROUND))))
(define T2 (ufo-render
            (fired-ufo S2)
            (tank-render (fired-tank S2)
                         (missile-render (fired-missile S2)
                                         BACKGROUND))))
(check-expect T1 T2)

; SIGS -> Boolean
; returns true when the game stop;
; the game stops if the UFO lands or if the missile hits the UFO
(check-expect (si-game-over? S1) #false)
(check-expect (si-game-over? S2) #false)
(check-expect (si-game-over? S3) #true)
(check-expect (si-game-over? S4) #true)

(define (si-game-over? s)
  (cond
    [(aim? s)
     (cond
       [(>= (posn-y (aim-ufo s)) (- HEIGHT CLOSE)) #true]
       [else #false])]
    [(fired? s)
     (cond
       [(>= (posn-y (fired-ufo s)) (- HEIGHT CLOSE)) #true]
       [(and (<= (- (posn-x (fired-ufo s)) UFO-X)
                 (posn-x (fired-missile s))
                 (+ (posn-x (fired-ufo s)) UFO-X))
             (<= (- (posn-y (fired-ufo s)) UFO-Y)
                 (posn-y (fired-missile s))
                 (+ (posn-y (fired-ufo s)) UFO-Y))) #true]
       [else #false])]
    [else #false]))

; SIGS -> Image
; renders the final state of the game
(define (si-render-final s)
  (si-render s))

; SIGS -> SIGS
; updates the position of objects
(check-random (si-move S1)
              (make-aim
               (make-posn (random (posn-x (aim-ufo S1))) 11)
               (make-tank 25 -3)))
(check-random (si-move S3)
              (make-fired
               (make-posn (random (posn-x (fired-ufo S3))) 101)
               (make-tank 103 3)
               (make-posn 22  102)))

(define (si-move s)
  (si-move-proper s (random (cond [(aim?   s) (posn-x (aim-ufo   s))]
                                  [(fired? s) (posn-x (fired-ufo s))]))))

; Number -> Number
; produces a number in the interval [0,n),
; possibly a different one each time it is called
; (define (random n) ...)

; SIGS Number -> SIGS
; moves the space-invader objects predictably by delta
(define (si-move-proper s delta)
  (cond [(aim?   s) (make-aim   (update-ufo  (aim-ufo s) delta)
                                (update-tank (aim-tank s)))]
        [(fired? s) (make-fired (update-ufo  (fired-ufo s) delta)
                                (update-tank (fired-tank s))
                                (update-missile (fired-missile s)))]))

; UFO Number -> UFO
; updates the UFO by a value
(check-expect (update-ufo (make-posn 1 1) 2) (make-posn 2 2))
(check-expect (update-ufo (make-posn 0 1) 0) (make-posn 1 2))

(define (update-ufo u v)
  (make-posn (if (= v 0) 1 v) (add1 (posn-y u))))

; Tank -> Tank
; updates the Tank
(check-expect (update-tank (make-tank 0 -1))    (make-tank 0 -1))
(check-expect (update-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))
(check-expect (update-tank (make-tank 5 -1))    (make-tank 4 -1))
(check-expect (update-tank (make-tank 5  1))    (make-tank 6 1))

(define (update-tank t)
  (make-tank (cond [(<   (+ (tank-loc t) (tank-vel t)) 0) 0]
                   [(>   (+ (tank-loc t) (tank-vel t)) WIDTH) WIDTH]
                   [else (+ (tank-loc t) (tank-vel t))])
             (tank-vel t)))

; Missile -> Missile
; updates the missile
(check-expect (update-missile (make-posn 1 1)) (make-posn 1 0))

(define (update-missile m)
  (make-posn (posn-x m) (sub1 (posn-y m))))

; SIGS KeyEvent -> SIGS
; handles the main events:
; - pressing the left arrow ensures that the tank moves left;
; - pressing the right arrow ensures that the tank moves right; and
; - pressing the space bar fires the missile if it hasn’t been launched yet.
(check-expect (si-control S1 "left")
              (make-aim (aim-ufo S1)
                        (create-tank (aim-tank S1) "left")))
(check-expect (si-control S1 "right")
              (make-aim (aim-ufo S1)
                        (create-tank (aim-tank S1) "right")))
(check-expect (si-control S1 " ")
              (make-fired (aim-ufo S1)
                          (aim-tank S1)
                          (make-posn (tank-loc (aim-tank S1))
                                     (- HEIGHT TANK-HEIGHT))))
(check-expect (si-control S1 "a") S1)
(check-expect (si-control S2 "left")
              (make-fired (fired-ufo S2)
                          (create-tank (fired-tank S2) "left")
                          (fired-missile S2)))
(check-expect (si-control S2 "right")
              (make-fired (fired-ufo S2)
                          (create-tank (fired-tank S2) "right")
                          (fired-missile S2)))
(check-expect (si-control S2 " ")
              (make-fired (fired-ufo  S2)
                          (fired-tank S2)
                          (make-posn (tank-loc (fired-tank S2))
                                     (- HEIGHT TANK-HEIGHT))))
(check-expect (si-control S2 "a") S2)

(define (si-control s ke)
  (cond [(or (key=? ke "left")
             (key=? ke "right"))
         (cond [(aim? s)
                (make-aim (aim-ufo s)
                          (create-tank (aim-tank s) ke))]
               [(fired? s)
                (make-fired (fired-ufo s)
                            (create-tank (fired-tank s) ke)
                            (fired-missile s))])]
        [(key=? ke " ")
         (cond [(aim? s)
                (make-fired (aim-ufo  s)
                            (aim-tank s)
                            (make-posn (tank-loc (aim-tank s))
                                       (- HEIGHT TANK-HEIGHT)))]
               [(fired? s)
                (make-fired (fired-ufo  s)
                            (fired-tank s)
                            (make-posn (tank-loc (fired-tank s))
                                       (- HEIGHT TANK-HEIGHT)))])]
        [else s]))

; Tank -> Tank
; produces a tank from t with the speed direction dir
(check-expect (create-tank (make-tank 100 3)  "left")  (make-tank 100 -3))
(check-expect (create-tank (make-tank 100 -3) "left")  (make-tank 100 -3))
(check-expect (create-tank (make-tank 100 3)  "right") (make-tank 100 3))
(check-expect (create-tank (make-tank 100 -3) "right") (make-tank 100 3))

(define (create-tank t dir)
  (make-tank (tank-loc t)
             (* (tank-vel t)
                (cond [(string=? dir "left")
                       (if (negative? (tank-vel t)) 1 -1)]
                      [(string=? dir "right")
                       (if (negative? (tank-vel t)) -1 1)]))))


;; Exercise 101
;; Exercise 102


;; =================
;; Data definitions:

; A MissileOrNot is one of:
; - #false
; - Posn
; interpretation #false means the missile is in the tank;
;                Posn says the missile is at that location

(define-struct sigs [ufo tank missile])
; A SIGS.v2 (short for SIGS version 2) is a structure:
;   (make-sigs UFO Tank MissileOrNot)
; interpretation represents the complete state of a
; space invader game
(define S5 (make-sigs (make-posn 20 10)
                      (make-tank 28 -3)
                      #false))
(define S6 (make-sigs (make-posn 20 10)
                      (make-tank 28 -3)
                      (make-posn 32 (- HEIGHT TANK-HEIGHT 10))))
(define S7 (make-sigs (make-posn 20 100)
                      (make-tank 100 3)
                      (make-posn 22 103)))
(define S8 (make-sigs (make-posn 10 (- HEIGHT CLOSE))
                      (make-tank 28 -3)
                      #false))


;; =================
;; Functions:

; SIGS.v2 -> World
; starts a world with (main S1)
(define (main.v2 s)
  (big-bang s
            (on-tick   si-move.v2)
            (on-draw   si-render.v2)
            (on-key    si-control.v2)
            (stop-when si-game-over?.v2)))

; SIGS.v2 -> SIGS.v2
; updates the position of objects
(check-random (si-move.v2 S5)
              (make-sigs
               (make-posn (random (+ (posn-x (sigs-ufo S5)) (image-width UFO))) 11)
               (make-tank 25 -3)
               #false))
(check-random (si-move.v2 S6)
              (make-sigs
               (make-posn (random (+ (posn-x (sigs-ufo S6)) (image-width UFO))) 11)
               (make-tank 25 -3)
               (make-posn 32 (sub1 (- HEIGHT TANK-HEIGHT 10)))))

(define (si-move.v2 s)
  (si-move-proper.v2 s (random (+ (posn-x (sigs-ufo s))
                               (image-width  UFO)))))

; SIGS.v2 Number -> SIGS.v2
; moves the space-invader objects predictably by delta
(define (si-move-proper.v2 s delta)
  (make-sigs (update-ufo  (sigs-ufo  s) delta)
             (update-tank (sigs-tank s))
             (update-missile.v2 (sigs-missile s))))

; Missile -> Missile
; updates the missile
(define (update-missile.v2 m)
  (cond [(boolean? m) m]
        [(posn?    m) (make-posn (posn-x m) (sub1 (posn-y m)))]))

; SIGS.v2 -> Image
; renders the given game state on top of BACKGROUND
(check-expect (si-render.v2 S5)
              (place-image UFO 20 10
                           (place-image TANK 28 HEIGHT
                                        BACKGROUND)))
(check-expect (si-render.v2 S6)
              (place-image UFO 20 10
                           (place-image TANK 28 HEIGHT
                                        (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT 10)
                                                     BACKGROUND))))

(define (si-render.v2 s)
  (tank-render
   (sigs-tank s)
   (ufo-render (sigs-ufo s)
               (missile-render.v2 (sigs-missile s)
                                  BACKGROUND))))

; MissileOrNot Image -> Image
; adds an image of missile m to scene s
(define (missile-render.v2 m s)
  (cond [(boolean? m) s]
        [(posn?    m) (place-image MISSILE (posn-x m) (posn-y m) s)]))

; SIGS.v2 KeyEvent -> SIGS.v2
; handles the main events:
; - pressing the left arrow ensures that the tank moves left;
; - pressing the right arrow ensures that the tank moves right; and
; - pressing the space bar fires the missile if it hasn’t been launched yet.
(check-expect (si-control.v2 S5 "left")
              (make-sigs (sigs-ufo S5)
                         (create-tank (sigs-tank S5) "left")
                         (sigs-missile S5)))
(check-expect (si-control.v2 S5 "right")
              (make-sigs (sigs-ufo S5)
                         (create-tank (sigs-tank S5) "right")
                         (sigs-missile S5)))
(check-expect (si-control.v2 S5 " ")
              (make-sigs (sigs-ufo S5)
                         (sigs-tank S5)
                         (make-posn (tank-loc (sigs-tank S5))
                                    (- HEIGHT TANK-HEIGHT))))
(check-expect (si-control.v2 S5 "a") S5)
(check-expect (si-control.v2 S6 "left")
              (make-sigs (sigs-ufo S6)
                         (create-tank (sigs-tank S6) "left")
                         (sigs-missile S6)))
(check-expect (si-control.v2 S6 "right")
              (make-sigs (sigs-ufo S6)
                         (create-tank (sigs-tank S6) "right")
                         (sigs-missile S6)))
(check-expect (si-control.v2 S6 " ")
              (make-sigs (sigs-ufo  S6)
                         (sigs-tank S6)
                         (make-posn (tank-loc (sigs-tank S6))
                                    (- HEIGHT TANK-HEIGHT))))
(check-expect (si-control.v2 S6 "a") S6)

(define (si-control.v2 s ke)
  (cond [(or (key=? ke "left")
             (key=? ke "right"))
         (make-sigs (sigs-ufo s)
                    (create-tank (sigs-tank s) ke)
                    (sigs-missile s))]
        [(key=? ke " ")
         (make-sigs (sigs-ufo s)
                    (sigs-tank s)
                    (make-posn (tank-loc (sigs-tank s))
                               (- HEIGHT TANK-HEIGHT)))]
        [else s]))

; SIGS.v2 -> Boolean
; returns true when the game stop;
; the game stops if the UFO lands or if the missile hits the UFO
(check-expect (si-game-over?.v2 S5) #false)
(check-expect (si-game-over?.v2 S6) #false)
(check-expect (si-game-over?.v2 S7) #true)
(check-expect (si-game-over?.v2 S8) #true)

(define (si-game-over?.v2 s)
  (cond
    [(>= (posn-y (sigs-ufo s)) (- HEIGHT CLOSE)) #true]
    [(and (posn? (sigs-missile s))
          (<= (- (posn-x (sigs-ufo s)) UFO-X)
              (posn-x (sigs-missile s))
              (+ (posn-x (sigs-ufo s)) UFO-X))
          (<= (- (posn-y (sigs-ufo s)) UFO-Y)
              (posn-y (sigs-missile s))
              (+ (posn-y (sigs-ufo s)) UFO-Y))) #true]
    [else #false]))


;; Exercise 103

(define-struct spider (legs space))
; Spider is a structure:
; interpretation (make-spider Number Number)
;  legs means the total of legs remaining
;  space means the amount of space to transport
(define SP1 (make-spider 4 10))
(define SP2 (make-spider 8 20))

(define-struct elephant (space))
; Elephante is a structure:
; interpretation (make-elephant Number)
;  space means the amount of space to transport
(define E1 (make-elephant 10))
(define E2 (make-elephant 20))

(define-struct boa-constrictor (length girth))
; Boa-constrictor is a structure:
; interpretation (make-boa-constrictor Number(0..N] Number(0..N])
;  length means its horizontal size, between 0 and N
;  girth means its vertical size, between 0 and N
(define B1 (make-boa-constrictor 5 2))
(define B2 (make-boa-constrictor 5 4))

(define-struct armadillo (claws space))
; Armadillo is a structure:
; interpretation (make-armadillo Number[0..100] Number)
;  claws means the percentage of durability
;  space means the amount of space to transport
(define A1 (make-armadillo 100 10))
(define A2 (make-armadillo 100 20))

(define-struct my-zoo (spider elephant boa-constrictor armadillo))
; A Zoo is a structure:
; interpretation (make-my-zoo Spider Elephant Boa-constrictor Armadillo)
;  represents four kinds of zoo animals
(define Z1 (make-my-zoo SP1 E1 B1 A1))
(define Z2 (make-my-zoo SP2 E2 B2 A2))

#;
(define (fn-for-my-zoo z)
  (... (my-zoo-spider z)
       (my-zoo-elephant z)
       (my-zoo-boa-constrictor z)
       (my-zoo-armadillo z)))

; Zoo Number -> Boolean
; determines whether the cage is large enough for the animal
(check-expect (fits? Z1 50) #true)
(check-expect (fits? Z2 50) #false)

(define (fits? z v)
  (<= (+ (spider-space   (my-zoo-spider z))
         (elephant-space (my-zoo-elephant z))
         (* (boa-constrictor-length (my-zoo-boa-constrictor z))
            (boa-constrictor-girth  (my-zoo-boa-constrictor z)))
         (armadillo-space (my-zoo-armadillo z)))
      v))


;; Exercise 104

; Vehicle is one of:
; - "automobile"
; - "van"
; - "bus"
; - "SUV"
; - "truck"
; interpretation a vehicle of fleet

(define-struct fleet (vehicle passenger license fuel))
; Fleet is a structure:
; interpretation (make-fleet Vehicle Number Number Number)
;  vehicle means a vehicle type
;  passenger means the total number that it can carry
;  license means the value of plate number
;  fuel means the current value in miles per gallon

#;
(define (fn-to-fleet f)
  (... (fleet-vehicle f)
       (fleet-passenger f)
       (fleet-license f)
       (fleet-fuel f)))


;; Exercise 105

; A Coordinate is one of:
; - a NegativeNumber
; interpretation on the y axis, distance from top
; - a PositiveNumber
; interpretation on the x axis, distance from left
; - a Posn
; interpretation an ordinary Cartesian point
(define C1 -10)
(define C2 -20)
(define C3  10)
(define C4  20)
(define C5 (make-posn 10 10))
(define C6 (make-posn  0 50))



;; 6.2 - Mixing Up Worlds

;; Exercise 106


;; =================
;; Constants:

(define WIDTH-V6  400)
(define HEIGHT-V6 400)

(define CAT (circle 6 "solid" "brown"))
(define OFFSET-CAT (/ (image-width CAT) 2))

(define CHAM (circle 6 "solid" "red"))
(define OFFSET-CHAM  (/ (image-width CHAM) 2))

(define Y-VA (/ HEIGHT-V6 2))
(define SPEED 3)

(define GAUGE-WIDTH  (/ WIDTH-V6  3))
(define GAUGE-HEIGHT (/ HEIGHT-V6 40))
(define MIN 0)
(define MAX 100)
(define DEC -0.1)
(define INC-DOWN 1/5)
(define INC-UP   1/3)

(define BACKGROUND-V6 (empty-scene WIDTH-V6 HEIGHT-V6))


;; =================
;; Data definitions:

(define-struct vcat (x h))
; A VCat is a structure:
;   (make-vcat Number Number)
; interpretation (make-vcat x h) describes a virtual cat
; where x means x-coordinate
; and   h means your happiness

(define-struct vcham (x h c))
; A VCham is a structure:
;   (make-vcham Number Number String)
; interpretation (make-vcham x h c) describes a virtual cham
;   where x means x-coordinate
;   and   h means your happiness
;   and   c means your color

; A VAnimal is either
; - a VCat
; - a VCham


;; =================
;; Functions:

; VAnimal -> World
; starts a world with (cat-cham (make-vcat 0 100)) or (cat-cham (make-vcham 0 100 "red"))
(define (cat-cham va)
  (big-bang va
            [on-tick tock]
            [to-draw render]
            [on-key  interact]))

; VAnimal -> VAnimal
; moves the virtual animal by SPEED pixels and
; decreases the happiness by DEC for every clock tick,
; reset when the virtual animal disappears on the right
(define (tock va)
  (cond [(vcat? va)
         (make-vcat (do-x-coordinate va OFFSET-CAT)
                    (do-happiness va))]
        [(vcham? va)
         (make-vcham (do-x-coordinate va OFFSET-CHAM)
                     (do-happiness va)
                     (vcham-c va))]))

; VAnimal Number -> Number
; updates the x-coordinate
(define (do-x-coordinate x offset)
  (if (>= (+ (va-x-coordinate x) SPEED)
          (+ WIDTH-V6 offset))
      0
      (+ (va-x-coordinate x) SPEED)))

; VAnimal Number -> Number
; updates the happiness
(define (do-happiness h)
  (if (<= (+ (va-happiness h) DEC) MIN)
      MIN
      (+ (va-happiness h) DEC)))

; VAnimal -> Image
; places the virtual animal into the BACKGROUND scene
(define (render va)
  (place-image (cond [(vcat?  va) CAT]
                     [(vcham? va) (circle 6 "solid" (vcham-c va))])
               (va-x-coordinate va) Y-VA
               (overlay/align "middle" "top"
                              (rectangle (* (va-happiness va) GAUGE-WIDTH .01)
                                         GAUGE-HEIGHT
                                         "solid" "red")
                              BACKGROUND-V6)))

; VAnimal KeyEvent -> VAnimal
; interacts with the virtual animal:
;  happiness: up is 1/3 and down is 1/5
;  color: r is "red", g is "green" and b is "blue"
(define (interact va ke)
  (cond [(vcat? va)
         (make-vcat (vcat-x va)
                    (cond [(key=? ke "up")   (calculate (vcat-h va) INC-UP)]
                          [(key=? ke "down") (calculate (vcat-h va) INC-DOWN)]
                          [else (vcat-h va)]))]
        [(vcham? va)
         (make-vcham (vcham-x va)
                     (cond [(key=? ke "up")   (calculate (vcham-h va) INC-UP)]
                           [(key=? ke "down") (calculate (vcham-h va) INC-DOWN)]
                           [else (vcham-h va)])
                     (cond [(key=? ke "r") "red"]
                           [(key=? ke "g") "green"]
                           [(key=? ke "b") "blue"]
                           [else (vcham-c va)]))]))

; Number -> Number
; help function for increase x by n, limited by MAX
(define (calculate x n)
  (cond [(= x MIN) 1]
        [(> (+ x (* x n)) MAX) MAX]
        [else (+ x (* x n))]))

; VAnimal -> Number
; returns the x-coordinate
(define (va-x-coordinate va)
  (cond [(vcat?  va) (vcat-x  va)]
        [(vcham? va) (vcham-x va)]))

; VAnimal -> Number
; returns the happiness
(define (va-happiness va)
  (cond [(vcat?  va) (vcat-h  va)]
        [(vcham? va) (vcham-h va)]))

;; Exercise 107


;; =================
;; Data definitions:

(define-struct zoo (cat cham focus))
; A Zoo is a structure:
; interpretation (make-zoo VCat VCham String)
;  cat means a VCat type
;  cham means a VCham type
;  focus means the selected animal, "k" for the Cat and "l" for the Cham
(define ZOO1 (make-zoo (make-vcat  50 100)
                       (make-vcham 10 100 "red")
                       "k"))


;; =================
;; Functions:

; Zoo -> World
; starts a world with (cham-and-cat Z1)
(define (cham-and-cat z)
  (big-bang z
            [on-tick tock.v7]
            [to-draw render.v7]
            [on-key  interact.v7]))

; Zoo -> Zoo
; moves the virtual animals by SPEED pixels and
; decreases the happiness by DEC for every clock tick,
; reset when the virtual animals disappears on the right
(define (tock.v7 z)
  (make-zoo (make-vcat
             (do-x-coordinate (zoo-cat z) OFFSET-CAT)
             (do-happiness (zoo-cat z)))
            (make-vcham
             (do-x-coordinate (zoo-cham z) OFFSET-CHAM)
             (do-happiness (zoo-cham z))
             (vcham-c (zoo-cham z)))
            (zoo-focus z)))

; Zoo -> Image
; places the virtual animals into the BACKGROUND scene
(define (render.v7 z)
  (place-image CAT
               (va-x-coordinate (zoo-cat z)) Y-VA
               (place-image (circle 6 "solid" (vcham-c (zoo-cham z)))
                            (va-x-coordinate (zoo-cham z)) Y-VA
                            (overlay/align "left" "top"
                                           (rectangle (* (va-happiness (zoo-cat z)) GAUGE-WIDTH .01)
                                                      GAUGE-HEIGHT
                                                      "solid" "red")
                                           (overlay/align "right" "top"
                                                          (rectangle (* (va-happiness (zoo-cham z)) GAUGE-WIDTH .01)
                                                                     GAUGE-HEIGHT
                                                                     "solid" "red")
                                                          BACKGROUND-V6)))))

; Zoo KeyEvent -> Zoo
; interacts with virtual animals:
;  happiness: up is 1/3 and down is 1/5
;  color: r is "red", g is "green" and b is "blue"
;  focus: k is "Cat" and l is "Cham"
(define (interact.v7 z ke)
  (cond
    [(key=? ke "k") (make-zoo (zoo-cat z) (zoo-cham z) "k")]
    [(key=? ke "l") (make-zoo (zoo-cat z) (zoo-cham z) "l")]
    [(string=? (zoo-focus z) "k")
     (make-zoo
      (make-vcat (vcat-x (zoo-cat z))
                 (cond [(key=? ke "up")   (calculate (vcat-h (zoo-cat z)) INC-UP)]
                       [(key=? ke "down") (calculate (vcat-h (zoo-cat z)) INC-DOWN)]
                       [else (vcat-h (zoo-cat z))]))
      (zoo-cham z)
      (zoo-focus z))]
    [(string=? (zoo-focus z) "l")
     (make-zoo
      (zoo-cat z)
      (make-vcham (vcham-x (zoo-cham z))
                  (cond [(key=? ke "up")   (calculate (vcham-h (zoo-cham z)) INC-UP)]
                        [(key=? ke "down") (calculate (vcham-h (zoo-cham z)) INC-DOWN)]
                        [else (vcham-h (zoo-cham z))])
                  (cond [(key=? ke "r") "red"]
                        [(key=? ke "g") "green"]
                        [(key=? ke "b") "blue"]
                        [else (vcham-c (zoo-cham z))]))
      (zoo-focus z))]))

;; Exercise 108


;; =================
;; Constants:

(define SIZE 10)
(define RED   (circle SIZE "solid" "red"))
(define GREEN (circle SIZE "solid" "green"))

(define COUNT-RED   40)
(define COUNT-GREEN 20)

(define WIDTH-V8  40)
(define HEIGHT-V8 WIDTH-V8)
(define CENTER (/ WIDTH-V8 2))
(define BACKGROUND-V8 (rectangle WIDTH-V8 HEIGHT-V8 "solid" "black"))


;; =================
;; Data definitions:

(define-struct traffic-light (state count-down))
; Traffic-Light is a structure:
; interpretation (make-traffic-light s c)
;  - s means the current state: standing or walking
;  - c means the count-down
(define TL1 (make-traffic-light "standing" COUNT-RED))
(define TL2 (make-traffic-light "standing" 20))
(define TL3 (make-traffic-light "standing" 0))
(define TL4 (make-traffic-light "walking"  COUNT-GREEN))
(define TL5 (make-traffic-light "walking"  10))
(define TL6 (make-traffic-light "walking"  9))
(define TL7 (make-traffic-light "walking"  0))


;; =================
;; Functions:

; Traffic-Light -> World
; starts the world with (main.v8 TL1)
(define (main.v8 tl)
  (big-bang tl
            (on-tick tock.v8 1)
            (on-draw render.v8)
            (on-key  reset)))

; Traffic-Light -> Traffic-Light
; updates the states and count-down values
(check-expect (tock.v8 TL1) (make-traffic-light "standing" (sub1 COUNT-RED)))
(check-expect (tock.v8 TL2) (make-traffic-light "standing" 19))
(check-expect (tock.v8 TL3) (make-traffic-light "walking"  COUNT-GREEN))
(check-expect (tock.v8 TL4) (make-traffic-light "walking"  (sub1 COUNT-GREEN)))
(check-expect (tock.v8 TL5) (make-traffic-light "walking"  9))
(check-expect (tock.v8 TL6) (make-traffic-light "walking"  8))
(check-expect (tock.v8 TL7) (make-traffic-light "standing" COUNT-RED))

(define (tock.v8 tl)
  (cond [(string=? (traffic-light-state tl) "standing")
         (if (> (traffic-light-count-down tl) 0)
             (make-traffic-light "standing" (sub1 (traffic-light-count-down tl)))
             (make-traffic-light "walking" COUNT-GREEN))]
        [(string=? (traffic-light-state tl) "walking")
         (if (> (traffic-light-count-down tl) 0)
             (make-traffic-light "walking" (sub1 (traffic-light-count-down tl)))
             (make-traffic-light "standing" COUNT-RED))]))

; Traffic-Light -> Image
; renders the image of Traffic Light into the BACKGROUND scene
(check-expect (render.v8 TL1) (place-image RED   CENTER CENTER BACKGROUND-V8))
(check-expect (render.v8 TL4) (place-image GREEN CENTER CENTER BACKGROUND-V8))
(check-expect (render.v8 TL5) (overlay/align "middle" "middle"
                                          (text "10" SIZE "red")
                                          GREEN BACKGROUND-V8))
(check-expect (render.v8 TL6) (overlay/align "middle" "middle"
                                          (text "9" SIZE "darkgreen")
                                          GREEN BACKGROUND-V8))
(check-expect (render.v8 TL7) (place-image GREEN CENTER CENTER BACKGROUND-V8))

(define (render.v8 tl)
  (cond [(string=? (traffic-light-state tl) "standing")
         (place-image RED CENTER CENTER BACKGROUND-V8)]
        [(string=? (traffic-light-state tl) "walking")
         (if (< 0 (traffic-light-count-down tl) 11)
             (overlay/align "middle" "middle"
                            (text (number->string (traffic-light-count-down tl)) SIZE
                                  (if (even? (traffic-light-count-down tl))
                                      "red"
                                      "darkgreen"))
                                  GREEN BACKGROUND-V8)
             (place-image GREEN CENTER CENTER BACKGROUND-V8))]))

; Traffic-Light KeyEvent -> Traffic-Light
; resets the default state
(check-expect (reset TL2 " ")    TL1)
(check-expect (reset TL2 "left") TL2)

(define (reset tl ke)
  (if (key=? ke " ") TL1 tl))

;; Exercise 109

;; =================
;; Constants:

(define WIDTH-V9  100)
(define HEIGHT-V9 WIDTH-V9)
(define BACKGROUND-V9 (rectangle WIDTH-V9 HEIGHT-V9 "solid" "white"))


;; =================
;; Data definitions:

; ExpectsToSee is one of:
; - "start, expect an 'a'"
; - "expect 'b', 'c', or 'd'"
; - "finished"
; - "error, illegal key"


;; =================
;; Functions:

; ExpectsToSee -> World
; starts the world with (main.v9 "start")
(define (main.v9 ets)
  (big-bang ets
            (on-draw render.v9)
            (on-key  event)))

; ExpectsToSee -> Image
; renders the current event into the BACKGROUND scene
(check-expect (render.v9 "start")    (rectangle WIDTH-V9 HEIGHT-V9 "solid" "white"))
(check-expect (render.v9 "expect")   (rectangle WIDTH-V9 HEIGHT-V9 "solid" "yellow"))
(check-expect (render.v9 "finished") (rectangle WIDTH-V9 HEIGHT-V9 "solid" "green"))
(check-expect (render.v9 "error")    (rectangle WIDTH-V9 HEIGHT-V9 "solid" "red"))

(define (render.v9 ets)
  (overlay (rectangle WIDTH-V9 HEIGHT-V9 "solid"
                      (cond [(string=? "start"    ets) "white"]
                            [(string=? "expect"   ets) "yellow"]
                            [(string=? "finished" ets) "green"]
                            [(string=? "error"    ets) "red"]))
           BACKGROUND-V9))

; ExpectsToSee KeyEvent -> ExpectsToSee
; generates the next event to ExpectsToSee
(check-expect (event "start"    "a") "expect")
(check-expect (event "start"    "b") "error")
(check-expect (event "expect"   "b") "expect")
(check-expect (event "expect"   "c") "expect")
(check-expect (event "expect"   "d") "finished")
(check-expect (event "expect"   "e") "error")
(check-expect (event "finished" "e") "finished")
(check-expect (event "error"    "e") "error")

(define (event ets ke)
  (cond [(and (string=? "start"  ets)
              (key=? "a" ke))
         "expect"]
        [(and (string=? "expect" ets)
              (or (key=? "b" ke)
                  (key=? "c" ke)))
         "expect"]
        [(and (string=? "expect" ets)
              (key=? "d" ke))
         "finished"]
        [(string=? "finished" ets)
         "finished"]
        [else "error"]))



;; 6.3 - Input Errors

;; Exercise 110


;; =================
;; Data definitions:

; Any BSL value is one of:
; - Number
; - Boolean
; - String
; - Image
; - (make-posn Any Any)
; ...
; - (make-tank Any Any)
; ...


;; =================
;; Functions:

; Number -> Number
; computes the area of a disk with radius r
(check-expect (area-of-disk 2)   (* 3.14 (* 2 2)))
(check-expect (area-of-disk 5.6) (* 3.14 (* 5.6 5.6)))

(define (area-of-disk r)
  (* 3.14 (* r r)))

; Any -> Number
; computes the area of a disk with radius v,
; if v is a positive number
(check-expect (checked-area-of-disk 2)     (* 3.14 (* 2 2)))
(check-error  (checked-area-of-disk "two") "area-of-disk: positive number expected")
(check-error  (checked-area-of-disk 0)     "area-of-disk: positive number expected")
(check-error  (checked-area-of-disk -1)    "area-of-disk: positive number expected")

(define (checked-area-of-disk v)
  (cond
    [(and (number? v)
          (> v 0))
     (area-of-disk v)]
    [else (error "area-of-disk: positive number expected")]))

;; Exercise 111


;; =================
;; Data definitions:

; A PositiveNumber is a Number greater than/equal to 0.

(define-struct vec [x y])
; A vec is
;   (make-vec PositiveNumber PositiveNumber)
; interpretation represents a velocity vector


;; =================
;; Functions:

; Any -> Vec
; creates a vec structure
; if x and y are positive numbers
(check-expect (checked-make-vec 5  5)  (make-vec 5 5))
(check-error  (checked-make-vec 0  0)  "make-vec: positive numbers expected")
(check-error  (checked-make-vec 1  0)  "make-vec: positive numbers expected")
(check-error  (checked-make-vec 0  1)  "make-vec: positive numbers expected")
(check-error  (checked-make-vec -1 1)  "make-vec: positive numbers expected")
(check-error  (checked-make-vec 1  -1) "make-vec: positive numbers expected")
(check-error  (checked-make-vec -1 -1) "make-vec: positive numbers expected")

(define (checked-make-vec x y)
  (cond [(and (> x 0)
              (> y 0))
         (make-vec x y)]
        [else (error "make-vec: positive numbers expected")]))

;; Exercise 112


;; =================
;; Functions:

; Any -> Boolean
; is v an element of the MissileOrNot collection
(check-expect (missile-or-not? #false) #true)
(check-expect (missile-or-not? (make-posn 9 2)) #true)
(check-expect (missile-or-not? "yellow") #false)
(check-expect (missile-or-not? #true) #false)
(check-expect (missile-or-not? 10) #false)
(check-expect (missile-or-not? empty-image) #false)

(define (missile-or-not? v)
  (or (false? v)
      (posn?  v)))

;; Exercise 113


;; =================
;; Functions:

; Any -> Boolean
; is s an element of the SIGS collection
(check-expect (is-a-sigs? (make-aim "ufo" "tank")) #true)
(check-expect (is-a-sigs? (make-fired "ufo" "tank" "missile")) #true)
(check-expect (is-a-sigs? "troops") #false)
(check-expect (is-a-sigs? 5) #false)

(define (is-a-sigs? s)
  (or (aim?   s)
      (fired? s)))

; Any -> Boolean
; is c an element of the Coordinate collection
(check-expect (is-a-coordinate? -3) #true)
(check-expect (is-a-coordinate?  3) #true)
(check-expect (is-a-coordinate? (make-posn 10 10)) #true)
(check-expect (is-a-coordinate? "1,2") #false)

(define (is-a-coordinate? c)
  (or (and (number? c)
           (or (< c 0)
               (> c 0)))
      (posn? c)))

; Any -> Boolean
; is va an element of the VAnimal collection
(check-expect (is-a-vanimal? (make-vcat  20 100)) #true)
(check-expect (is-a-vanimal? (make-vcham 20 100 "red")) #true)
(check-expect (is-a-vanimal? "cat") #false)

(define (is-a-vanimal? va)
  (or (vcat?  va)
      (vcham? va)))



;; 6.4 - Checking the World

;; Exercise 114


;; =================
;; Data definitions:

; SIGS -> World
; starts a world with...
(define (main-sigs s)
  (big-bang s
            (on-tick ...)
            (on-draw ...)
            (check-with is-a-sigs?)))

; VAnimal -> World
; starts a world with...
(define (main-vanimal va)
  (big-bang va
            (on-tick ...)
            (on-draw ...)
            (check-with is-a-vanimal?)))


;; =================
;; Data definitions:

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with
; the cursor displayed between s and t


;; =================
;; Functions:

; Editor -> World
; starts a world with...
(define (main-editor e)
  (big-bang e
            (on-tick ...)
            (on-draw ...)
            (check-with is-an-editor?)))

; Any -> Boolean
; is e an Editor structure
(check-expect (is-an-editor? (make-editor "Hello" "World")) #true)
(check-expect (is-an-editor? "Hello | World") #false)
(check-expect (is-an-editor? 10) #false)

(define (is-an-editor? e)
  (editor? e))



;; 6.5 - Equality Predicates

;; Exercise 115


;; =================
;; Constants:

(define MESSAGE "traffic light expected, given ")


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

; Any -> Boolean
; is the given value an element of TrafficLight
(define (light? x)
  (cond [(string? x)
         (or (string=? "red"    x)
             (string=? "green"  x)
             (string=? "yellow" x))]
        [else #false]))

; Any Any -> Boolean
; are the two values elements of TrafficLight and,
; if so, are they equal
; else throw an error
(check-expect (light=? "red"    "red")    #true)
(check-expect (light=? "red"    "green")  #false)
(check-expect (light=? "green"  "green")  #true)
(check-expect (light=? "yellow" "yellow") #true)
(check-error  (light=? "white"  "yellow") "traffic light expected, given white")

(define (light=? a-value another-value)
  (cond [(and (light? a-value)
              (light? another-value))
         (string=? a-value another-value)]
        [else
         (error (string-append MESSAGE
                               (if (light? a-value)
                                   another-value
                                   a-value)))]))



;; 7 - Summary
