;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname JNevinsHW5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#| Jackson Nevins
 HW5B ITEC 380
 https://learn.radford.edu/d2l/lms/dropbox/user/folder_submit_files.d2l?db=335599&grpid=0&isprv=0&bp=0&ou=219724
Source(s) used: hw04b-struct-pacman.rkt by ibarland@radford.edu https://learn.radford.edu/d2l/le/content/219724/viewContent/3613291/View
                https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html
|#
(require 2htdp/image)
(require 2htdp/universe)
(require "student-extras.rkt")
(require "videogame-helpers.rkt")

(define (lon? val) ; lon = list-of-numbers 'list' is a built-in keyword that cannot be overridden
  (or (equal? val '()) ; interpretation: an empty list
      (cons? val)))

(define-struct/c cons ([first any?] [rest lon?]))   ; interpretation: `first` tacked to the front of the list `rest`.

;Start of count-bigs test cases
(check-expect (count-bigs 7 '()) 0)
(check-expect (count-bigs 5 (list 5)) 0)
(check-expect (count-bigs 3 (list 5)) 1)
(check-expect (count-bigs 5 (list 1 2 3 4 5)) 0)
(check-expect (count-bigs 3 (list 2 5)) 1)
(check-expect (count-bigs 2 (list 2 5)) 1)
(check-expect (count-bigs 1 (list 2 5)) 2)

; Returns the amount of numbers in the list that are bigger than the threshold
;Start of count-bigs function
(define (count-bigs threshold lst)
  (cond [(empty? lst) 0]
        [(> (first lst) threshold) 
         (add1 (count-bigs threshold (rest lst)))]
        [else (count-bigs threshold (rest lst))]))

; Start of map-sqr test cases
(check-expect (map-sqr empty)  empty)
(check-expect (map-sqr (list 7 5)) (cons 49 (cons 25 empty)))
(check-expect (map-sqr (list 9 7)) (cons 81 (cons 49 empty)))

; Returns the list with each number in it squared
(define (map-sqr list)
  (if (empty? list)
      empty
      (cons (* (first list) (first list))
            (map-sqr (rest list)))))

;; ------------ Start of Pacman Part ------------;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These consants were taken from hw04b-struct-pacman.rkt
(define BOARD-WIDTH 500) ; in pixels
(define BOARD-HEIGHT BOARD-WIDTH) ; in pixels
(define MAX-MOUTH-ANGLE 111) ; how wide a pac-man's mouth can open, from center, in degrees.
(define MOUTH-dθ 3) ; How fast a pac-man's mouth-angle changes, in degrees/tick.
(define V 3)  ; How fast a pac-man or ghost can move in x- or y- direction, in pixels/tick.
(define RADIUS 20) ; Radius of a pacman and a ghost
(define BACKGROUND (rectangle BOARD-WIDTH BOARD-HEIGHT 'solid 'black))
; End of Constants

; Define Datatype
(define (direction? x)
  (or (equal? x "left")
      (equal? x "right")
      (equal? x "down")
      (equal? x "up")
      (equal? x "still")))
; Examples of Direction
; "left"
; "right"
; "up"
; "down"
; "still"

; Template 
#|(define directionToSomething?)
	(-> direction? ..?)
	(cond [ (direction? d)  ...]
			  [ (false? d)    ...]
			  [ (else)               ...]))|#

; Define Datatype
(define-struct/contract pacman ([x real?] ;X coordinate in pixels: X & Y mean the center of the image
                       [y real?] ;Y coordinate in pixels X & Y mean the center of the image
                       [age real>=0?] ; Age of the pacman to determine position for quicksave file
                       [direction direction?] ; Current direction the pacman is going
                       [next-dir direction?] ; The direction the pacman should go at the next intersection
                       [power-time-remaining real?])) ; Time remaining on Pacman's power up
; Examples of Datatype
(define p1 (make-pacman 0 0 10 "left" "up" 100))
(pacman-x p1)
(pacman-y p1)
(pacman-age p1)
(pacman-direction p1)
(pacman-next-dir p1)
(pacman-power-time-remaining p1)

(define p2 (make-pacman 1 0 20 "right" "down" 2))
(pacman-x p2)
(pacman-y p2)
(pacman-direction p2)
(pacman-next-dir p2)
(pacman-power-time-remaining p2)

(define p3 (make-pacman 0 1 45 "up" "right" 15))
(pacman-x p3)
(pacman-y p3)
(pacman-direction p3)
(pacman-next-dir p3)
(pacman-power-time-remaining p3)

; Template 
#|(define/c (pacmanToSomething? a-pman)
  (... (pacman-x a-pman)
       (pacman-y a-pman)
       (pacman-direction a-pman)))|#

#|(define/contract (pacman-glide p1) old glide method
  (-> pacman? pacman?)
  (make-pacman (pacman-x p1) (pacman-y p1) (pacman-currentDirection p1) (pacman-pendingDirection p1) (pacman-totalRuntime p1 + 1))) |#

; New glide from hw04b-struct-pacman
(define/contract (glide-pacman p)
  (-> pacman?  pacman?)
    (make-pacman (+ (pacman-x p) (* V (dir->dx (pacman-direction p))))
                 (+ (pacman-y p) (* V (dir->dy (pacman-direction p))))
                 (add1 (pacman-age p))
                 (pacman-direction p)
                 (pacman-next-dir p)
                 (monus (pacman-power-time-remaining p) 1)))

(check-expect (dir->dx "left")  -1)
(check-expect (dir->dx "right") +1)
(check-expect (dir->dx "down")   0)
(check-expect (dir->dx "up")     0)
(check-expect (dir->dx " ")      0)

(check-expect (dir->dy "left")   0)
(check-expect (dir->dy "right")  0)
(check-expect (dir->dy "down")  +1)
(check-expect (dir->dy "up")    -1)
(check-expect (dir->dy " ")      0)

(define/contract (dir->dx dir)
  (-> string? real?)
  (cond [(string=? dir "left")  -1]
        [(string=? dir "right") +1]
        [(string=? dir "down")   0]
        [(string=? dir "up")     0]
        [(string=? dir " ")      0]))

(define/contract (dir->dy dir)
  (-> string? real?)
  (cond [(string=? dir "left")   0]
        [(string=? dir "right")  0]
        [(string=? dir "down")  +1]
        [(string=? dir "up")    -1]
        [(string=? dir " ")      0]))


(check-expect (direction->angle "left")  180)
(check-expect (direction->angle "right")   0)
(check-expect (direction->angle "down")  -90)
(check-expect (direction->angle "up")    +90)
(check-expect (direction->angle " ")       0)

; direction->angle : direction -> real?
; Return an angle to rotate a figure by, based on its direction.
;
(define (direction->angle dir)
  (cond [(string=? dir "left")  180]
        [(string=? dir "right")   0]
        [(string=? dir "down")  -90]
        [(string=? dir "up")    +90]
        [(string=? dir " ")       0]))





;;;;;;;;;;; simple math helpers:

(check-expect (monus 3 2) 1)
(check-expect (monus 3 3) 0)
(check-expect (monus 3 4) 0)
(check-expect (monus 0 3) 0)
(check-expect (monus 0 0) 0)

; monus : real?, real? -> (and/c real? non-negative/c)
; Return a-b, except return 0 if a-b<0.
;
(define (monus a b) (max (- a b) 0))


; <=< : real?, real?, real? -> boolean?
; Is x in the half-open interval [a,b)?
; pre-conditon: (<= a b)
;
(define (<=< a x b) (and (<= a x) (< x b)))
(check-expect (<=< 4  7   10) true)
(check-expect (<=< 7  7   10) true)
(check-expect (<=< 4  7    7) false)
(check-expect (<=< 4 -7   10) false)
(check-expect (<=< 4  4    4) false)
(check-expect (<=< 4  4.1  4.5) true)
(check-expect (<=< -4.5  -4.1  -4) true)
;(check-expect (<=< -4  -4.1  -4.5) false) ; N.B. violates pre-condition: a > b.

; Define Datatype
(define-struct/contract ghost ([x real?] ;X coordinate in pixels: X & Y mean the center of the image
                       [y real?] ;Y coordinate in pixels X & Y mean the center of the image
                       [direction direction?] ; Current direction the ghost is going
                       [next-dir direction?] ; The direction the ghost should go at the next intersection
                       [power-time-remaining real?]))
; Examples of Datatype
(make-ghost 1 0 "up" "left" 0)
(make-ghost 2 2 "down" "right" 100)

; Template 
#|(define/c (func-for-ghost a-ghost)
  (... (ghost-x a-ghost)
       (ghost-y a-ghost)
       (ghost-direction a-ghost)
       (ghost-next-dir a-ghost)
       (ghost-power-time-remaining a-ghost)))|#

; Define Datatype
(define-struct/contract wall ([x real?] ;X coordinate in pixels: X & Y mean the center of the image
                       [y real?] ;Y coordinate in pixels X & Y mean the center of the image
                       [width real>=0?] ; The width of the wall in pixels
                       [height real>=0?])) ; the height of the wall in pixels
; Examples of Datatype
(make-wall 10 20 7 7)
(make-wall 20 10 5 3)

; Template 
#|(define/c (func-for-wall a-wall)
  (... (wall-x a-wall)
       (wall-y a-wall)
       (wall-width a-wall)
       (wall-height a-wall)))|#

(check-expect (glide-ghost (make-ghost 0 0 "left" "right" 10))
              (make-ghost (- V) 0 "left" "right" 9))

(check-expect (glide-ghost (make-ghost 5 5 "up" "down" 20))
              (make-ghost 5 (- V 1) "up" "down" 19))

(check-expect (glide-ghost (make-ghost 10 10 "right" "left" 15))
              (make-ghost (+ V 10) 10 "right" "left" 14))

; glide-ghost : ghost? -> ghost?
; Returns a new ghost struct one tick of time later, ignoring any exterior factors like walls or key-presses.
(define/contract (glide-ghost g)
  (-> ghost?  ghost?)
    (make-ghost (+ (ghost-x g) (* V (dir->dx (ghost-direction g))))
                 (+ (ghost-y g) (* V (dir->dy (ghost-direction g))))
                 (ghost-direction g)
                 (ghost-next-dir g)
                 (monus (ghost-power-time-remaining g) 1)))


; draw-pacman (pacman? -> image?)
; Returns an image of a pacman in the right direction on a background 
(define (draw-pacman a-pman)
  (cond
    [(equal? (pacman-direction a-pman) "up")
     (underlay BACKGROUND (rotate -90 (underlay (rotate 30 (wedge 30 300 "solid" "gold"))
                                                (circle 3 "solid" "black"))))]
    [(equal? (pacman-direction a-pman) "down")
     (underlay BACKGROUND (rotate 90 (underlay (rotate 30 (wedge 30 300 "solid" "gold"))
                                               (circle 3 "solid" "black"))))]
    [(equal? (pacman-direction a-pman) "left")
     (underlay BACKGROUND (rotate -90 (underlay (rotate 30 (wedge 30 300 "solid" "gold"))
                                                (circle 3 "solid" "black"))))]
    [(equal? (pacman-direction a-pman) "right")
     (underlay BACKGROUND (underlay (rotate 30 (wedge 30 300 "solid" "gold"))
                                    (circle 3 "solid" "black")))]))
  
(define p0  (make-pacman 0 0 0 "right" "right" 0))

(check-expect (pacman-handle-key p0 "right")
              (struct-copy pacman p0 [next-dir "right"]))

(check-expect (pacman-handle-key (make-pacman 35 99 1000 "up"  "left" 0) "up")
              (make-pacman 35 99 1000 "up"  "up" 0))

(check-expect (pacman-handle-key p0 "left")
              (struct-copy pacman p0 [next-dir "left"]))

(check-expect (pacman-handle-key (make-pacman 35 99 1000 "up"  "left" 0) "down")
              (make-pacman 35 99 1000 "up"  "down" 0))

; pacman-handle-key : pacman? key-event? -> pacman?
; Returns a new or updated pacman after a key has been pressed
(define (pacman-handle-key a-pman key-pressed)
  ;(pacman? key-event? -> pacman?)
  (cond
    [(key=? key-pressed "up")
     (struct-copy pacman a-pman [next-dir "up"])]
    [(key=? key-pressed "down")
     (struct-copy pacman a-pman [next-dir "down"])]
    [(key=? key-pressed "left")
     (struct-copy pacman a-pman [next-dir "left"])]
    [(key=? key-pressed "right")
     (struct-copy pacman a-pman [next-dir "right"])]))

(check-expect (pacman-overlap-wall? (make-pacman 0 0 0 "right" "right" 0) (make-wall 5 10 5 10)) #t)
(check-expect (pacman-overlap-wall? (make-pacman -5 -5 5 "right" "right" 0) (make-wall 5 10 5 10)) #f)
(check-expect (pacman-overlap-wall? (make-pacman 15 15 5 "right" "right" 0) (make-wall 0 1 0 1)) #f)

; pacman-overlap-wall? : pacman? wall? -> boolean?
; Returns True or false based on if a pacman is overlapping a wall
(define (pacman-overlap-wall? pacman wall)
  (overlap? (pacman-x pacman) (pacman-y pacman) 
            RADIUS RADIUS
            (wall-x wall) (wall-y wall) 
            (wall-width wall) (wall-height wall)))

(check-expect (draw-ghost (make-ghost 0 0 "left" "down" 9)
                           'blue)
              (rotate 180
  (beside (underlay (circle 20 'solid 'blue)
                    (circle 5 'solid 'white))
          (rotate (- 90) (triangle 15 'solid 'blue)))))

(check-expect (draw-ghost (make-ghost 0 0 "up" "down" 9)
                           'red)
              (rotate 90
  (beside (underlay (circle 20 'solid 'red)
                    (circle 5 'solid 'white))
          (rotate (- 90) (triangle 15 'solid 'red)))))

; draw-ghost : ghost? color? -> image?
; Returns an image of a ghost facing the correct direction

(define (draw-ghost g color)
  (cond
    [(equal? (ghost-direction g) "up")
     (rotate 90
  (beside (underlay (circle 20 'solid color)
                    (circle 5 'solid 'white))
          (rotate (- 90) (triangle 15 'solid color))))]
    [(equal? (ghost-direction g) "down")
     (rotate -90
  (beside (underlay (circle 20 'solid color)
                    (circle 5 'solid 'white))
          (rotate (- 90) (triangle 15 'solid color))))]
    [(equal? (ghost-direction g) "left")
     (rotate 180
  (beside (underlay (circle 20 'solid color)
                    (circle 5 'solid 'white))
          (rotate (- 90) (triangle 15 'solid color))))]
    [(equal? (ghost-direction g) "right")
     (beside (underlay (circle 20 'solid color)
                    (circle 5 'solid 'white))
          (rotate (- 90) (triangle 15 'solid color)))]))

(check-expect (draw-wall (make-wall 5 10 5 10))
              (rectangle 5 10 'solid 'blue))
(check-expect (draw-wall (make-wall 10 5 10 5))
              (rectangle 10 5 'solid 'blue))
(check-expect (draw-wall (make-wall 0 0 0 0))
               empty-image)

; draw-wall : wall? -> image?
; Returns an image of a wall with the given dimensions
(define (draw-wall w)
  (rectangle (wall-x w) (wall-height w) 'solid 'blue))

(check-expect (ghost-overlap-wall? (make-ghost 5 10 "right" "down" 0) (make-wall 5 10 5 10)) #t)
(check-expect (ghost-overlap-wall? (make-ghost -5 -5 "left" "up" 1) (make-wall 5 10 5 10)) #f)
(check-expect (ghost-overlap-wall? (make-ghost 15 15 "up" "right" 0) (make-wall 0 1 0 1)) #f)

; pacman-overlap-wall? : pacman? wall? -> boolean?
; Returns True or false based on if a pacman is overlapping a wall
(define (ghost-overlap-wall? a-ghost wall)
  (overlap? (ghost-x a-ghost) (ghost-y a-ghost) 
            RADIUS RADIUS
            (wall-x wall) (wall-y wall) 
            (wall-width wall) (wall-height wall)))

(check-expect (ghost-spin (make-ghost 5 10 "right" "down" 0)) (make-ghost 5 10 "down" "down" 0))
(check-expect (ghost-spin (make-ghost 0 0 "up" "right" 0)) (make-ghost 0 0 "right" "right" 0))
(check-expect (ghost-spin (make-ghost 1 1 "down" "up" 0)) (make-ghost 1 1 "left" "up" 0))

; ghost-spin : ghost? -> ghost?
; returns a ghost that is turned one direction clockwise

(define (ghost-spin a-ghost)
  (cond
    [(equal? (ghost-direction a-ghost) "up")
     (struct-copy ghost a-ghost [direction "right"])]
    [(equal? (ghost-direction a-ghost) "right")
     (struct-copy ghost a-ghost [direction "down"])]
    [(equal? (ghost-direction a-ghost) "down")
     (struct-copy ghost a-ghost [direction "left"])]
    [(equal? (ghost-direction a-ghost) "left")
     (struct-copy ghost a-ghost [direction "up"])]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW CODE / START OF HW5A
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Datatype definition:
(define (list-of-ghosts? val)
  (cond ((eq? val '()) #t)
        ((not (ghost? (first val))) #f)
        (else (list-of-ghosts? (rest val)))))

;Examples of Data:
(list (make-ghost 5 10 "right" "down" 0) (make-ghost 10 5 "left" "up" 0) (make-ghost 5 5 "up" "down" 0) (make-ghost 0 0 "up" "left" 0))

(cons (make-ghost 5 10 "right" "down" 0) (cons (make-ghost 10 5 "left" "up" 0) '()))

;Template:
;;(define (func-for-list a-log)
  ;;(-> list-of-ghosts?  ...?)
  ;;(cond [(equal? a-log '()) ...]
        ;;[(cons? a-log) (... (first a-log) (func-for-list (rest a-log)))]))

;Returns if the pacman is overlapping the given ghost
(define (pacman-overlap-ghost? pacman ghost)
  ;(-> pacman? ghost?  boolean?)
  (overlap? (pacman-x pacman) (pacman-y pacman) 
            (* 2 RADIUS) (* 2 RADIUS)
            (ghost-x ghost) (ghost-y ghost)
            (* 2 RADIUS) (* 2 RADIUS)))

;;Start of pacman-overlap-ghosts? test cases:

(define pacman1 (make-pacman 0 0 0 "right" "right" 0))
(define pacman2 (make-pacman 5 5 0 "up" "left" 0))
(define pacman3 (make-pacman 100 100 0 "right" "down" 0))
(define testGhost1 (make-ghost 0 0 "up" "left" 0))
(define testGhost2 (make-ghost 4 5 "down" "right" 0))
(define testGhost3 (make-ghost 1 5 "left" "down" 0))
(define testGhost4 (make-ghost 5 5 "down" "up" 0))
(check-expect (pacman-overlap-ghosts? pacman1 (list testGhost1 testGhost2)) #t)
(check-expect (pacman-overlap-ghosts? pacman2 (list testGhost1 testGhost2 testGhost3 testGhost4)) #t)
(check-expect (pacman-overlap-ghosts? pacman3 (list testGhost1 testGhost2 testGhost3 testGhost4)) #f)
;Returns if the pacman is overlapping any of the given ghosts
(define (pacman-overlap-ghosts? pacman ghosts)
  ;(-> pacman? list-of-ghosts?  boolean?)
  (cond ((null? ghosts) #f)
        ((pacman-overlap-ghost? pacman (first ghosts)) #t)
        (else (pacman-overlap-ghosts? pacman (rest ghosts)))))

; Start of ghosts-remaining test cases:
(check-expect (ghosts-remaining (make-pacman 0 0 0 "left" "right" 0) empty) empty)
(check-expect (ghosts-remaining (make-pacman 0 0 0 "right" "right" 0)
                                 (list (make-ghost 0 0 "up" "up" 0)
                                       (make-ghost 0 0 "left" "up" 0)))
              empty)

;Returns a list of the ghosts that are not overlapping pacman
(define (ghosts-remaining pacman ghosts)
  ;(-> pacman? list-of-ghosts?  list-of-ghosts?)
  (cond ((null? ghosts) '())
        ((pacman-overlap-ghost? pacman (first ghosts))
         (ghosts-remaining pacman (rest ghosts)))
        (else (cons (first ghosts)
                    (ghosts-remaining pacman (rest ghosts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEW CODE / START OF HW5B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Datatype definition:
(define (list-of-walls? val)
  (cond ((eq? val '()) #t)
        ((not (wall? (first val))) #f)
        (else (list-of-walls? (rest val)))))

; Examples of list-of-walls?
(define gameWalls (list (make-wall 0 50 50 5)
                        (make-wall 0 -50 50 5)
                        (make-wall 50 0 5 50)
                        (make-wall -50 0 5 50)))
(list (make-wall 0 0 20 20)
      (make-wall 50 50 20 20)
      (make-wall 10 10 10 10))

(cons (make-wall 0 0 10 10)(cons (make-wall 10 10 10 10)(cons (make-wall 20 20 10 10) '())))



; Start of test cases for pacman-overlap-walls?
(check-expect (pacman-overlap-walls? (make-pacman 0 0 0 "left" "right" 0)
                                     empty)
              #f)

(check-expect (pacman-overlap-walls? (make-pacman 0 0 0 "left" "right" 0)
                                     (list (make-wall 0 0 20 20)))
              #t)

(check-expect (pacman-overlap-walls? (make-pacman 10 10 0 "left" "right" 0)
                                     (list (make-wall 50 50 20 20)))
              #f)

(check-expect (pacman-overlap-walls? (make-pacman 20 20 0 "left" "right" 0)
                                     (list (make-wall 0 0 10 10)
                                           (make-wall 10 10 10 10)
                                           (make-wall 20 20 10 10)))
              #t)
; pacman-overlap-walls? : pacman, list-of-walls -> boolean
; Returns if the pacman is overlapping any of the given walls
;
(define (pacman-overlap-walls? pacman walls)
  ;(-> pacman? list-of-walls?  boolean?)
  (cond ((null? walls) #f)
        ((pacman-overlap-wall? pacman (first walls)) #t)
        (else (pacman-overlap-walls? pacman (rest walls)))))

; Start of test cases for ghost-overlap-walls?
(check-expect (ghost-overlap-walls? (make-ghost 0 0 "up" "left" 0)
                                     empty)
              #f)

(check-expect (ghost-overlap-walls? (make-ghost 4 5 "down" "right" 0)
                                     gameWalls)
              #f)

(check-expect (ghost-overlap-walls? (make-ghost 5 5 "left" "right" 0)
                                     (list (make-wall 50 50 20 20)
                                           (make-wall 15 15 0 0)))
              #f)

(check-expect (ghost-overlap-walls? (make-ghost 20 20 "left" "right" 0)
                                     (list (make-wall 0 0 10 10)
                                           (make-wall 10 10 10 10)
                                           (make-wall 20 20 10 10)))
              #t)

; ghost-overlap-walls? : ghost, list-of-walls -> boolean
;Returns if the ghost is overlapping any of the given walls
;
(define (ghost-overlap-walls? ghost walls)
  ;(-> ghost? list-of-walls?  boolean?)
  (cond ((null? walls) #f)
        ((ghost-overlap-wall? ghost (first walls)) #t)
        (else (ghost-overlap-walls? ghost (rest walls)))))

; Start of draw-walls
(check-expect (draw-walls null)
              BACKGROUND)

(check-expect (draw-walls (list (make-wall 200 200 10 10)
                                (make-wall 100 100 5 5)))
              (place-image (draw-wall (make-wall 200 200 10 10))
                           200 200
                           (place-image (draw-wall (make-wall 100 100 5 5))
                                        100 100
                                        BACKGROUND)))
; draw-walls : list-of-walls -> image
; Returns all of the walls from the list-of-walls on the defined BACKGROUND at the start of the code
;
(define (draw-walls walls)
  ;(-> list-of-walls?  image?)
  (cond ((null? walls) BACKGROUND)
        (else (place-image (draw-wall (first walls)) (wall-x (first walls)) (wall-y (first walls)) (draw-walls (rest walls))))))

; Start of draw-ghosts test cases
(check-expect (draw-ghosts null)
              BACKGROUND)

(check-expect (draw-ghosts (list (make-ghost 50 10 "right" "down" 0)
                                 (make-ghost 25 50 "left" "up" 0)))
              (place-image (draw-ghost (make-ghost 50 10 "right" "down" 0) "red")
                           50 10
                           (place-image (draw-ghost (make-ghost 25 50 "left" "up" 0) "red")
                                        25 50
                                        BACKGROUND)))



; draw-ghosts : list-of-ghosts -> image
; Returns all of the ghosts from the list-of-ghosts on the defined BACKGROUND at the start of the code
;
(define (draw-ghosts ghosts)
  ;(-> list-of-ghosts?  image?)
  (cond ((null? ghosts) BACKGROUND)
        (else (place-image (draw-ghost (first ghosts) 'red) (ghost-x (first ghosts)) (ghost-y (first ghosts)) (draw-ghosts (rest ghosts)))))) 


; Start of move-ghost test cases
(check-expect (move-ghost (make-ghost 0 0 "right" "up" 0)
                          gameWalls)
              (make-ghost 1 0 "right" "up" 0))

(check-expect (move-ghost (make-ghost 0 0 "left" "down" 0)
                          gameWalls)
              (make-ghost -1 0 "left" "down" 0))

(check-expect (move-ghost (make-ghost 0 39 "up" "down" 0)
                          gameWalls)
              (make-ghost 0 39 "right" "down" 0))

; move-ghost : ghost, list-of-walls -> ghost
; Similar to glide-ghost but accounts for walls, if about to collide with wall, uses ghost-spin instead of glide-ghost
;
(define (move-ghost g walls)
  ;(-> ghost? list-of-walls? ghost?)
  (let* ((next-x (+ (ghost-x g) (dir->dx (ghost-direction g))))
         (next-y (+ (ghost-y g) (dir->dy (ghost-direction g)))))    
    (cond 
      [(ghost-overlap-walls? (make-ghost next-x next-y (ghost-direction g) (ghost-next-dir g) (monus (ghost-power-time-remaining g) 1)) walls)
       (ghost-spin g)]
      [else 
       (make-ghost next-x next-y (ghost-direction g) (ghost-next-dir g) (monus (ghost-power-time-remaining g) 1))])))

; Start of move-pacman test cases
(check-expect (move-pacman (make-pacman 0 0 0 "right" "up" 0)
                          gameWalls)
              (make-pacman 1 0 1 "right" "up" 0))

(check-expect (move-pacman (make-pacman 0 0 0 "left" "down" 0)
                          gameWalls)
              (make-pacman -1 0 1 "left" "down" 0))

(check-expect (move-pacman (make-pacman 0 0 39 "up" "down" 0)
                          gameWalls)
              (make-pacman 0 -1 40 "up" "down" 0))

; move-pacman : pacman, list-of-walls -> pacman
; Similar to glide-pacman but accounts for walls
;
(define (move-pacman p walls)
  ;(-> pacman? list-of-walls? pacman?)
  (let* ((next-x (+ (pacman-x p) (dir->dx (pacman-direction p))))
         (next-y (+ (pacman-y p) (dir->dy (pacman-direction p)))))    
    (cond
      [(pacman-overlap-walls? (make-pacman next-x next-y (add1 (pacman-age p)) (pacman-direction p) (pacman-next-dir p) (monus (pacman-power-time-remaining p) 1)) walls)
       (let ((new-x (+ (pacman-x p) (dir->dx (pacman-next-dir p)))))
         (let ((new-y (+ (pacman-y p) (dir->dy (pacman-next-dir p)))))
           (cond
             [(pacman-overlap-walls? (make-pacman new-x new-y (add1 (pacman-age p)) (pacman-next-dir p) (pacman-next-dir p) (monus (pacman-power-time-remaining p) 1)) walls)
              p]
             [else
              (make-pacman new-x new-y (add1 (pacman-age p)) (pacman-next-dir p) (pacman-next-dir p) (monus (pacman-power-time-remaining p) 1))])))]
      [else
       (make-pacman next-x next-y (add1 (pacman-age p)) (pacman-direction p) (pacman-next-dir p) (monus (pacman-power-time-remaining p) 1))])))

;Datatype definition:
; world : pacman, list-of-ghosts, list-of-walls -> image
; Struct for a world that contains pacman ghosts and walls.
(define-struct world (pacman ghosts walls))

; Example worlds:
(define world1 (make-world (make-pacman 50 50 0 "right" "right" 0)
                           (list (make-ghost 25 25 "left" "up" 0) (make-ghost 75 75 "right" "down" 0))
                           (list (make-wall 0 0 100 10) (make-wall 0 90 100 100) (make-wall 0 0 10 100) (make-wall 90 0 100 100))))

(define world2 (make-world (make-pacman 20 30 0 "up" "up" 0)
                           (list (make-ghost 20 30 "right" "down" 0) (make-ghost 60 40 "left" "up" 0))
                           (list (make-wall 0 0 100 10) (make-wall 0 90 100 100) (make-wall 0 0 10 100) (make-wall 90 0 100 100))))
; Template
(define (func-for-world w)
  (make-world (... (world-pacman w))
              (map ... (world-ghosts w))
              (world-walls w)))

; Start of test cases for move-world
(check-expect (move-world (make-world (make-pacman 50 10 0 "right" "right" 0) 
                                      (list (make-ghost 50 50 "right" "up" 0))
                                      (list (make-wall 100 200 10 20))))
              (make-world
               (make-pacman 51 10 1 "right" "right" 0)
               (list
                (make-ghost 51 50 "right" "up" 0))
               (list (make-wall 100 200 10 20))))
                          
(check-expect (move-world (make-world (make-pacman 20 20 0 "up" "up" 0) 
                                      (list (make-ghost 40 40 "left" "up" 0))
                                      (list (make-wall 0 0 10 10))))
              (make-world
               (make-pacman 20 19 1 "up" "up" 0)
               (list (make-ghost 39 40 "left" "up" 0))
               (list (make-wall 0 0 10 10))))

; move-world: world -> world
; Returns a world one tick later
(define (move-world world)
  ;(-> world? world?)
  (let ((new-pacman (move-pacman (world-pacman world) (world-walls world)))
        (new-ghosts (map (lambda (g) (move-ghost g (world-walls world))) (world-ghosts world))))
    (make-world new-pacman new-ghosts (world-walls world))))

; Test cases for world-handle-key
(check-expect (world-handle-key world1 "right")
              (make-world
               (make-pacman 50 50 0 "right" "right" 0)
               (list
                (make-ghost 25 25 "left" "up" 0)
                (make-ghost 75 75 "right" "down" 0))
               (list
                (make-wall 0 0 100 10)
                (make-wall 0 90 100 100)
                (make-wall 0 0 10 100)
                (make-wall 90 0 100 100))))


; world-handle-key: world, key-event -> world
; Returns a new world that is updated to handle the key press
(define (world-handle-key world key-pressed)
  ;(world? key-event? -> world?)
  (make-world (pacman-handle-key (world-pacman world) key-pressed)
              (world-ghosts world)
              (world-walls world)))



; Test case for draw-world
(check-expect (draw-world null) BACKGROUND)

; draw-world: world -> image
; Returns the world on the BACKGROUND defined earlier
;
(define (draw-world world)
  (cond ((null? world) BACKGROUND)
        (else (overlay/xy (overlay/xy(draw-pacman (world-pacman world))
                                      (pacman-x (world-pacman world))
                                      (pacman-y (world-pacman world))
                                      (draw-ghosts (world-ghosts world)))
              (pacman-x (world-pacman world))
              (pacman-y (world-pacman world))
              (draw-walls (world-walls world))))))

; Test case for game-over?
(check-expect (game-over? null) #f)
(check-expect (game-over? world2) #t)

; game-over: world -> image
; Returns whether or not the game is over (Pacman is colliding with one or more ghosts)
;
(define (game-over? world)
  (cond ((null? world) #f)
        (else (pacman-overlap-ghosts? (world-pacman world) (world-ghosts world)))))
    


