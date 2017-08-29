;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname nsb296) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "struct-inheritance.rkt")

(define firing-engines? false)


(define (update-player! p)
  (when firing-engines?
    (set-game-object-velocity! p
                               (posn-+ (game-object-velocity p)
                                       (posn-* inter-frame-interval
                                               (posn-* 50 (forward-direction p)))
                                       ))))

(define (update-missile! m)
  (if (= 0 (missile-lifetime m))
      (destroy! m)
      (set-missile-lifetime! m (- (missile-lifetime m) 1))))

;; Called when the up arrow key is pressed
(define (on-up-press)
  (set! firing-engines? true))

;; Called when the up arrow key is released
(define (on-up-release)
  (set! firing-engines? false))

;; Called when the left arrow key is pressed
(define (on-left-press)
  (set-game-object-rotational-velocity! the-player (- 1)))

;; Called when the left arrow key is released
(define (on-left-release)
  (set-game-object-rotational-velocity! the-player 0))

;; Called when the right arrow key is pressed
(define (on-right-press)
  (set-game-object-rotational-velocity! the-player 1))

;; Called when the right arrow key is released
(define (on-right-release)
  (set-game-object-rotational-velocity! the-player 0))

;; Called when the space bar is pressed.
;; There's no action to take when the space bar is released.
(define (on-space-press)
  (fire-missile!))

;;;
;;; Don't modify the code below
;;;

;;;
;;; Turnable constants
;;;

(define window-width 800)
(define window-height 600)
(define frame-rate 30)
(define inter-frame-interval (/ 1.0 frame-rate))
(define asteroid-count 10)


;;;
;;; Type definitions
;;;

(define-struct game-object
  [position velocity orientation rotational-velocity radius])

(define-struct (player game-object) [])
(define-struct (missile game-object)
  [lifetime])
(define-struct (asteroid game-object)
  [color])

;;;
;;; Tracking game objects
;;;

(define all-game-objects '())

(define (destroy! object)
  (set! all-game-objects
        (remove object all-game-objects)))

(define the-player '())

;;;
;;; Object creation
;;;

(define (new-asteroid)
  (make-asteroid (make-asteroid-position)
                 (random-velocity)
                 0
                 0
                 (random-float 10 30)
                 (random-color)))

(define (make-asteroid-position)
  (local [(define candidate (make-posn (random window-width)
                                       (random window-height)))]
    (if (> (distance-squared candidate (game-object-position the-player))
           (squared (* 5
                       (game-object-radius the-player))))
        candidate
        (make-asteroid-position))))

(define (fire-missile!)
  (local [(define forward (forward-direction the-player))]
    (set! all-game-objects
          (cons (make-missile (posn-+ (game-object-position the-player)
                                      (posn-* (+ (game-object-radius the-player) 5)
                                              forward))
                              (posn-+ (game-object-velocity the-player)
                                      (posn-* 100 forward))
                              0
                              0
                              1
                              100)
                all-game-objects))))

;;;
;;; Driver loop
;;;

(define (asteroids)
  (begin (set! the-player
               (make-player (make-posn (/ window-width 2)
                                       (/ window-height 2))
                            (make-posn 0 0)
                            0
                            0
                            20))
         (set! all-game-objects
               (cons the-player
                     (build-list asteroid-count
                                 (λ (ignore) (new-asteroid)))))
         (big-bang all-game-objects
                   (on-key (λ (ignore key)
                             (begin (on-key-press key)
                                    all-game-objects)))
                   (on-release (λ (ignore key)
                                 (begin (on-key-release key)
                                        all-game-objects)))
                   (on-tick (lambda (game-objects)
                              (begin (for-each update! game-objects)
                                     (update-physics!)
                                     all-game-objects))
                            inter-frame-interval)
                   (to-draw (lambda (game-objects)
                              (foldl (lambda (object scene)
                                       (place-image (rotate (radians->rotation (game-object-orientation object))
                                                            (render object))
                                                    (posn-x (game-object-position object))
                                                    (posn-y (game-object-position object))
                                                    scene))
                                     (rectangle window-width window-height "solid" "black")
                                     game-objects))
                            800
                            600))))

;;;
;;; Event dispatch
;;;

(define (on-key-press key)
  (cond [(equal? key "up")
         (on-up-press)]
        [(equal? key "left")
         (on-left-press)]
        [(equal? key "right")
         (on-right-press)]
        [(equal? key " ")
         (on-space-press)]
        [else null]))

(define (on-key-release key)
  (cond [(equal? key "up")
         (on-up-release)]
        [(equal? key "left")
         (on-left-release)]
        [(equal? key "right")
         (on-right-release)]
        [else null]))

;;;
;;; Rendering (drawing on the screen)
;;;

(define (render object)
  (cond [(player? object)
         (isosceles-triangle 30 40 "solid" "aquamarine")]
        [(missile? object)
         (circle 1 "solid" "white")]
        [(asteroid? object)
         (circle (game-object-radius object) "solid" (asteroid-color object))]))

(define radians->rotation-coefficient
  (/ -360.0
     (* 2 pi)))

(define (radians->rotation radians)
  (+ (* radians radians->rotation-coefficient)
     -90))

;;;
;;; State update
;;;

(define (update! object)
  (cond [(player? object)
         (update-player! object)]
        [(missile? object)
         (update-missile! object)]
        [else null]))

(define (update-physics!)
  (begin (for-each (λ (object)
                     (begin (set-game-object-orientation! object
                                                          (+ (game-object-orientation object)
                                                             (* inter-frame-interval
                                                                (game-object-rotational-velocity object))))
                            (set-game-object-position! object
                                                       (local [(define new-position
                                                                 (posn-+ (posn-* inter-frame-interval
                                                                                 (game-object-velocity object))
                                                                         (game-object-position object)))]
                                                         (make-posn (wrap (posn-x new-position) window-width)
                                                                    (wrap (posn-y new-position) window-height))))))
                   all-game-objects)
         (handle-collisions all-game-objects)))

;;;
;;; Collision handling
;;;

(define (handle-collisions objects)
  (unless (empty? objects)
    (local [(define head (first objects))
            (define tail (rest objects))]
      (begin (for-each (λ (object)
                         (when (collided? head object)
                           (handle-collision head object)))
                       tail)
             (handle-collisions tail)))))

(define (collided? a b)
  (< (distance-squared (game-object-position a)
                       (game-object-position b))
     (squared (+ (game-object-radius a)
                 (game-object-radius b)))))

(define (handle-collision a b)
  (if (and (asteroid? a)
           (asteroid? b))
      (bounce a b)
      (begin (destroy! a)
             (destroy! b))))

(define (bounce a b)
  (local [(define mass-a (mass a))
          (define mass-b (mass b))
          (define vel-a (game-object-velocity a))
          (define vel-b (game-object-velocity b))
          (define one-over-mass (/ 1 (+ mass-a mass-b)))]
    (begin (set-game-object-velocity! a
                                      (posn-* one-over-mass
                                              (posn-+ (posn-* (- mass-a mass-b)
                                                              vel-a)
                                                      (posn-* (* 2 mass-b)
                                                              vel-b))))
           (set-game-object-velocity! b
                                      (posn-* one-over-mass
                                              (posn-+ (posn-* (- mass-b mass-a)
                                                              vel-b)
                                                      (posn-* (* 2 mass-a)
                                                              vel-a)))))))

(define (mass asteroid)
  (squared (game-object-radius asteroid)))

;;;
;;; Vector arithmetic
;;;

(define (posn-+ a b)
  (make-posn (+ (posn-x a)
                (posn-x b))
             (+ (posn-y a)
                (posn-y b))))

(define (posn-* k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(define (distance-squared p1 p2)
  (+ (squared (- (posn-x p1)
                 (posn-x p2)))
     (squared (- (posn-y p1)
                 (posn-y p2)))))


(define (forward-direction object)
  (local [(define o (game-object-orientation object))]
    (make-posn (cos o)
               (sin o))))

;;;
;;; Randomization
;;;

(define random-color
  (local [(define colors
            (list (color 255 0 0)
                  (color 0 255 0)
                  (color 0 0 255)
                  (color 128 128 0)
                  (color 128 0 129)
                  (color 0 128 128)))]
    (λ () (random-element colors))))

(define (random-element list)
  (list-ref list
            (random (length list))))

(define (random-float min max)
  (+ min
     (* (random)
        (- max min))))

(define (random-velocity)
  (make-posn (random-float -10 10)
             (random-float -10 10)))

;;;
;;; Other arithmetic utilities
;;;

(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else
         number]))

(define (squared x)
  (* x x))

