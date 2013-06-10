;;; Copyright (c) 2013 by Álvaro Castro Castilla
;;; OpenGL 2.1 2d skeleton

(define vertex-shader #<<end-of-shader

#version 120
attribute vec2 position;
attribute vec2 texCoord;

varying vec2 colorCoord;

uniform mat4 perspectiveMatrix;

void main()
{
  colorCoord = texCoord;
  gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
}

end-of-shader
)

(define fragment-shader #<<end-of-shader
   
#version 120

varying vec2 colorCoord;
uniform sampler2D colorTexture;

void main()
{
  gl_FragColor = texture2D(colorTexture, colorCoord);
}

end-of-shader
)
;;(call-with-output-file "level-one.dat" (lambda (f) (display (world-tile-vector world) f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO 
;;ANIMACIONES 

(define-type world (game-state unprintable:) player (tile-vector unprintable:) (collision-vector unprintable:) (sector-logic unprintable:) (vertex-vector unprintable:) (sounds unprintable:))
(define-type sounds jump-sound* touch-ground-sound* explosion-sound*)
(define-type sector-logic x y)
(define-type player x y  (width unprintable:) (height unprintable:) horizontal-state previous-horizontal-state vertical-state lives gravity is-collision? is-dead? sector animation-counter)
(define (make-world/init) 
  (make-world 'init-screen (make-player 40. 650. 24. 48. 'idle 'idle 'idle  3. 'down #f #f '1 0.) (read (open-input-file "level-test/level-test.dat")) (read (open-input-file "level-test/level-collisions-test.dat")) (make-sector-logic 0. 0.) '() (make-sounds 0. 0. 0.)))
(define tile-size 32.)


(define-syntax vertex-data-swap!
  (syntax-rules ()
    ((_ vertex-data a b)
     (let ((a-value (GLfloat*-ref vertex-data a))
           (b-value (GLfloat*-ref vertex-data b)))
       (GLfloat*-set! vertex-data a b-value)
       (GLfloat*-set! vertex-data b a-value)))))

(define (player-swap-vertical vertex-data)
  (vertex-data-swap! vertex-data 48002 48014)
  (vertex-data-swap! vertex-data 48003 48015)
  (vertex-data-swap! vertex-data 48006 48010)
  (vertex-data-swap! vertex-data 48007 48011))

;; Selects one of the 3 sprites of Pink Face, in the direction intended
;; down-right | down-left | up-right | up-left
(define (player-sprite-selector direction position vertex-data world)
  (let ((player-sprite-selector-template
         (lambda (v1a v1b v2a v2b v3a v3b v4a v4b)
           ;;Vertice 1
           (GLfloat*-set! vertex-data 48002 (+ v1a (* 0.0625 position)))
           (GLfloat*-set! vertex-data 48003 v1b)
           ;;Vertice 2
           (GLfloat*-set! vertex-data 48006 (+ v2a (* 0.0625 position)))
           (GLfloat*-set! vertex-data 48007 v2b)
           ;;Vertice 3
           (GLfloat*-set! vertex-data 48010 (+ v3a (* 0.0625 position)))
           (GLfloat*-set! vertex-data 48011 v3b)
           ;;Vertice 4
           (GLfloat*-set! vertex-data 48014 (+ v4a (* 0.0625 position)))
           (GLfloat*-set! vertex-data 48015 v4b))))

    (case direction 
      ((down-right)
       (player-sprite-selector-template 0.0000 0.0000
                                        0.0625 0.0000
                                        0.0625 0.0625
                                        0.0000 0.0625))
      ((down-left)
       (player-sprite-selector-template 0.0625 0.0000
                                        0.0000 0.0000
                                        0.0000 0.0625
                                        0.0625 0.0625))
      ((up-right)
       (player-sprite-selector-template 0.0000 0.0625
                                        0.0625 0.0625
                                        0.0625 0.0000
                                        0.0000 0.0000))
      ((up-left)
       (player-sprite-selector-template 0.0625 0.0625
                                        0.0000 0.0625
                                        0.0000 0.0000
                                        0.0625 0.0000)))))

(define (player-animation-updater vertex-data world)
  (when (eq? (player-horizontal-state (world-player world)) 'idle)
                (case (player-gravity (world-player world))
                  ((up)                   
                   (case (player-previous-horizontal-state (world-player world))
                     ((left)
                      (player-sprite-selector 'up-left 0. vertex-data world))
                     ((right)
                      (player-sprite-selector 'up-right 0. vertex-data world))))
                  ((down)                   
                   (case (player-previous-horizontal-state (world-player world))
                     ((left)
                      (player-sprite-selector 'down-left 0. vertex-data world))
                     ((right)
                      (player-sprite-selector 'down-right 0. vertex-data world))))))
  
  (unless (eq? (player-is-collision? (world-player world)) #f)
          
          (if (>= (player-animation-counter (world-player world))  40.)
                      (player-animation-counter-set! (world-player world) 0.)
                      (player-animation-counter-set! (world-player world) (+ (player-animation-counter (world-player world)) 1.)))
          
          (when (eq? (player-horizontal-state (world-player world)) 'left)
                (case (player-gravity (world-player world))
                  ((up)                   
                   (when (> (player-animation-counter (world-player world)) 30)
                         (player-sprite-selector 'up-left 0. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 30)
                          (> (player-animation-counter (world-player world)) 20))
                         (player-sprite-selector 'up-left 2. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 20)
                          (> (player-animation-counter (world-player world)) 10))
                         (player-sprite-selector 'up-left 0. vertex-data world))
                   (when (< (player-animation-counter (world-player world)) 10)
                         (player-sprite-selector 'up-left 1. vertex-data world)))
                  
                  ((down)                   
                   (when (> (player-animation-counter (world-player world)) 30)
                         (player-sprite-selector 'down-left 0. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 30)
                          (> (player-animation-counter (world-player world)) 20))
                         (player-sprite-selector 'down-left 2. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 20)
                          (> (player-animation-counter (world-player world)) 10))
                         (player-sprite-selector 'down-left 0. vertex-data world))
                   (when (< (player-animation-counter (world-player world)) 10)
                         (player-sprite-selector 'down-left 1. vertex-data world)))))

          (when (eq? (player-horizontal-state (world-player world)) 'right)
                (case (player-gravity (world-player world))
                  ((up)                   
                   (when (> (player-animation-counter (world-player world)) 30)
                         (player-sprite-selector 'up-right 0. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 30)
                          (> (player-animation-counter (world-player world)) 20))
                         (player-sprite-selector 'up-right 2. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 20)
                          (> (player-animation-counter (world-player world)) 10))
                         (player-sprite-selector 'up-right 0. vertex-data world))
                   (when (< (player-animation-counter (world-player world)) 10)
                         (player-sprite-selector 'up-right 1. vertex-data world)))
                  ((down)                   
                   (when (> (player-animation-counter (world-player world)) 30)
                         (player-sprite-selector 'down-right 0. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 30)
                          (> (player-animation-counter (world-player world)) 20))
                         (player-sprite-selector 'down-right 2. vertex-data world))
                   (when (and 
                          (< (player-animation-counter (world-player world)) 20)
                          (> (player-animation-counter (world-player world)) 10))
                         (player-sprite-selector 'down-right 0. vertex-data world))
                   (when (< (player-animation-counter (world-player world)) 10)
                         (player-sprite-selector 'down-right 1. vertex-data world)))))))

(define (update-sector-coordinates world)
  (case 
   (player-sector (world-player world))
   ((1)
    (sector-logic-x-set! (world-sector-logic world) 0.)
    (sector-logic-y-set! (world-sector-logic world) 0.))
   ((2)
    (sector-logic-x-set! (world-sector-logic world) 1280.)
    (sector-logic-y-set! (world-sector-logic world) 0.))
   ((3)
    (sector-logic-x-set! (world-sector-logic world) 0.)
    (sector-logic-y-set! (world-sector-logic world) 752.))
   ((4)
    (sector-logic-x-set! (world-sector-logic world) 1280.)
    (sector-logic-y-set! (world-sector-logic world) 752.))))

(define (sector-updater vertex-data  world)
  (cond
   ((and (> (player-x (world-player world)) 0)
         (< (player-x (world-player world)) 1280.)
         (> (player-y (world-player world)) 0)
         (< (player-y (world-player world)) 752.))
    (unless
     (eq? (player-sector (world-player world)) '1)
     (player-sector-set! (world-player world) '1)
     
     (update-sector-vector (player-sector (world-player world)) vertex-data world)))
   ((and (> (player-x (world-player world)) 1280.)
         (< (player-x (world-player world)) 2400.)
         (> (player-y (world-player world)) 0)
         (< (player-y (world-player world)) 752.))
    (unless
     (eq? (player-sector (world-player world)) '2)
     (player-sector-set! (world-player world) '2)
     (update-sector-vector (player-sector (world-player world)) vertex-data world)))
   ((and (> (player-x (world-player world)) 0)
         (< (player-x (world-player world)) 1280.)
         (> (player-y (world-player world)) 752.)
         (< (player-y (world-player world)) 1500.))
    (unless
     (eq? (player-sector (world-player world)) '3)
     (player-sector-set! (world-player world) '3)
     (update-sector-vector (player-sector (world-player world)) vertex-data world)))
   ((and (> (player-x (world-player world)) 1280.)
         (< (player-x (world-player world)) 2400.)
         (> (player-y (world-player world)) 752.)
         (< (player-y (world-player world)) 1504.))
    (unless
     (eq? (player-sector (world-player world)) '4)
     (player-sector-set! (world-player world) '4)
     (update-sector-vector (player-sector (world-player world)) vertex-data world)))))

(define (player-ground-collision world)
  (when (and(eq? (player-gravity (world-player world)) 'up) 
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(floor (/  (player-x (world-player world)) tile-size)))) 'ground)
            (eq? (vector-ref (vector-ref (world-collision-vector world)
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size))))
                             (inexact->exact(ceiling (/  ( + (player-x (world-player world)) 10. ) tile-size)))) 'ground))
        (player-is-collision?-set! (world-player world) #t))

  (when (and (eq? (player-gravity (world-player world)) 'down)
             (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                          (inexact->exact (floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                              (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground)
             (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                          (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                              (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'ground))
        (player-is-collision?-set! (world-player world) #t)))

(define (player-death-collision explosion-sound* world)
  (when (or (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(floor (/  (player-x (world-player world)) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world)
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(ceiling (/  ( - (player-x (world-player world)) 10. ) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                             (inexact->exact(floor (/ (+(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                             (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact (floor (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact (ceiling (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world)
                                         (inexact->exact (floor (/ (+ (player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                             (inexact->exact(floor (/ (- (player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(ceiling (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (+(player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes))
        (Mix_PlayChannel 1 explosion-sound* 0)
        (player-is-dead?-set! (world-player world) #t)))

(define (player-goal-collision vertex-data world)
  (when (or (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(floor (/  (player-x (world-player world)) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world)
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(ceiling (/  ( - (player-x (world-player world)) 10. ) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                             (inexact->exact(floor (/ (+(player-x (world-player world)) 3.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                             (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact (floor (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact (ceiling (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world)
                                         (inexact->exact (floor (/ (+ (player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                             (inexact->exact(floor (/ (- (player-x (world-player world)) 3.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(ceiling (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'goal)
            (eq? (vector-ref (vector-ref (world-collision-vector world) 
                                         (inexact->exact(floor (/ (+(player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'goal))
        (player-x-set! (world-player world) 1270.)
        (player-y-set! (world-player world) 740.)
        (world-tile-vector-set! world (read (open-input-file "level-two/level-two.dat")))
        (world-collision-vector-set! world (read (open-input-file "level-two/level-collisions-two.dat")))
        (update-sector-vector (player-sector (world-player world)) vertex-data world)
        (player-sprite-selector 'down-left 0. vertex-data world)
        (world-game-state-set! world 'level-two)))

(define (player-position-updater vertex-data world)
  (when (and (eq? (player-gravity (world-player world)) 'up) 
             (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                              (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                                  (inexact->exact(floor (/  (player-x (world-player world)) tile-size)))) 'ground))
             (not(eq? (vector-ref (vector-ref (world-collision-vector world)
                                              (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                                  (inexact->exact(ceiling (/  ( - (player-x (world-player world)) 10. ) tile-size)))) 'ground)))
        (player-y-set! (world-player world) ( - (player-y (world-player world)) 3.))
        (player-movement-increment 'up vertex-data))

  (when (and (eq? (player-gravity (world-player world)) 'down)
             (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                              (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                                  (inexact->exact(floor (/ (+(player-x (world-player world)) 3.) tile-size))))  'ground))
             (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                              (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                                  (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'ground)))
        (player-y-set! (world-player world) ( + (player-y (world-player world)) 3.))
        (player-movement-increment 'down vertex-data))

  (when (and(eq? (player-horizontal-state (world-player world)) 'left)
            (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                             (inexact->exact (floor (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                             (inexact->exact (ceiling (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact(floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-collision-vector world)
                                             (inexact->exact (floor (/ (+ (player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                                 (inexact->exact(floor (/ (- (player-x (world-player world)) 3.) tile-size)))) 'ground)))
        (player-x-set! (world-player world) ( - (player-x (world-player world)) 3.))
        (player-movement-increment 'left vertex-data))

  (when (and(eq? (player-horizontal-state (world-player world)) 'right)
            (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                             (inexact->exact(floor (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                             (inexact->exact(ceiling (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-collision-vector world) 
                                             (inexact->exact(floor (/ (+(player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                                 (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground)))
        (player-x-set! (world-player world) ( + (player-x (world-player world)) 3.))
        (player-movement-increment 'right vertex-data)))

(define (player-sector-updater vertex-data world)
  (GLfloat*-set! vertex-data 48000 (-(- (player-x (world-player world)) (sector-logic-x (world-sector-logic world))) 15.))
  (GLfloat*-set! vertex-data 48001 (- (- (+ (player-y (world-player world)) (player-height (world-player world))) 48.) (sector-logic-y (world-sector-logic world))))
  (GLfloat*-set! vertex-data 48004 (-(+(- (+ (player-x (world-player world)) 32.) (sector-logic-x (world-sector-logic world))) 24.)15.))
  (GLfloat*-set! vertex-data 48005 (- (- (+ (player-y (world-player world)) (player-height (world-player world))) 48.) (sector-logic-y (world-sector-logic world))))
  (GLfloat*-set! vertex-data 48008 (-(+(- (+ (player-x (world-player world)) 32.) (sector-logic-x (world-sector-logic world))) 24.)15.))
  (GLfloat*-set! vertex-data 48009 (- (+ (player-y (world-player world)) (player-height (world-player world)))(sector-logic-y (world-sector-logic world))))
  (GLfloat*-set! vertex-data 48012 (-(- (player-x (world-player world)) (sector-logic-x (world-sector-logic world)))15.))
  (GLfloat*-set! vertex-data 48013 (- (+ (player-y (world-player world)) (player-height (world-player world))) (sector-logic-y (world-sector-logic world)))))

(define (player-movement-increment direction vertex-data)
  (let ((GLfloat*-increment
         (lambda (n x) (GLfloat*-set! vertex-data n (+ (GLfloat*-ref vertex-data n) x)))))
    (case direction
      ((right)
       (GLfloat*-increment 48000 3.0)
       (GLfloat*-increment 48004 3.0)
       (GLfloat*-increment 48008 3.0)
       (GLfloat*-increment 48012 3.0)
       (GLfloat*-increment 48016 3.0)
       (GLfloat*-increment 48020 3.0)
       (GLfloat*-increment 48024 3.0)
       (GLfloat*-increment 48028 3.0))
      ((left)
       (GLfloat*-increment 48000 -3.0)
       (GLfloat*-increment 48004 -3.0)
       (GLfloat*-increment 48008 -3.0)
       (GLfloat*-increment 48012 -3.0)
       (GLfloat*-increment 48016 -3.0)
       (GLfloat*-increment 48020 -3.0)
       (GLfloat*-increment 48024 -3.0)
       (GLfloat*-increment 48028 -3.0))
      ((down)
       (GLfloat*-increment 48001 3.0)
       (GLfloat*-increment 48005 3.0)
       (GLfloat*-increment 48009 3.0)
       (GLfloat*-increment 48013 3.0)
       (GLfloat*-increment 48017 3.0)
       (GLfloat*-increment 48021 3.0)
       (GLfloat*-increment 48025 3.0)
       (GLfloat*-increment 48029 3.0))
      ((up)
       (GLfloat*-increment 48001 -3.0)
       (GLfloat*-increment 48005 -3.0)
       (GLfloat*-increment 48009 -3.0)
       (GLfloat*-increment 48013 -3.0)
       (GLfloat*-increment 48017 -3.0)
       (GLfloat*-increment 48021 -3.0)
       (GLfloat*-increment 48025 -3.0)
       (GLfloat*-increment 48029 -3.0)))))

(define (level-changer world)
  (when (eq? (player-is-dead? (world-player world)) #t)
        (world-game-state-set! world 'death)))

(define (sector-background-initializer vertex-data world)
  (GLfloat*-set! vertex-data 0 0.)
  (GLfloat*-set! vertex-data 1 0.)
  (GLfloat*-set! vertex-data 4 1280.)
  (GLfloat*-set! vertex-data 5 0.)
  (GLfloat*-set! vertex-data 8 1280.)
  (GLfloat*-set! vertex-data 9 752.)
  (GLfloat*-set! vertex-data 12 0.)
  (GLfloat*-set! vertex-data 13 752.))

(define (sector-background-updater sector vertex-data world)
  (let ((sector-background-updater-template 
         (lambda (a b c d e f g h ) 
           (GLfloat*-set! vertex-data 2 a)
           (GLfloat*-set! vertex-data 3 b)
           (GLfloat*-set! vertex-data 6 c)
           (GLfloat*-set! vertex-data 7 d)
           (GLfloat*-set! vertex-data 10 e)
           (GLfloat*-set! vertex-data 11 f)
           (GLfloat*-set! vertex-data 14 g)
           (GLfloat*-set! vertex-data 15 h))))
    (case sector
      ((1)
       (sector-background-updater-template 0.0000 0.3125
                                           0.3125 0.3125
                                           0.3125 0.49609375
                                           0.0000 0.49609375))
      ((2)
       (sector-background-updater-template 0.3125 0.3125
                                           0.6250 0.3125
                                           0.6250 0.49609375
                                           0.3125 0.49609375))
      ((3)
       (sector-background-updater-template 0.0000 0.49609375
                                           0.3125 0.49609375
                                           0.3125 0.67968750
                                           0.0000 0.67968750))
      ((4)
       (sector-background-updater-template 0.3125 0.49609375
                                           0.6250 0.49609375
                                           0.6250 0.67968750
                                           0.3125 0.67968750))
      ((5)
       (sector-background-updater-template 0.6250 0.3125
                                           0.9375 0.3125
                                           0.9375 0.49609375
                                           0.6250 0.49609375))
      ((6)
       (sector-background-updater-template 0.6250 0.49609375
                                           0.9375 0.49609375
                                           0.9375 0.67968750
                                           0.6250 0.67968750)))))

(define (update-sector-vector sector vertex-data world)
  (when 
   (eq? (world-game-state world) 'level-one)
   (case sector
     ((1)
      ;;(sector-background-updater '1 vertex-data world)
      (vector-copy (read (open-input-file "level-one/level-one-sector1.dat")) vertex-data world))
     ((2)
      (vector-copy (read (open-input-file "level-one/level-one-sector2.dat")) vertex-data world))
     ((3)
      (vector-copy (read (open-input-file "level-one/level-one-sector3.dat")) vertex-data world))
     ((4)
      (vector-copy (read (open-input-file "level-one/level-one-sector4.dat")) vertex-data world))))
  (when 
   (eq? (world-game-state world) 'level-two)
   (case sector
     ((1)
      ;;(sector-background-updater '1 vertex-data world)
      (vector-copy (read (open-input-file "level-two/level-two-sector1.dat")) vertex-data world))
     ((2)
      (vector-copy (read (open-input-file "level-two/level-two-sector2.dat")) vertex-data world))
     ((3)
      (vector-copy (read (open-input-file "level-two/level-two-sector3.dat")) vertex-data world))
     ((4)
      (vector-copy (read (open-input-file "level-two/level-two-sector4.dat")) vertex-data world))))
  )

(define (vector-copy vector vertex-data world)
  (let recur ((counter 0))
    (cond
     ((eq? counter 70000))
     ((>= counter  (f32vector-length vector))
      (GLfloat*-set! vertex-data counter 0.)
      (recur (+ counter 1)))
     (else
      (GLfloat*-set! vertex-data counter (f32vector-ref vector counter ))
      (recur (+ counter 1))))))

(define (game-updater vertex-data explosion-sound* world)
  (case (world-game-state world)
    ((init-screen)
     (sector-background-initializer vertex-data world)
     (sector-background-updater '5 vertex-data world))
    ((instructions)
     (sector-background-updater '6 vertex-data world))
    ((death)
     (player-is-dead?-set! (world-player world) #f)
     (world-game-state-set! world 'level-one)
     (player-x-set! (world-player world) 40.)
     (player-y-set! (world-player world) 650.)
     (player-gravity-set! (world-player world) 'down)
     ;;(player-lives-set! (world-player world) (- (player-lives (world-player world)) 1.))
     (when (< (player-lives (world-player world)) 0.)
           (world-game-state-set! world 'game-over)))
    ((game-over)
     (vector-copy '#f32(0.) vertex-data world))
    ((level-one)
     (sector-updater vertex-data world)
     (player-sector-updater vertex-data world)
     (update-sector-coordinates world)
     (level-changer world)
     (player-position-updater vertex-data world)
     (player-animation-updater vertex-data world)
     (player-ground-collision world)
     (player-death-collision explosion-sound* world)
     (player-goal-collision vertex-data world))
    ((level-two)
     (sector-updater vertex-data world)
     (player-sector-updater vertex-data world)
     (update-sector-coordinates world)
     (level-changer world)
     (player-position-updater vertex-data world)
     (player-animation-updater vertex-data world)
     (player-ground-collision world)
     (player-death-collision explosion-sound* world))))

(define (main)
  (let ((init-screen-width 1280)
        (init-screen-height 752)
        (screen-width* (alloc-int* 1))
        (screen-height* (alloc-int* 1))
        (world (make-world/init)))
    (when (< (SDL_Init SDL_INIT_VIDEO) 0) report: (fusion:error "Couldn't initialize SDL!"))
    ;; SDL
    (let ((win (SDL_CreateWindow
                "THIS A NEW, BETTER GAME. A GAME FOR EVERYONE."
                SDL_WINDOWPOS_CENTERED
                SDL_WINDOWPOS_CENTERED
                (cond-expand (mobile 0) (else init-screen-width))
                (cond-expand (mobile 0) (else init-screen-height))
                SDL_WINDOW_OPENGL)))
      (unless win (fusion:error "Unable to create render window" (SDL_GetError)))
      (SDL_GetWindowSize win screen-width* screen-height*)
      (let ((screen-width (*->int screen-width*))
            (screen-height (*->int screen-height*))
            (ctx (SDL_GL_CreateContext win)))
        (SDL_Log (string-append "SDL screen size: " (object->string screen-width) " x " (object->string screen-height)))
        ;; OpenGL
        (SDL_Log (string-append "OpenGL Version: " (*->string (glGetString GL_VERSION))))
        (SDL_Log "Using API OpenGL Version: 2.1 - GL Shading Language Version: 1.2")
        ;; Glew: initialize extensions
        (glewInit)
        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)

        ;; SDL TTF
        ;; (unless (= 0 (TTF_Init))
        ;;         (fusion:error (string-append "Unable to initialize True Type Fonts system -- " (TTF_GetError))))

        ;; SDL Mixer
        (unless (= 0 (Mix_OpenAudio MIX_DEFAULT_FREQUENCY MIX_DEFAULT_FORMAT 2 1024))
                (fusion:error (string-append "Unable to initialize sound system -- " (Mix_GetError))))
        
        ;; PRECHARGE stuff

        ;; Generate programs, buffers, textures
        
        (let* ((perspective-matrix (matrix:* (make-translation-matrix -1.0 1.0 0.0)
                                             (matrix:* (make-scaling-matrix (/ 2.0 screen-width) (/ -2.0 screen-height) 1.0)
                                                       (make-identity-matrix))))
               (position-buffer-object-id* (alloc-GLuint* 1))
               (main-vao-id* (alloc-GLuint* 1))
               (surface-id* (alloc-GLuint* 1))
               (texture-id* (alloc-GLuint* 1))
               (texture-unit 0)
               (sampler-id* (alloc-GLuint* 1))
               
               ;; VECTOR
               (vertex-data-vector (make-f32vector 80000 0.))

               (vertex-data (f32vector->GLfloat* vertex-data-vector))
               (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                              (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
               (shader-program (fusion:create-program shaders))
               ;;(texture-image* (SDL_LoadBMP "assets/128x32.bmp"))
               (texture-image* (or (IMG_Load "assets/PlantillaPFGAME-FINAL.png")
                                       (fusion:error (string-append "Unable to load texture image -- " (IMG_GetError)))))
               (background-music* (or (Mix_LoadMUS "assets/Daft-Punk-Get-Lucky.ogg")
                                      (fusion:error (string-append "Unable to load OGG music -- " (Mix_GetError)))))
               (jump-sound* (or (Mix_LoadWAV "assets/Jump.wav")
                                (fusion:error (string-append "Unable to load WAV chunk -- " (Mix_GetError)))))
               (touch-ground-sound* (or (Mix_LoadWAV "assets/Jump.wav")
                                (fusion:error (string-append "Unable to load WAV chunk -- " (Mix_GetError)))))
               (explosion-sound* (or (Mix_LoadWAV "assets/Explosion.wav")
                                (fusion:error (string-append "Unable to load WAV chunk -- " (Mix_GetError))))))
          ;; Clean up shaders once the program has been compiled and linked
          (for-each glDeleteShader shaders)

          ;; Texture
          (glGenTextures 1 texture-id*)
          (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
          (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA
                        (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                        0 GL_RGBA GL_UNSIGNED_BYTE
                        (SDL_Surface-pixels texture-image*))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0)
          (glBindTexture GL_TEXTURE_2D 0)
          (SDL_FreeSurface texture-image*)
          (glEnable GL_BLEND)
          (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)

          ;; Uniforms
          (glUseProgram shader-program)
          (glUniformMatrix4fv (glGetUniformLocation shader-program "perspectiveMatrix")
                              1 GL_FALSE
                              (matrix->GLfloat*
                               (matrix:map exact->inexact
                                           perspective-matrix)))
          (glUniform1i (glGetUniformLocation shader-program "colorTexture") texture-unit)
          (glUseProgram 0)

          ;; Sampler
          (glGenSamplers 1 sampler-id*)
          (let ((sampler-id (*->GLuint sampler-id*)))
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
            (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_LINEAR)
            (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_LINEAR))
          
          ;; Vertex Array Object
          (glGenBuffers 1 position-buffer-object-id*)
          (let ((position-buffer-object-id (*->GLuint position-buffer-object-id*)))
            ;; Upload buffer
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            (glBufferData GL_ARRAY_BUFFER
                          (* (f32vector-length vertex-data-vector) GLfloat-size)
                          vertex-data
                          GL_DYNAMIC_DRAW)
            ;; Create VAO
            (glGenVertexArrays 1 main-vao-id*)
            (glBindVertexArray (*->GLuint main-vao-id*))
            (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
            
            (let ((position-attr (glGetAttribLocation shader-program "position"))
                  (texture-coordinates-attr (glGetAttribLocation shader-program "texCoord")))
              (glEnableVertexAttribArray position-attr)
              (glVertexAttribPointer position-attr 2 GL_FLOAT GL_FALSE (* 4 GLfloat-size) #f)
              (glEnableVertexAttribArray texture-coordinates-attr)
              (glVertexAttribPointer texture-coordinates-attr 2
                                     GL_FLOAT GL_FALSE
                                     (* 4 GLfloat-size) (integer->void* (* 2 GLfloat-size))))
            
            (glBindBuffer GL_ARRAY_BUFFER 0)
            (glBindVertexArray 0)
            
            ;; Game loop
            (let ((event* (alloc-SDL_Event)))
              (call/cc
               (lambda (quit)
                 (let main-loop ()
                   (let event-loop ()
                     (when (= 1 (SDL_PollEvent event*))
                           (let ((event-type (SDL_Event-type event*)))
                             (cond
                              ((= event-type SDL_KEYDOWN)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond ((= key SDLK_ESCAPE)
                                        (quit))
                                       ((and (= key SDLK_SPACE) (eq? (world-game-state world) 'init-screen))
                                        (world-game-state-set! world 'instructions))
                                       ((and (= key SDLK_SPACE) (eq? (world-game-state world) 'instructions))
                                        (world-game-state-set! world 'level-one)
                                        (world-tile-vector-set! world (read (open-input-file "level-one/level-one.dat")))
                                        (world-collision-vector-set! world (read (open-input-file "level-one/level-collisions-one.dat")))
                                        (update-sector-vector (player-sector (world-player world)) vertex-data world)
                                        (player-sprite-selector 'down-right 0. vertex-data world)
                                        
                                        (unless (Mix_PlayMusic background-music* -1)
                                                (fusion:error (string-append "Unable to play OGG music -- " (Mix_GetError)))))
                                       ((and (= key SDLK_SPACE) (not (eq? (world-game-state world) 'init-screen))(eq? (player-is-collision? (world-player world)) #t))
                                        (Mix_PlayChannel 1 jump-sound* 0)
                                        (player-swap-vertical vertex-data)
                                        (case (player-gravity (world-player world))
                                          ((up)
                                           (player-is-collision?-set! (world-player world) #f)
                                           (player-gravity-set! (world-player world) 'down))
                                          ((down)
                                           (player-is-collision?-set! (world-player world) #f)
                                           (player-gravity-set! (world-player world) 'up))))
                                       ((and (= key SDLK_RIGHT) (not (eq? (world-game-state world) 'init-screen)))
                                        (player-previous-horizontal-state-set! (world-player world) 'right)
                                        (player-horizontal-state-set! (world-player world) 'right))
                                       ((and (= key SDLK_LEFT) (not (eq? (world-game-state world) 'init-screen)))
                                        (player-previous-horizontal-state-set! (world-player world) 'left)
                                        (player-horizontal-state-set! (world-player world) 'left)
                                        (case (player-gravity (world-player world))
                                          ((up)
                                           (player-sprite-selector 'up-left 1. vertex-data world))
                                          ((down)
                                           (player-sprite-selector 'down-left 1. vertex-data world))))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              ((= event-type SDL_KEYUP)
                               (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key up")
                               (let* ((kevt* (SDL_Event-key event*))
                                      (key (SDL_Keysym-sym
                                            (SDL_KeyboardEvent-keysym kevt*))))
                                 (cond ((and (= key SDLK_RIGHT) (eq? (player-horizontal-state (world-player world)) 'right)) 
                                        (player-horizontal-state-set! (world-player world) 'idle))
                                       ((and (= key SDLK_LEFT) (eq? (player-horizontal-state (world-player world)) 'left)) 
                                        (player-horizontal-state-set! (world-player world) 'idle))
                                       (else
                                        (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))
                              (else #f)))
                           (event-loop)))

                   ;; -- Game logic -- 

                   ;; (println (string-append "animation-counter: " (object->string (player-animation-counter (world-player world)))))
                   (game-updater vertex-data explosion-sound* world)
                   
                   ;; -- Draw --
                   (glClearColor 0.0 0.0 0.0 0.0)
                   (glClear GL_COLOR_BUFFER_BIT)
                   
                   (glActiveTexture (+ GL_TEXTURE0 texture-unit))
                   (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
                   (glBindSampler texture-unit (*->GLuint sampler-id*))

                   ;; Begin VAO
                   (glBindVertexArray (*->GLuint main-vao-id*))
                   ;; Update vertex data buffer
                   (glBindBuffer GL_ARRAY_BUFFER position-buffer-object-id)
                   (glBufferSubData GL_ARRAY_BUFFER
                                    0
                                    (* (f32vector-length vertex-data-vector) GLfloat-size)
                                    vertex-data)
                   
                   (glUseProgram shader-program)
                   (glDrawArrays GL_QUADS 0 (/(f32vector-length vertex-data-vector) 4))
                   (glUseProgram 0)
                   (glBindVertexArray 0)
                   ;; End VAO
                   
                   (SDL_GL_SwapWindow win)
                   (main-loop))))
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit)))))))
  (##gc))

