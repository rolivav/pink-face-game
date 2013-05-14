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
;; CAMBIAR player-death-collision CON CASE PARA QUE CHEQUEE COLISION CON TODO. ESO ES PORTALES Y LLAVES AND SHIT.

(define-type world (game-state unprintable:) player (tile-vector unprintable:) (sector-logic unprintable:) (vertex-vector unprintable:))
(define-type sector-logic x y)
(define-type player x y  (width unprintable:) (height unprintable:) horizontal-state vertical-state lives gravity is-collision? is-dead? sector)
(define (make-world/init) 
  (make-world 'init-screen (make-player 40. 650. 24. 48. 'idle 'idle  3. 'down #f #f '1) (read (open-input-file "level-one.dat")) (make-sector-logic 0. 0.) '()))
(define tile-size 32.)


(define (insert-full-bmp x y width height world)
  (let ((lst (list x y 0.0 0.0
                   x (+ y height) 0.0 1.0
                   (+ x width) (+ y height) 1.0 1.0
                   (+ x width) y 1.0 0.0 )))
    (world-vertex-vector-set! world 
                              (append (world-vertex-vector world) 
                                      lst))))

(define (insert-section-bmp x y width height section world)
  (world-vertex-vector-set! world 
                            (append (world-vertex-vector world) 
                                    (list x y (+ 0.1 (* 0.25 section)) 0.0
                                          x (+ y height) (+ 0.1 (* 0.25 section)) 1.0
                                          (+ x width) (+ y height) (+ 0.25  (* 0.25 section)) 1.0
                                          (+ x width) y (+ 0.25  (* 0.25 section)) 0.0 ))))

(define (player-init vertex-data world)
  (begin
    (GLfloat*-set! vertex-data 9008 (player-x (world-player world)))
    (GLfloat*-set! vertex-data 9009 (player-y (world-player world)))
    (GLfloat*-set! vertex-data 9010 (+ 0.1 (* 0.25 3.)))
    (GLfloat*-set! vertex-data 9011 0.0)
    (GLfloat*-set! vertex-data 9012 (player-x (world-player world)))
    (GLfloat*-set! vertex-data 9013 (+ (player-y (world-player world)) (player-height (world-player world))))
    (GLfloat*-set! vertex-data 9014 (+ 0.1 (* 0.25 3.)))
    (GLfloat*-set! vertex-data 9015 1.0)
    (GLfloat*-set! vertex-data 9016 (+ (player-x (world-player world)) (player-width (world-player world))))
    (GLfloat*-set! vertex-data 9017 (+ (player-y (world-player world)) (player-height (world-player world))))
    (GLfloat*-set! vertex-data 9018 (+ 0.25  (* 0.25 3.)))
    (GLfloat*-set! vertex-data 9019 1.0)
    (GLfloat*-set! vertex-data 9020 (+ (player-x (world-player world)) (player-width (world-player world))))
    (GLfloat*-set! vertex-data 9021 (player-y (world-player world)))
    (GLfloat*-set! vertex-data 9022 (+ 0.25  (* 0.25 3.)))
    (GLfloat*-set! vertex-data 9023 0.0)))

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
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(floor (/  (player-x (world-player world)) tile-size))))  'ground)
            (eq? (vector-ref (vector-ref (world-tile-vector world)
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size))))
                             (inexact->exact(ceiling (/  ( + (player-x (world-player world)) 10. ) tile-size)))) 'ground))
        (player-is-collision?-set! (world-player world) #t))

  (when (and (eq? (player-gravity (world-player world)) 'down)
             (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                          (inexact->exact (floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                              (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground)
             (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                          (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                              (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'ground))
        (player-is-collision?-set! (world-player world) #t)))

(define (player-death-collision world)
  (when (or (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(floor (/  (player-x (world-player world)) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world)
                                         (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                             (inexact->exact(ceiling (/  ( - (player-x (world-player world)) 10. ) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                             (inexact->exact(floor (/ (+(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                             (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact (floor (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact (ceiling (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world)
                                         (inexact->exact (floor (/ (+ (player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                             (inexact->exact(floor (/ (- (player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(floor (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(ceiling (/ (player-y (world-player world)) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes)
            (eq? (vector-ref (vector-ref (world-tile-vector world) 
                                         (inexact->exact(floor (/ (+(player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                             (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'spikes))
        (player-is-dead?-set! (world-player world) #t)))


(define (player-position-updater vertex-data world)
  (when (and (eq? (player-gravity (world-player world)) 'up) 
             (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                              (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                                  (inexact->exact(floor (/  (player-x (world-player world)) tile-size)))) 'ground))
             (not(eq? (vector-ref (vector-ref (world-tile-vector world)
                                              (inexact->exact(floor (/ (- (player-y (world-player world)) 3.) tile-size)))) 
                                  (inexact->exact(ceiling (/  ( - (player-x (world-player world)) 10. ) tile-size)))) 'ground)))
        (player-y-set! (world-player world) ( - (player-y (world-player world)) 3.))
        (player-movement-increment 'up vertex-data))

  (when (and (eq? (player-gravity (world-player world)) 'down) 
             (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                              (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                                  (inexact->exact(floor (/ (+(player-x (world-player world)) 3.) tile-size)))) 'ground))
             (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                              (inexact->exact(floor (/ (+ (+ (player-y (world-player world)) (player-height (world-player world))) 3.) tile-size)))) 
                                  (inexact->exact(ceiling (/ (- (player-x (world-player world)) 10.) tile-size)))) 'ground)))
        (player-y-set! (world-player world) ( + (player-y (world-player world)) 3.))
        (player-movement-increment 'down vertex-data))

  (when (and(eq? (player-horizontal-state (world-player world)) 'left)
            (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                             (inexact->exact (floor (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact (floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                             (inexact->exact (ceiling (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact(floor (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-tile-vector world)
                                             (inexact->exact (floor (/ (+ (player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                                 (inexact->exact(floor (/ (- (player-x (world-player world)) 3.) tile-size)))) 'ground)))
        (player-x-set! (world-player world) ( - (player-x (world-player world)) 3.))
        (player-movement-increment 'left vertex-data))

  (when (and(eq? (player-horizontal-state (world-player world)) 'right)
            (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                             (inexact->exact(floor (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                             (inexact->exact(ceiling (/ (player-y (world-player world)) tile-size)))) 
                                 (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground))
            (not(eq? (vector-ref (vector-ref (world-tile-vector world) 
                                             (inexact->exact(floor (/ (+(player-y (world-player world)) (player-height (world-player world))) tile-size)))) 
                                 (inexact->exact(ceiling (/ (-(player-x (world-player world)) 3.) tile-size)))) 'ground)))
        (player-x-set! (world-player world) ( + (player-x (world-player world)) 3.))
        (player-movement-increment 'right vertex-data)))

(define (player-sector-updater vertex-data world)
  (begin
    (GLfloat*-set! vertex-data 9008 (- (player-x (world-player world)) (sector-logic-x (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9009 (- (player-y (world-player world)) (sector-logic-y (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9012 (- (+ (player-x (world-player world)) (player-width (world-player world))) (sector-logic-x (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9013 (- (player-y (world-player world)) (sector-logic-y (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9016 (- (+ (player-x (world-player world)) (player-width (world-player world))) (sector-logic-x (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9017 (- (+ (player-y (world-player world)) (player-height (world-player world)))(sector-logic-y (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9020 (- (player-x (world-player world)) (sector-logic-x (world-sector-logic world))))
    (GLfloat*-set! vertex-data 9021 (- (+ (player-y (world-player world)) (player-height (world-player world))) (sector-logic-y (world-sector-logic world))))))

(define (player-movement-increment direction vertex-data)
  (let ((GLfloat*-increment
         (lambda (n x) (GLfloat*-set! vertex-data n (+ (GLfloat*-ref vertex-data n) x)))))
    (case direction
      ((right)
       (GLfloat*-increment 9008 3.0)
       (GLfloat*-increment 9012 3.0)
       (GLfloat*-increment 9016 3.0)
       (GLfloat*-increment 9020 3.0)
       (GLfloat*-increment 9024 3.0)
       (GLfloat*-increment 9028 3.0)
       (GLfloat*-increment 9032 3.0)
       (GLfloat*-increment 9036 3.0))
      ((left)
       (GLfloat*-increment 9008 -3.0)
       (GLfloat*-increment 9012 -3.0)
       (GLfloat*-increment 9016 -3.0)
       (GLfloat*-increment 9020 -3.0)
       (GLfloat*-increment 9024 -3.0)
       (GLfloat*-increment 9028 -3.0)
       (GLfloat*-increment 9032 -3.0)
       (GLfloat*-increment 9036 -3.0))
      ((down)
       (GLfloat*-increment 9009 3.0)
       (GLfloat*-increment 9013 3.0)
       (GLfloat*-increment 9017 3.0)
       (GLfloat*-increment 9021 3.0)
       (GLfloat*-increment 9025 3.0)
       (GLfloat*-increment 9029 3.0)
       (GLfloat*-increment 9033 3.0)
       (GLfloat*-increment 9037 3.0))
      ((up)
       (GLfloat*-increment 9009 -3.0)
       (GLfloat*-increment 9013 -3.0)
       (GLfloat*-increment 9017 -3.0)
       (GLfloat*-increment 9021 -3.0)
       (GLfloat*-increment 9025 -3.0)
       (GLfloat*-increment 9029 -3.0)
       (GLfloat*-increment 9033 -3.0)
       (GLfloat*-increment 9037 -3.0)))))

(define (level-changer world)
  (when (eq? (player-is-dead? (world-player world)) #t)
        (world-game-state-set! world 'death)))

(define (print-sector vec sector size world)
  (let ((print-sector-template
         (lambda (a b c d pos1 pos2)
           (let recur ((counter1 a) (counter2 b))
             (cond
              ((and (eq? counter1 c) (eq? counter2 d))
               (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 2. world) )
              ((eq? (vector-ref (vector-ref vec counter1) counter2) 'ground)
               (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 2. world)
               (if (eq? counter2 d) 
                   (recur (+ counter1 1) 0)
                   (recur counter1 (+ counter2 1))))
              ((eq? (vector-ref (vector-ref vec counter1) counter2) 'spikes)
               (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 1. world)
               (if (eq? counter2 d)
                   (recur (+ counter1 1) 0)
                   (recur counter1 (+ counter2 1))))
              
              ;; ((eq? (vector-ref (vector-ref vec counter1) counter2) 'goal)
              ;;  (cairo_set_source_rgba cr 1.0 0.0 1.0 1.0)
              ;;  (cairo_rectangle cr (pos1 counter2) (pos2 counter1) size size)
              ;;  (cairo_fill cr)
              ;;  (if (eq? counter2 d)
              ;;      (recur (+ counter1 1) 0)
              ;;      (recur counter1 (+ counter2 1))))
              ;; ((eq? (vector-ref (vector-ref vec counter1) counter2) 'key)
              ;;  (cairo_set_source_rgba cr 1.0 1.0 0.0 1.0)
              ;;  (cairo_rectangle cr (pos1 counter2) (pos2 counter1) size size)
              ;;  (cairo_fill cr)
              ;;  (if (eq? counter2 d)
              ;;      (recur (+ counter1 1) 0)
              ;;      (recur counter1 (+ counter2 1))))
              ;; ((eq? (vector-ref (vector-ref vec counter1) counter2) 'start)
              ;; (insert-section-bmp (pos1 counter2) (pos2 counter1) size size 3. world)
              ;;  (if (eq? counter2 d)
              ;;      (recur (+ counter1 1) 0)
              ;;      (recur counter1 (+ counter2 1))))
              ;; ((eq? (vector-ref (vector-ref vec counter1) counter2) 'inverter)
              ;;  (cairo_set_source_rgba cr 1.0 0.0 1.0 1.0)
              ;;  (cairo_rectangle cr (pos1 counter2) (pos2 counter1) size size)
              ;;  (cairo_fill cr)
              ;;  (if (eq? counter2 d)
              ;;      (recur (+ counter1 1) 0)
              ;;      (recur counter1 (+ counter2 1))))
              (else
               (if (eq? counter2 d) 
                   (recur (+ counter1 1) 0) 
                   (recur counter1 (+ counter2 1)))))))))
   (case sector
     ((1)
      (print-sector-template 0 0 24 39
                             (lambda (x) (* (exact->inexact x) size))
                             (lambda (x) (* (exact->inexact x) size))))
     ((2)
      (print-sector-template 0 39 24 79
                             (lambda (x) (-(* (exact->inexact x) size) 1280.))
                             (lambda (x) (* (exact->inexact x) size))))
     ((3)
      (print-sector-template 23 0 46 40
                             (lambda (x) (* (exact->inexact x) size))
                             (lambda (x) (-(* (exact->inexact x) size) 752.))))
     ((4)
      (print-sector-template 23 39 46 79
                             (lambda (x) (-(* (exact->inexact x) size) 1280.))
                             (lambda (x) (-(* (exact->inexact x) size) 752.)))))))

(define (update-sector-vector sector vertex-data world)
  (case sector
    ((1)
     (vector-copy (read (open-input-file "sector1-level1.dat")) vertex-data world))
    ((2)
     (vector-copy (read (open-input-file "sector2-level1.dat")) vertex-data world))
    ((3)
     (vector-copy (read (open-input-file "sector3-level1.dat")) vertex-data world))
    ((4)
     (vector-copy (read (open-input-file "sector4-level1.dat")) vertex-data world))))


(define (vector-copy vector vertex-data world)
  (let recur ((counter 0))
    (cond
     ((eq? counter 9000))
     ((>= counter (f32vector-length vector))
      (GLfloat*-set! vertex-data counter 0.)
      (recur (+ counter 1)))
     (else
      (GLfloat*-set! vertex-data counter (f32vector-ref vector counter))
      (recur (+ counter 1))))))
#;
(define (vector-copy-f32vector vector index vertex-data-vector world)
  (let recur ((counter index))
    (cond
     ((eq? counter (+ index 16)))
     (else
      (f32-vector-set! vertex-data-vector counter (f32vector-ref vector (- counter index)))
      (recur (+ counter 1))))))

(define (create-level vertex-data world)
  (world-tile-vector-set! world (read (open-input-file "/data/projects/editor-de-mapas/level-one.dat")))
  (update-sector-vector (player-sector (world-player world)) vertex-data world)
  (player-position-updater vertex-data world))

(define (game-updater vertex-data world)
  (case (world-game-state world)
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
     (player-ground-collision world)
     (player-death-collision world))))

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
        (SDL_Log (string-append "OpenGL Version: " (unsigned-char*->string (glGetString GL_VERSION))))
        (SDL_Log "Using API OpenGL Version: 2.1 - GL Shading Language Version: 1.2")
        ;; Glew: initialize extensions
        (glewInit)
        ;; OpenGL viewport
        (glViewport 0 0 screen-width screen-height)
        (glScissor 0 0 screen-width screen-height)
        
        ;; PRECHARGE stuff
        ;;(create-level vertex-data world)

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
               ;;(vertex-data-vector (list->f32vector (world-vertex-vector world)))
               ;;(vertex-data-vector (read (open-input-file "sector1-level1.dat")))
               (vertex-data-vector (make-f32vector 10000 0.))

               (vertex-data (f32vector->GLfloat* vertex-data-vector))
               (shaders (list (fusion:create-shader GL_VERTEX_SHADER vertex-shader)
                              (fusion:create-shader GL_FRAGMENT_SHADER fragment-shader)))
               (shader-program (fusion:create-program shaders))
               (texture-image* (SDL_LoadBMP "assets/128x32.bmp")))
          ;; Clean up shaders once the program has been compiled and linked
          (for-each glDeleteShader shaders)

          ;; Texture
          (glGenTextures 1 texture-id*)
          (glBindTexture GL_TEXTURE_2D (*->GLuint texture-id*))
          (glTexImage2D GL_TEXTURE_2D 0 3
                        (SDL_Surface-w texture-image*) (SDL_Surface-h texture-image*)
                        0 GL_BGR GL_UNSIGNED_BYTE
                        (SDL_Surface-pixels texture-image*))
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_BASE_LEVEL 0)
          (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAX_LEVEL 0)
          (glBindTexture GL_TEXTURE_2D 0)
          (SDL_FreeSurface texture-image*)

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
            (glSamplerParameteri sampler-id GL_TEXTURE_MAG_FILTER GL_NEAREST)
            (glSamplerParameteri sampler-id GL_TEXTURE_MIN_FILTER GL_NEAREST))
          
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
            
            ;;(create-level vertex-data world)
            
            ;; Game loop
            (let ((event* (alloc-SDL_Event 1)))
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
                                        (world-tile-vector-set! world (read (open-input-file "level-one.dat")))
                                        (update-sector-vector (player-sector (world-player world)) vertex-data world)
                                        (player-init vertex-data world)
                                        (world-game-state-set! world 'level-one))
                                       ((and (= key SDLK_SPACE) (not (eq? (world-game-state world) 'init-screen)) (eq? (player-is-collision? (world-player world)) #t))
                                        (case (player-gravity (world-player world))
                                          ((up)
                                           (player-is-collision?-set! (world-player world) #f)
                                           (player-gravity-set! (world-player world) 'down))
                                          ((down)
                                           (player-is-collision?-set! (world-player world) #f)
                                           (player-gravity-set! (world-player world) 'up))))
                                       ((and (= key SDLK_RIGHT) (not (eq? (world-game-state world) 'init-screen)))
                                        (player-horizontal-state-set! (world-player world) 'right))
                                       
                                       ((and (= key SDLK_LEFT) (not (eq? (world-game-state world) 'init-screen))) 
                                        (player-horizontal-state-set! (world-player world) 'left))
                                       
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
                   ;;(world-vertex-vector-set! world '())
                   
                   ;;(call-with-output-file "sector1-level1.dat" (lambda (f) (display (list->f32vector(world-vertex-vector world)) f)))
                   ;;(pp vertex-data)
                   (println (string-append "player: " (object->string (world-player world)) " ; playerx: "  (object->string(GLfloat*-ref vertex-data 9008))))
                   (game-updater vertex-data world)
                   
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
              (free event*)
              (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "Bye.")
              (SDL_GL_DeleteContext ctx)
              (SDL_DestroyWindow win)
              (SDL_Quit)))))))
  (##gc))

