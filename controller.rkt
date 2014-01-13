#lang racket/gui

(require slideshow/pict
         racket/runtime-path
         "defs.rkt"
         "model.rkt"
         "sprite.rkt"
         "view.rkt")

(require (except-in rsound clip))

(define-runtime-path piece-slide-loud-path "sounds/piece-slide-big.wav")
(define-runtime-path piece-slide-soft-path "sounds/piece-slide.wav")
(define-runtime-path success-low-path "sounds/success-low.wav")
(define-runtime-path success-high-path "sounds/success-high.wav")
(define-runtime-path success-huge-path "sounds/success-huge.wav")
(define-runtime-path music-path "sounds/ninja-cat-band.wav")

(define piece-slide-loud (rs-read piece-slide-loud-path))
(define piece-slide-soft (rs-scale 0.25 (rs-read piece-slide-soft-path)))
(define success-low (rs-read success-low-path))
(define success-high (rs-read success-high-path))
(define success-huge (rs-read success-huge-path))

(define music (rs-scale 0.66 (rs-read music-path)))
(define music-frames (rs-frames music))

(define (queue-music)
  (define stream (make-pstream #:buffer-time 0.2))
  (pstream-play stream music)
  (pstream-queue-callback stream queue-music (+ music-frames (/ 44100 2))))

(queue-music)


(define controller-fps 20)

(define trivial-tiles '(= 0))
(define level0-tiles '(= = = 0 1 2 3 4 5 6 7 8 9))
(define level1-tiles '(+ + + + = = = 0 1 2 3 4 5 6 7 8 9))
(define level2-tiles '(+ - + - = = = 0 1 2 3 4 5 6 7 8 9))
(define level3-tiles '(+ - * + = = = 0 1 2 3 4 5 6 7 8 9))
(define level4-tiles '(+ * - / = = = 0 1 2 3 4 5 6 7 8 9))
(define level5-tiles '(+ * - / = = = 0 1 2 3 4 5 6 7 8 9 a let))

(define logic-tiles '(#t #f imp conv and or iff))

(define level-tiles level4-tiles)

(define controller-frame%
  (class frame%
    (super-new [label "Nonplussed"]
               [style '(no-resize-border)]
               [stretchable-width #f]
               [stretchable-height #f])
    
    (inherit get-width get-height)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Mouse state
    
    (define last-x 0)
    (define last-y 0)
    (define down-tx 0)
    (define down-ty 0)
    (define last-tx 0)
    (define last-ty 0)
    (define last-down? #f)
    
    (define/public (get-mouse)
      (values last-x last-y last-tx last-ty last-down?))
    
    (define (dragging? tx ty)
      (and last-down? (not (and (= last-tx tx) (= last-ty ty)))))
    
    (define (dragged? tx ty)
      (and last-down? (not (and (= down-tx tx) (= down-ty ty)))))
    
    (define (update-mouse! tx ty down?)
      (set! last-tx tx)
      (set! last-ty ty)
      (set! last-down? down?))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Model
    
    (define frozen-count 0)
    
    (define tiles-left 100)
    (define model (make-model 9 9 18))
    (model-fill-empty! model level-tiles)
    
    (define/public (get-model) model)
    
    (define score-hash
      (make-hasheq '((+ . 2)
                     (- . 2)
                     (* . 3)
                     (/ . 3))))
    
    (define (check-elim!)
      (define-values (stmts coords) (model-find-truth model))
      (unless (empty? coords)
        (set! frozen-count (+ frozen-count 1))
        (define hash
          (for/fold ([hash (make-immutable-hasheq)]) ([xy  (in-list coords)])
            (match-define (list tx ty) xy)
            (redraw-tile tx ty 'hilite)
            (define v (model-tile-ref model tx ty))
            (define s (hash-ref score-hash v 1))
            (hash-set hash s (+ 1 (hash-ref hash s 0)))))
        (define points (hash->points hash))
        (define (sprite-death)
          (set-model-score! model (+ (model-score model) points))
          (set-model-stmts! model (append stmts (model-stmts model)))
          (for ([xy  (in-list coords)])
            (match-define (list tx ty) xy)
            (model-tile-set! model tx ty empty)
            (redraw-tile tx ty 'normal))
          (set! tiles-left (max 0 (- tiles-left n)))
          (redraw-score)
          (redraw-elims)
          (set! frozen-count (- frozen-count 1)))
        (define n (length coords))
        (define avg-tx (/ (apply + (map first coords)) n))
        (define avg-ty (/ (apply + (map second coords)) n))
        (define-values (x y) (tile-coords->view-coords avg-tx avg-ty))
        (cond [(points . >= . 256)  (play success-huge)]
              [(points . >= . 64)  (play success-high)]
              [else  (play success-low)])
        (send view-canvas add-sprite
              (trajectory-sprite
               (add-score-pict hash)
               2500 500
               (+ x (* 1/2 tile-size)) (+ y (* 1/2 tile-size)) 1 1
               260 140 0 2/5
               sprite-death))))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; View
    
    (define view-panel (new panel% [parent this]))
    (define view-canvas (new view-canvas% [parent view-panel] [controller this]))
    
    (define (redraw-tile tx ty state)
      (send view-canvas update-tile tx ty (model-tile-ref model tx ty) state)
      (send view-canvas refresh-later))
    
    (define (redraw-score)
      (send view-canvas update-score (model-score model) (model-moves model) tiles-left)
      (send view-canvas refresh-later))
    
    (define (redraw-elims)
      (send view-canvas update-elims (model-stmts model))
      (send view-canvas refresh-later))
    
    (redraw-score)
    (redraw-elims)
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Tile drop
    
    (define last-num-dropped 0)
    
    (define (timer-tick)
      (define coords (model-drop-tiles! model))
      (cond [(empty? coords)
             (unless (zero? last-num-dropped)
               (model-fill-empty! model level-tiles)
               (check-elim!))]
            [else
             (play piece-slide-loud)
             (for ([xy  (in-list coords)])
               (match-define (list tx ty) xy)
               (redraw-tile tx ty 'normal))])
      (set! last-num-dropped (length coords)))
    
    (define timer (make-object timer% timer-tick (ceiling (/ 1000 controller-fps))))
    
    (define/augment (on-close)
      (stop)
      (exit 0))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Event handling
    
    (define/public (on-view-event e)
      (define-values (w h) (model-size model))
      (define vh (model-visible-height model))
      (define type (send e get-event-type))
      (set! last-x (send e get-x))
      (set! last-y (send e get-y))
      (define-values (tx ty) (view-coords->tile-coords last-x last-y))
      (when (and (= frozen-count 0) (= 0 last-num-dropped))
        (case type
          [(left-down)  (update-mouse! tx ty #t)
                        (set! down-tx tx)
                        (set! down-ty ty)
                        (redraw-tile tx ty 'selected)]
          [(left-up)  (define d? (dragged? tx ty))
                      (update-mouse! tx ty #f)
                      (redraw-tile tx ty 'normal)
                      (when d? (check-elim!))]
          [(motion)  (when (dragging? tx ty)
                       (define dist (max (abs (- tx last-tx)) (abs (- ty last-ty))))
                       (when (= dist 1)
                         (model-tile-swap! model tx ty last-tx last-ty)
                         (play piece-slide-soft)
                         (redraw-tile tx ty 'selected)
                         (redraw-tile last-tx last-ty 'normal)
                         (send view-canvas add-sprite
                               (trajectory-sprite
                                (colorize (text "-1" '(bold . system) 18) "gold")
                                500 500
                                last-x last-y 1/4 2
                                260 168 1 1
                                (λ ()
                                  (set-model-moves! model (+ 1 (model-moves model)))
                                  (redraw-score)))))
                       (update-mouse! tx ty last-down?))
                     (when last-down?
                       (send view-canvas refresh-later))])))
    ))

(define controller-frame (make-object controller-frame%))
(send controller-frame show #t)
