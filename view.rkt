#lang racket

(require racket/gui/base
         slideshow/pict
         ;images/flomap
         "types.rkt"
         "defs.rkt"
         "resources.rkt"
         "model.rkt"
         "sprite.rkt")

(provide view-canvas% tile-coords->view-coords view-coords->tile-coords)

(define view-fps 60)

(define num-elims 9)

(define board-size 9)
(define board-width (* (+ 1 board-size) tile-size))
(define board-height board-width)
(define view-width (+ score-panel-width board-width))
(define view-height board-height)

(define logo-y 0)
(define logo-height 100)
(define score-y (+ logo-y logo-height))
(define score-height 115)
(define elims-y (+ score-y score-height))
(define elims-height (+ 35 (* num-elims 37)))

(define (tile-coords->view-coords tx ty)
  (values (+ (* tx tile-size) (* 1/2 tile-size) score-panel-width)
          (+ (* (- (- board-size 1) ty) tile-size) (* 1/2 tile-size))))

(define (view-coords->tile-coords x y)
  (define tx (quotient (- x (* 1/2 tile-size) score-panel-width) tile-size))
  (define ty (- (- board-size 1) (quotient (- y (* 1/2 tile-size)) tile-size)))
  (values (max 0 (min (- board-size 1) tx))
          (max 0 (min (- board-size 1) ty))))

(define hrule-pict (colorize (linewidth 2 (hline (- score-panel-width (* 2 gap)) gap)) "white"))

(define small-lparen-pict (scale lparen-pict 1/2))
(define small-rparen-pict (scale rparen-pict 1/2))

;; ===================================================================================================
;; Score "panel"

(define score-background-brush
  (new brush% [gradient (make-object linear-gradient% 0 0 score-panel-width 0
                          (list (list 0 (make-object color% 0 16 32))
                                (list 1 (make-object color% 0 0 0))))]))

(define (draw-score-background dc)
  (send dc set-brush score-background-brush)
  (send dc draw-rectangle 0 0 score-panel-width view-height))

;; ---------------------------------------------------------------------------------------------------
;; Logo

(define (draw-logo dc)
  (define logo-pict
    (cc-superimpose
     (blank score-panel-width 0)
     (vl-append (blank 0 gap)
                (bitmap logo)
                (blank 0 gap))))
  (draw-pict logo-pict dc 0 logo-y))

;; ---------------------------------------------------------------------------------------------------
;; Cutesy definition

(define (draw-def dc)
  (define def-pict
    (colorize
     (inset
      (htl-append
       (text "nonplussed:  " '(bold . system) 12)
       (vr-append (text "adj." '(italic . system) 12)
                  (text "adj." '(italic . system) 12)
                  (text "adj." '(italic . system) 12))
       (vl-append (text " Confused, bewildered." 'system 12)
                  (text " Calm, unflustered." 'system 12)
                  (text " You, playing this game." 'system 12)))
      (* 2 gap) gap)
     "lavender"))
  (draw-pict def-pict dc 0 (- view-height (pict-height def-pict))))

;; ---------------------------------------------------------------------------------------------------
;; Score

(define tiles-pict (colorize (text "Tiles Left" '(bold . system) 18) "white"))
(define points-pict (colorize (text "Points" '(bold . system) 18) "lawngreen"))
(define swaps-pict (colorize (text "Swaps" '(bold . system) 18) "gold"))
(define ratio-pict (colorize (text "Score" '(bold . system) 18) "white"))

(define (draw-score dc points moves tiles-left)
  (define t (colorize (text (format "~a" tiles-left) '(bold . system) 18) "white"))
  (define p (colorize (text (format "~a" points) '(bold . system) 18) "lawngreen"))
  (define m (colorize (text (format "- ~a" moves) '(bold . system) 18) "gold"))
  (define s (colorize (text (format "~a" (- points moves)) '(bold . system) 18) "white"))
  #;
  (define s (if (= moves 0)
                (colorize (text "(undefined)" '(bold . system) 18) "gray")
                (colorize (text (format "≈ ~a" (real->decimal-string (/ points moves) 2))
                                '(bold . system) 18)
                          "white")))
  (define div-width (max (pict-width p) (pict-width m) (pict-width s)))
  (define div (colorize (linewidth 2 (hline div-width gap)) "white"))
  (define score-pict
    (inset
     (rc-superimpose
      (lc-superimpose
       (blank score-panel-width 0)
       (inset
        (vl-append tiles-pict (blank 0 (* 2 gap)) points-pict swaps-pict (blank 0 gap) ratio-pict)
        (* 2 gap) 0))
      (inset
       (vr-append t (blank 0 (* 2 gap)) p m div s )
       (* 10 gap) 0))
     0 gap))
  (draw-pict score-pict dc 0 score-y))

;; ---------------------------------------------------------------------------------------------------
;; Eliminations

(define (make-math-pict vs)
  (apply
   hc-append
   (for/list ([v  (in-list vs)])
     (cond [(tile-value? v)  (inset (scale (bitmap (symbol->tile v)) 1/2) -4)]
           [else
            (case v
              [(space)   (blank (* small-tile-size 1/4) small-tile-size)]
              [(lparen)  small-lparen-pict]
              [(rparen)  small-rparen-pict])]))))

(define (draw-elims dc stmts)
  (define elims-pict
    (inset
     (vl-append
      (colorize (text "Latest Discoveries" '(bold . system) 18) "white")
      hrule-pict
      (blank 0 gap)
      (if (empty? stmts)
          (blank)
          (apply
           vl-append
           (for/list ([str  (in-list stmts)]
                      [i  (in-range num-elims)])
             (define alpha (expt (/ (- num-elims i) num-elims) 3/2))
             (cellophane (inset (make-math-pict str) gap) alpha)))))
     (* 2 gap) gap))
  (draw-pict elims-pict dc 0 elims-y))
#|
;; ---------------------------------------------------------------------------------------------------
;; Variables

(define (integer->digits n)
  (map (compose string->number string) (string->list (number->string n))))

(define (integer->tiles n)
  (apply hc-append (map (compose bitmap symbol->small-tile) (integer->digits n))))

(define (make-var-pict name val)
  (hc-append
   (bitmap (symbol->small-tile name))
   (blank (* small-tile-size 1/3) small-tile-size)
   (bitmap (symbol->small-tile '=))
   (blank (* small-tile-size 1/3) small-tile-size)
   (if (integer? val)
       (integer->tiles val)
       (hc-append (integer->tiles (numerator val))
                  (bitmap (symbol->small-tile '/))
                  (integer->tiles (denominator val))))))

(define (draw-vars dc vars)
  (define vars-pict
    (inset
     (vl-append
      (colorize (text "Variables" '(bold . system) 18) "white")
      hrule-pict
      (blank 0 gap)
      (if (= 0 (hash-count vars))
          (blank)
          (apply
           vl-append
           (for/list ([(name val)  (in-hash vars)])
             (inset (make-var-pict name val) gap)))))
     (* 2 gap) gap))
  (draw-pict vars-pict dc 0 vars-y))
|#
;; ===================================================================================================
;; Board "panel"

(define board-background-brush
  (new brush% [gradient (make-object linear-gradient% score-panel-width 0 view-width 0
                          (list (list 0 (make-object color% 0 0 0))
                                (list 1 (make-object color% 0 32 0))))]))

(define (draw-board-background dc)
  (send dc set-brush board-background-brush)
  (send dc draw-rectangle score-panel-width 0 board-width board-height))

(define (draw-board-border dc)
  (send dc translate (+ (* 1/2 tile-size) score-panel-width) (* 1/2 tile-size))
  (define size (* board-size tile-size))
  (draw-pict lt-border-pict dc (- border-thickness) (- border-thickness))
  (draw-pict ct-border-pict dc 0 (- border-thickness))
  (draw-pict rt-border-pict dc size (- border-thickness))
  (draw-pict lc-border-pict dc (- border-thickness) 0)
  (draw-pict rc-border-pict dc size 0)
  (draw-pict lb-border-pict dc (- border-thickness) size)
  (draw-pict cb-border-pict dc 0 size)
  (draw-pict rb-border-pict dc size size)
  (send dc translate (- (* -1/2 tile-size) score-panel-width) (* -1/2 tile-size)))

(define (draw-tile dc tx ty v state)
  (define-values (x y) (tile-coords->view-coords tx ty))
  (case state
    [(normal)  (send dc draw-bitmap (indexes->background-tile tx ty) x y)
               (unless (empty? v)
                 (send dc draw-bitmap (symbol->tile v) x y))]
    [(hilite)  (send dc draw-bitmap hilite-background-tile x y)
               (unless (empty? v)
                 (send dc draw-bitmap (symbol->tile v) x y))]
    [else  (send dc draw-bitmap selected-background-tile x y)]))

(define (draw-board dc model)
  (for* ([ty  (in-range board-size)] [tx  (in-range board-size)])
    (define v (model-tile-ref model tx ty))
    (draw-tile dc tx ty v 'normal)))

(define (draw-sprites dc sprites)
  (define ms (current-milliseconds))
  (for ([s  (in-list (reverse sprites))])
    (when (ms . < . (sprite-timeout s))
      ((sprite-draw s) dc))))

;; ===================================================================================================
;; View

(define view-canvas%
  (class canvas%
    (init-field controller)
    (inherit get-dc refresh-now)
    
    (super-new [min-width view-width]
               [min-height view-height])
    
    (define/override (on-event e)
      (send controller on-view-event e))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Canvas contents
    
    (define bm (make-screen-bitmap view-width view-height))
    (define bm-dc (make-object bitmap-dc% bm))
    
    (draw-score-background bm-dc)
    (draw-logo bm-dc)
    (draw-score bm-dc 0 0 0)
    (draw-elims bm-dc empty)
    (draw-def bm-dc)
    
    (draw-board-background bm-dc)
    (draw-board-border bm-dc)
    (draw-board bm-dc (send controller get-model))
    
    (define/public (update-tile tx ty v state)
      (define-values (x y) (tile-coords->view-coords tx ty))
      (define rgn (make-object region%))
      (send rgn set-rectangle x y tile-size tile-size)
      (send bm-dc set-clipping-region rgn)
      (draw-board-background bm-dc)
      (draw-tile bm-dc tx ty v state))
    
    (define/public (update-score points moves tiles-left)
      (define rgn (make-object region%))
      (send rgn set-rectangle 0 score-y score-panel-width score-height)
      (send bm-dc set-clipping-region rgn)
      (draw-score-background bm-dc)
      (draw-score bm-dc points moves tiles-left))
    
    (define/public (update-elims stmts)
      (define rgn (make-object region%))
      (send rgn set-rectangle 0 elims-y score-panel-width elims-height)
      (send bm-dc set-clipping-region rgn)
      (draw-score-background bm-dc)
      (draw-elims bm-dc stmts))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Sprites
    
    (define sprites empty)
    
    (define/public (add-sprite s)
      (set! sprites (cons s sprites))
      (refresh-later))
    
    ;; Removes all timed-out sprites
    (define (clean-sprites)
      (define ms (current-milliseconds))
      (define new-sprites
        (reverse
         (for/fold ([sprites  empty]) ([s  (in-list sprites)])
           (cond [(ms . < . (sprite-timeout s))   (cons s sprites)]
                 [else  ((sprite-death s))
                        sprites]))))
      (unless (= (length new-sprites) (length sprites))
        (refresh-later))
      (set! sprites new-sprites))
    
    ;; -----------------------------------------------------------------------------------------------
    ;; Painting
    
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc draw-bitmap bm 0 0)
      (define-values (mouse-x mouse-y mouse-tx mouse-ty down?) (send controller get-mouse))
      (when down?
        (define model (send controller get-model))
        (define v (model-tile-ref model mouse-tx mouse-ty))
        (unless (empty? v)
          (define-values (tile-x tile-y) (tile-coords->view-coords mouse-tx mouse-ty))
          (let* ([mouse-x  (max (+ score-panel-width (* 1/2 tile-size))
                                (min (- view-width (* 1/2 tile-size)) mouse-x))]
                 [mouse-y  (max (* 1/2 tile-size)
                                (min (- view-height (* 1/2 tile-size)) mouse-y))]
                 [x  (- (/ (- mouse-x tile-x) (* 1/2 tile-size)) 1)]
                 [y  (- (/ (- mouse-y tile-y) (* 1/2 tile-size)) 1)]
                 [x  (+ (* (expt x 3) (* 1/2 tile-size)) tile-x)]
                 [y  (+ (* (expt y 3) (* 1/2 tile-size)) tile-y)])
            (send dc draw-bitmap (symbol->tile v) x y))))
      (draw-sprites dc sprites))
    
    (define refresh? #f)
    (define/public (refresh-later) (set! refresh? #t))
    
    (define (timer-tick)
      (clean-sprites)
      (when (or (not (empty? sprites)) refresh?)
        (set! refresh? #f)
        (refresh-now)))
    
    (define timer (make-object timer% timer-tick (ceiling (/ 1000 view-fps))))
    ))
