#lang racket

(require racket/draw racket/class racket/runtime-path
         slideshow/pict
         images/flomap
         images/private/deep-flomap
         unstable/parameter-group
         "defs.rkt")

(provide (all-defined-out))

(define tile-lighting
  (deep-flomap-lighting-value
   '(-0.25 -0.5 1.0)
   '(1.0 1.0 1.0)
   '(1.0 1.0 1.0)
   '(1.0 1.0 1.0)))

(define tile-material
  (deep-flomap-material-value
   'diamond 1.0 0.75 0.2
   0.25 0.1 1.0
   0.2 0.4 0.25
   0.01))

(define background-material
  (deep-flomap-material-value
   'diamond 1.0 0.75 0.2
   0.25 0.1 1.0
   0.2 0.4 0.25
   0.06))

(define symbol-material
  (deep-flomap-material-value
   3.0 1.0 0.75 0.2
   0.75 0.1 0.2
   0.2 0.4 0.25
   0.1))

(define logo-material tile-material)

(define (outlined-fm fm)
  (flomap-cc-superimpose (flomap-outline (flomap-inset fm (exact-ceiling (* 2 tile-scale)))
                                         (exact-ceiling (* 1/2 tile-scale)))
                         fm))

(define (symbol-flomap op color)
  (define pict (text (format "~a" op) '(bold . system) (exact-ceiling (* 40 tile-scale))))
  (define fm (flomap-trim (bitmap->flomap (pict->bitmap (colorize pict color)))))
  (outlined-fm fm))

(define (symbol-flomap/tile-height op color)
  (define pict (text (format "~a" op) '(bold . system)
                     (exact-ceiling (* 2 tile-size tile-scale))))
  (define fm
    (flomap-scale
     (flomap-resize (flomap-trim (bitmap->flomap (pict->bitmap (colorize pict color))))
                    #f
                    (exact-ceiling (* (- tile-size 4) tile-scale)))
     1.25 1))
  (outlined-fm fm))

(define (flomap-blacken fm)
  (define-values (w h) (flomap-size fm))
  (flomap-append-components (flomap-ref-component fm 0) (make-flomap 3 w h)))

(define blank-tile (make-flomap 4
                                (exact-ceiling (* tile-size tile-scale))
                                (exact-ceiling (* tile-size tile-scale))))

(define (shadowed-tile tile-fm)
  (flomap-cc-superimpose
   (flomap-cc-superimpose
    blank-tile
    (flomap-inset (flomap-shadow (flomap-inset tile-fm (exact-ceiling (* 4 tile-scale)))
                                 (* 2 tile-scale))
                  (exact-ceiling (* 2 tile-scale))
                  (exact-ceiling (* 4 tile-scale))
                  0 0))
   (flomap-cc-superimpose blank-tile tile-fm)))

(define (stamped-tile tile-fm symbol-fm)
  (let* ([indent-fm  (flomap-blacken symbol-fm)]
         [indent-fm  (flomap-blur indent-fm (* 1/2 tile-scale))]
         [indent-dfm  (flomap->deep-flomap indent-fm)]
         [indent-dfm  (deep-flomap-emboss indent-dfm (* 6 tile-scale) (* -6 tile-scale))]
         [dfm  (flomap->deep-flomap tile-fm)]
         [dfm  (deep-flomap-emboss dfm (* 6 tile-scale) (* 6 tile-scale))]
         [dfm  (deep-flomap-bulge-round dfm (* -6 tile-scale))]
         [dfm  (deep-flomap-raise dfm (* symbol-size tile-scale))]
         [dfm  (deep-flomap-cc-superimpose 'add dfm indent-dfm)]
         [symbol-dfm  (flomap->deep-flomap symbol-fm)]
         [symbol-dfm  (deep-flomap-emboss symbol-dfm (* 6 tile-scale) (* 6 tile-scale))])
    (parameterize/group ([deep-flomap-lighting  tile-lighting])
      (flomap->bitmap
       (shadowed-tile
        (flomap-cc-superimpose
         (parameterize/group ([deep-flomap-material  tile-material])
           (deep-flomap-render dfm))
         (parameterize/group ([deep-flomap-material  symbol-material])
           (deep-flomap-render symbol-dfm))))
       #:backing-scale tile-scale))))

(define (unstamped-tile symbol-fm)
  (let* ([symbol-dfm  (flomap->deep-flomap symbol-fm)]
         [symbol-dfm  (deep-flomap-emboss symbol-dfm (* 6 tile-scale) (* 6 tile-scale))]
         [symbol-dfm  (deep-flomap-bulge-round symbol-dfm (* -6 tile-scale))]
         [symbol-dfm  (deep-flomap-raise symbol-dfm (* 32 tile-scale))]
         )
    (parameterize/group ([deep-flomap-lighting  tile-lighting]
                         [deep-flomap-material  symbol-material])
      (flomap->bitmap
       (deep-flomap-render symbol-dfm)
       #:backing-scale tile-scale))))

(define (arithmetic-operator-tile op)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush (make-object color% 255 32 24) 'solid)
       (send dc draw-ellipse 0.5 0.5 47 47))
     (exact-ceiling (* symbol-size tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (stamped-tile tile-fm (symbol-flomap op "Lavender")))

(define (digit-tile n)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush "Lavender" 'solid)
       (send dc draw-polygon '((0.5 . 9.5)
                               (9.5 . 0.5)
                               (38.5 . 0.5)
                               (47.5 . 9.5)
                               (47.5 . 38.5)
                               (38.5 . 47.5)
                               (9.5 . 47.5)
                               (0.5 . 38.5))))
     (exact-ceiling (* symbol-size tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (stamped-tile tile-fm (symbol-flomap n (make-object color% 160 0 0))))

(define (comparison-tile op)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush "greenyellow" 'solid)
       (send dc draw-ellipse 0.5 0.5 47 47))
     (exact-ceiling (* symbol-size tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (stamped-tile tile-fm (symbol-flomap op "white")))

(define (logical-operator-tile op)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush "deepskyblue" 'solid)
       (send dc draw-rounded-rectangle 0.5 0.5 47 47 -0.4))
     (exact-ceiling (* symbol-size tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (stamped-tile tile-fm (symbol-flomap op "LemonChiffon")))

(define (truth-tile sym)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush "LemonChiffon" 'solid)
       (send dc draw-rounded-rectangle 0.5 0.5 47 47 -0.125))
     (exact-ceiling (* symbol-size tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (stamped-tile tile-fm (symbol-flomap sym "navy")))

(define (name-tile sym)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush "white" 'solid)
       (send dc draw-rounded-rectangle 0.5 0.5 47 47))
     (exact-ceiling (* symbol-size tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (define symbol-fm
    (let ([fm  (outlined-fm
                (bitmap->flomap
                 (pict->bitmap
                  (colorize (text (format "~a" sym)
                                  '(bold . system)
                                  (exact-ceiling (* 40 tile-scale)))
                            "chocolate"))))])
      (define-values (x y w h) (flomap-nonzero-rect fm))
      (subflomap fm x 0 w (exact-ceiling (* symbol-size tile-scale)))))
  (stamped-tile tile-fm symbol-fm))

(define (word-tile str)
  (define tile-fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-brush "white" 'solid)
       (send dc draw-rectangle 0.5 0.5 55 47))
     (exact-ceiling (* (* 1/2 (+ tile-size symbol-size)) tile-scale))
     (exact-ceiling (* symbol-size tile-scale))))
  (define symbol-fm
    (let* ([fm  (bitmap->flomap
                 (pict->bitmap
                  (colorize (text (format "~a" str)
                                  '(bold . system)
                                  (exact-ceiling (* 40 tile-scale)))
                            "black")))]
           [fm  (flomap-resize fm
                               (min (exact-ceiling (* 50 tile-scale))
                                    (flomap-width fm))
                               (flomap-height fm))]
           [fm  (outlined-fm fm)])
      (define-values (x y w h) (flomap-nonzero-rect fm))
      (subflomap fm x 0 w (exact-ceiling (* symbol-size tile-scale)))))
  (stamped-tile tile-fm symbol-fm))

(define (background-tile color)
  (let* ([fm  (draw-flomap
               (λ (dc)
                 (send dc scale tile-scale tile-scale)
                 (send dc set-pen "black" 1/2 'transparent)
                 (send dc set-brush color 'solid)
                 (send dc draw-rounded-rectangle 0.5 0.5 63 63 3))
               (exact-ceiling (* tile-size tile-scale))
               (exact-ceiling (* tile-size tile-scale)))]
         [dfm  (flomap->deep-flomap fm)]
         [dfm  (deep-flomap-emboss dfm (* 5 tile-scale) (* 5 tile-scale))]
         [dfm  (deep-flomap-bulge-horizontal dfm (* -2 tile-scale))]
         [dfm  (deep-flomap-bulge-vertical dfm (* -2 tile-scale))]
         [dfm  (deep-flomap-raise dfm (* symbol-size tile-scale))])
    (parameterize/group ([deep-flomap-lighting  tile-lighting]
                         [deep-flomap-material  background-material])
      (flomap->bitmap
       (deep-flomap-render dfm)
       #:backing-scale tile-scale))))

(define-runtime-path logo-path "images/logo.png")

(define logo-fm
  (lazy
   (let* ([fm  (bitmap->flomap (read-bitmap logo-path))]
          [fm  (flomap-cc-superimpose (flomap-outline fm (* 1/2 tile-scale)) fm)]
          [dfm  (flomap->deep-flomap fm)]
          [dfm  (deep-flomap-emboss dfm (* 3 tile-scale) (* 3 tile-scale))]
          [dfm  (deep-flomap-bulge-horizontal dfm (* 7 tile-scale))]
          [dfm  (deep-flomap-bulge-vertical dfm (* 7 tile-scale))]
          [dfm  (deep-flomap-raise dfm (* symbol-size tile-scale))])
     (parameterize/group ([deep-flomap-lighting  tile-lighting]
                          [deep-flomap-material  background-material])
       (deep-flomap-render dfm)))))

(define logo (lazy (flomap->bitmap (force logo-fm) #:backing-scale tile-scale)))

(define (border-fms color)
  (define b border-thickness)
  (define n 8)
  (define fm
    (draw-flomap
     (λ (dc)
       (send dc scale tile-scale tile-scale)
       (send dc set-pen "black" 1 'transparent)
       (send dc set-brush color 'solid)
       (send dc draw-rectangle 0 0 (* b n) b)
       (send dc draw-rectangle 0 0 b (* b n))
       (send dc draw-rectangle 0 (* b (- n 1)) (* b n) b)
       (send dc draw-rectangle (* b (- n 1)) 0 b (* b n)))
     (exact-ceiling (* b n tile-scale))
     (exact-ceiling (* b n tile-scale))))
  (define border-fm
    (let* ([dfm  (flomap->deep-flomap fm)]
           [dfm  (deep-flomap-emboss dfm (* 1 tile-scale) (* 1 tile-scale))]
           [dfm  (deep-flomap-bulge-horizontal dfm (* 7 tile-scale))]
           [dfm  (deep-flomap-bulge-vertical dfm (* 7 tile-scale))]
           [dfm  (deep-flomap-raise dfm (* 16 tile-scale))])
      (parameterize/group ([deep-flomap-lighting  tile-lighting]
                           [deep-flomap-material  tile-material])
        (deep-flomap-render dfm))))
  (define-values (w h) (flomap-size border-fm))
  (let ([b  (exact-ceiling (* b tile-scale))])
    (list (subflomap border-fm 0 0 b b)
          (subflomap border-fm b 0 (- w b) b)
          (subflomap border-fm (- w b) 0 w b)
          (subflomap border-fm 0 b b (- h b))
          (subflomap border-fm (- w b) b w (- h b))
          (subflomap border-fm 0 (- h b) b h)
          (subflomap border-fm b (- h b) (- w b) h)
          (subflomap border-fm (- w b) (- h b) w h))))
