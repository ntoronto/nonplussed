#lang racket

(require racket/draw racket/class
         slideshow/pict
         images/flomap)

(provide (all-defined-out))

(struct sprite (timeout draw death) #:transparent)

(define (fadeout-pict-sprite p x y ms)
  (define timeout (+ (current-milliseconds) ms))
  (sprite timeout
          (λ (dc)
            (define old-alpha (send dc get-alpha))
            (define a (/ (min 500 (- timeout (current-milliseconds))) 500))
            (define s (+ 1 (* 0.5 (sqrt (- 1 (/ (min 500 (- timeout (current-milliseconds))) 500))))))
            (send dc set-alpha a)
            (let ([p  (scale p s)])
              (define px (- x (* 0.5 (pict-width p))))
              (define py (- y (* 0.5 (pict-height p))))
              (draw-pict p dc px py))
            (send dc set-alpha old-alpha))
          (λ () (void))
          ))

(define (trajectory-sprite p0 ms move-ms x0 y0 a0 s0 x1 y1 a1 s1 death)
  (define timeout (+ (current-milliseconds) ms))
  (define p (scale (bitmap (pict->bitmap (scale p0 (max s0 s1)))) (/ (max s0 s1))))
  (sprite timeout
          (λ (dc)
            (define old-alpha (send dc get-alpha))
            (define ms (- timeout (current-milliseconds)))
            (define t (min 1 (max 0 (sqr (- 1 (/ (min move-ms ms) move-ms))))))
            (define a (+ (* a1 t) (* a0 (- 1 t))))
            (define s (+ (* s1 t) (* s0 (- 1 t))))
            (define x (+ (* x1 t) (* x0 (- 1 t))))
            (define y (+ (* y1 t) (* y0 (- 1 t))))
            (send dc set-alpha a)
            
            (let ([p  (scale p s)])
              (define px (- x (* 0.5 (pict-width p))))
              (define py (- y (* 0.5 (pict-height p))))
              ;(send dc set-smoothing 'aligned)
              (draw-pict p dc px py))
            
            (send dc set-alpha old-alpha))
          death))

(define (square-pict p)
  (ht-append p (text "2" '(bold . system) 15)))

(define (paren-pict p)
  (hbl-append (text "(" 'system 35) p (text ")" 'system 40)))

(define (label-num n str)
  (vc-append (text (number->string n) '(bold . system) 20)
             (text str '(bold . system) 10)))

(define (hash->points hash)
  (sqr (for/fold ([pts 0]) ([(s n)  (in-hash hash)])
         (+ pts (* s n)))))

(define (score->color s)
  (case s
    [(1)  "lavender"]
    [(2)  (make-object color% 255 32 24)]
    [(3)  "deepskyblue"]))

(define (add-score-pict hash)
  (define keys (sort (hash-keys hash) <))
  (define s (first keys))
  (define n (hash-ref hash s))
  (define p
    (colorize
     (hc-append
      (square-pict
       (paren-pict
        (apply
         hc-append
         (colorize
          (cond [(= s 1)  (label-num n "tiles")]
                [else  (hc-append
                        (label-num n "tiles")
                        (text "×" '(bold . system) 30)
                        (label-num s "points"))])
          (score->color s))
         (for/list ([s  (in-list (rest keys))])
           (define n (hash-ref hash s))
           (colorize
            (hc-append (colorize (text " + " '(bold . system) 30) "white")
                       (label-num n "tiles")
                       (text "×" '(bold . system) 30)
                       (label-num s "points"))
            (score->color s))))))
       (text " = " '(bold . system) 35)
       (colorize (text (format "+~a" (hash->points hash)) '(bold . system) 40) "lawngreen"))
     "white"))
  (cc-superimpose (cellophane (filled-rounded-rectangle (+ 20 (pict-width p))
                                                        (+ 4 (pict-height p)))
                              0.66)
                  p))
