#lang racket

(require slideshow/pict
         images/compile-time
         images/flomap
         (for-syntax "make-resources.rkt"
                     racket/class
                     racket/draw
                     racket/promise
                     images/flomap)
         "defs.rkt")

(provide symbol->tile
         indexes->background-tile selected-background-tile hilite-background-tile
         lparen-pict rparen-pict
         logo
         lt-border-pict ct-border-pict rt-border-pict
         lc-border-pict rc-border-pict
         lb-border-pict cb-border-pict rb-border-pict)

(define symbol->tile-hash
  (make-hasheq
   (map cons '(0 1 2 3 4 5 6 7 8 9 + - * / = a b c let
                 #t #f ! != < <= > >= not and or imp conv iff
                 )
        (compiled-bitmap-list
         (list (digit-tile 0)
               (digit-tile 1)
               (digit-tile 2)
               (digit-tile 3)
               (digit-tile 4)
               (digit-tile 5)
               (digit-tile 6)
               (digit-tile 7)
               (digit-tile 8)
               (digit-tile 9)
               (arithmetic-operator-tile '+)
               (arithmetic-operator-tile '−)
               (logical-operator-tile '×)
               (logical-operator-tile '/)
               (comparison-tile '=)
               (name-tile 'a)
               (name-tile 'b)
               (name-tile 'c)
               (word-tile 'let)
               (truth-tile 'T)
               (truth-tile 'F)
               (arithmetic-operator-tile '!)
               (comparison-tile '≠)
               (comparison-tile '<)
               (comparison-tile '≤)
               (comparison-tile '>)
               (comparison-tile '≥)
               (logical-operator-tile '¬)
               (logical-operator-tile '∧)
               (logical-operator-tile '∨)
               (logical-operator-tile '→)
               (logical-operator-tile '←)
               (logical-operator-tile '↔)
               )))))

(define background-tiles
  (list->vector
   (compiled-bitmap-list
    (list (background-tile (make-object color% 132 164 208))
          (background-tile "darkslategray")))))

(define selected-background-tile (compiled-bitmap (background-tile "gold")))
(define hilite-background-tile (compiled-bitmap (background-tile (make-object color% 192 255 128))))

(define (symbol->tile sym)
  (hash-ref symbol->tile-hash sym))

(define (indexes->background-tile x y)
  (vector-ref background-tiles (remainder (+ x y) 2)))

(define lparen-pict
  (bitmap (compiled-bitmap (unstamped-tile (symbol-flomap/tile-height "(" "gold")))))

(define rparen-pict
  (bitmap (compiled-bitmap (unstamped-tile (symbol-flomap/tile-height ")" "gold")))))

;; ===================================================================================================
;; Logo

(define logo (compiled-bitmap (force logo)))

;; ===================================================================================================
;; Border

(define (stretch-border-wide left-fm fm right-fm)
  (define w (flomap-width fm))
  (define s (/ (* 9 tile-size tile-scale) w))
  (define x (* s (flomap-width left-fm)))
  (subflomap (flomap-scale (flomap-ht-append left-fm fm right-fm) s 1)
             (exact-ceiling x)
             0
             (exact-ceiling (+ x (* 9 tile-size tile-scale)))
             (exact-ceiling (* border-thickness tile-scale))))

(define (stretch-border-tall top-fm fm bot-fm)
  (define h (flomap-height fm))
  (define s (/ (* 9 tile-size tile-scale) h))
  (define y (* s (flomap-height top-fm)))
  (subflomap (flomap-scale (flomap-vl-append top-fm fm bot-fm) 1 s)
             0
             (exact-ceiling y)
             (exact-ceiling (* border-thickness tile-scale))
             (exact-ceiling (+ y (* 9 tile-size tile-scale)))))

(match-define (list lt-border-pict ct-border-pict rt-border-pict
                    lc-border-pict rc-border-pict
                    lb-border-pict cb-border-pict rb-border-pict)
  (let ()
    (match-define (list lt-border-fm ct-border-fm rt-border-fm
                        lc-border-fm rc-border-fm
                        lb-border-fm cb-border-fm rb-border-fm)
      (map bitmap->flomap
           (compiled-bitmap-list
            (map flomap->bitmap (border-fms (make-object color% 4 32 8))))))
    
    (list
     ;; lt, ct, rt
     (bitmap (flomap->bitmap lt-border-fm #:backing-scale tile-scale))
     (bitmap (flomap->bitmap (stretch-border-wide lt-border-fm ct-border-fm rt-border-fm)
                             #:backing-scale tile-scale))
     (bitmap (flomap->bitmap rt-border-fm #:backing-scale tile-scale))
     ;; lc, rc
     (bitmap (flomap->bitmap (stretch-border-tall lt-border-fm lc-border-fm lb-border-fm)
                             #:backing-scale tile-scale))
     (bitmap (flomap->bitmap (stretch-border-tall rt-border-fm rc-border-fm rb-border-fm)
                             #:backing-scale tile-scale))
     ;; lb, cb, rb
     (bitmap (flomap->bitmap lb-border-fm #:backing-scale tile-scale))
     (bitmap (flomap->bitmap (stretch-border-wide lb-border-fm cb-border-fm rb-border-fm)
                             #:backing-scale tile-scale))
     (bitmap (flomap->bitmap rb-border-fm #:backing-scale tile-scale)))))

;; ===================================================================================================
