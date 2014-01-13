#lang typed/racket

(require math/array
         "types.rkt"
         "interp.rkt")

(provide
 ;; Struct fields
 model model? model-visible-height model-tiles model-stmts model-score model-moves
 set-model-score! set-model-moves! set-model-stmts!
 ;; Intended public interface
 make-model
 model-size
 model-tile-ref
 model-tile-set!
 model-tile-swap!
 model-fill-empty!
 model-find-truth
 model-drop-tiles!)

(struct: model ([visible-height : Index]
                [tiles : (Mutable-Array Tile-Value)]
                [stmts : (Listof (Listof Unparsed-Value))]
                [score : Integer]
                [moves : Integer])
  #:mutable
  #:transparent)

(: make-model (Integer Integer Integer -> model))
(define (make-model vh w h)
  (unless (vh . <= . h)
    (error 'make-model "expected visible height <= height; given ~e > ~e" vh h))
  (with-asserts ([vh index?]
                 [w  index?]
                 [h  index?])
    (define: tiles : (Mutable-Array Tile-Value)
      (array->mutable-array ((inst make-array Tile-Value) (vector h w) empty)))
    (model vh tiles empty 0 0)))

(: model-size (model -> (Values Index Index)))
(define (model-size m)
  (match-define (vector h w) (array-shape (model-tiles m)))
  (values w h))

(: model-tile-ref (model Integer Integer -> Tile-Value))
(define (model-tile-ref m x y)
  (array-ref (model-tiles m) (vector y x)))

(: model-tile-set! (model Integer Integer Tile-Value -> Void))
(define (model-tile-set! m x y v)
  (array-set! (model-tiles m) (vector y x) v))

(: model-tile-swap! (model Integer Integer Integer Integer -> Void))
(define (model-tile-swap! m x1 y1 x2 y2)
  (define arr (model-tiles m))
  (define v1 (model-tile-ref m x1 y1))
  (define v2 (model-tile-ref m x2 y2))
  (model-tile-set! m x2 y2 v1)
  (model-tile-set! m x1 y1 v2))

;; ===================================================================================================
;; Randomized fill

(: model-count-tiles (model -> (HashTable Tile-Value Natural)))
(define (model-count-tiles m)
  (: hash (HashTable Tile-Value Natural))
  (define hash (make-hasheq))
  (define-values (w h) (model-size m))
  (for*: ([y  (in-range h)] [x  (in-range w)])
    (define v (model-tile-ref m x y))
    (hash-set! hash v (+ 1 (hash-ref hash v (λ () 0)))))
  hash)

(: make-random/list ((Listof Tile-Value) -> (-> Tile-Value)))
(define (make-random/list lst)
  (define vec (list->vector lst))
  (define len (vector-length vec))
  (λ () (vector-ref vec (random len))))

(: model-fill-empty! (model (Listof Tile-Value) -> Void))
(define (model-fill-empty! m lst)
  (define-values (w h) (model-size m))
  (define vh (model-visible-height m))
  (define n (* w h))
  (let ([lst  (append* (build-list (+ 1 (quotient n (length lst))) (λ (n) lst)))])
    (for*: ([y  (in-range 0 vh)] [x  (in-range 0 w)])
      (define v (model-tile-ref m x y))
      (unless (empty? v)
        (set! lst (remove v lst))))
    (set! lst (shuffle lst))
    (for*: ([y  (in-range vh h)] [x  (in-range 0 w)])
      (define v (model-tile-ref m x y))
      (when (empty? v)
        (model-tile-set! m x y (first lst))
        (set! lst (rest lst))))))

;; ===================================================================================================
;; Elimination

(struct: 2d-result ([kind : (U 'row 'col)]
                    [coord : Integer]
                    [start : Integer]
                    [end : Integer])
  #:transparent)

(: model-find-row-truth (model -> (Values (Listof (Listof Unparsed-Value)) (Listof 2d-result))))
(define (model-find-row-truth m)
  (define-values (w h) (model-size m))
  (define vh (model-visible-height m))
  (define arr (model-tiles m))
  (for/fold: ([stmts : (Listof (Listof Unparsed-Value))  empty]
              [2d-ress : (Listof 2d-result)  empty]
              ) ([y  (in-range vh)])
    (define row (build-list w (λ: ([x : Integer]) (model-tile-ref m x y))))
    (define ress (search-for-truth row (make-hasheq)))
    (cond [(empty? ress)  (values stmts 2d-ress)]
          [else
           (match-define (result (span start end stmt) val) (first ress))
           (values (cons stmt stmts)
                   (cons (2d-result 'row y start end) 2d-ress))])))

(: model-find-truth (model -> (Values (Listof (Listof Unparsed-Value))
                                      (Listof (List Integer Integer)))))
(define (model-find-truth m)
  (define arr (model-tiles m))
  (define-values (stmts ress) (model-find-row-truth m))
  (define coords
    (for/fold: ([coords : (Listof (List Integer Integer))  empty]) ([res  (in-list ress)])
      (match-define (2d-result kind coord start end) res)
      (define new-coords
        (case kind
          [(row)  (for/list: : (Listof (List Integer Integer)) ([x  (in-range start end)])
                    (list x coord))]
          [(col)  (for/list: : (Listof (List Integer Integer)) ([y  (in-range start end)])
                    (list coord y))]))
      (append new-coords coords)))
  (values stmts coords))

;; ===================================================================================================
;; Tile drop

(: model-drop-tiles! (model -> (Listof (List Integer Integer))))
(define (model-drop-tiles! m)
  (define-values (w h) (model-size m))
  (define vh (model-visible-height m))
  (define tiles (model-tiles m))
  (define: frozen : (Mutable-Array Boolean)
    (array->mutable-array ((inst make-array Boolean) (vector h w) #f)))
  
  (: tile-empty? (Integer Integer -> Boolean))
  (define (tile-empty? x y)
    (and (empty? (array-ref tiles (vector y x)))
         (not (array-ref frozen (vector y x)))))
  
  (: tile-set! (Integer Integer Tile-Value -> Void))
  (define (tile-set! x y v)
    (array-set! tiles (vector y x) v)
    (array-set! frozen (vector y x) #t))
  
  (for*: ([y  (in-range 1 h)] [x  (in-range w)])
    (define v (array-ref tiles (vector y x)))
    (unless (or (empty? v) (array-ref frozen (vector y x)))
      (cond [(tile-empty? x (- y 1))
             ;; Drop straight down
             (tile-set! x (- y 1) v)
             (tile-set! x y empty)]
            [(and (tile-symbol? (array-ref tiles (vector (- y 1) x)))
                  (not (array-ref frozen (vector (- y 1) x))))
             ;; Slip off to the side
             (define slip-left? (and (x . > . 0)
                                     (tile-empty? (- x 1) y)
                                     (tile-empty? (- x 1) (- y 1))))
             (define slip-right? (and (x . < . (- w 1))
                                      (tile-empty? (+ x 1) y)
                                      (tile-empty? (+ x 1) (- y 1))))
             (define dir
               (cond [(and slip-left? slip-right?)  (if ((random) . < . 0.5) -1 1)]
                     [slip-left?  -1]
                     [slip-right?  1]
                     [else  0]))
             (unless (= dir 0)
               ;; Slip to the side
               (tile-set! (+ x dir) y v)
               (tile-set! x y empty))])))
  
  (for*/fold: ([coords : (Listof (List Integer Integer))  empty]) ([y  (in-range vh)]
                                                                   [x  (in-range w)])
    (cond [(array-ref frozen (vector y x))  (cons (list x y) coords)]
          [else  coords])))
