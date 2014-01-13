#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))

(define: tile-scale : Exact-Rational 2)
(define: symbol-size : Integer 48)
(define: tile-size : Integer 64)
(define: small-tile-size : Integer 24)
(define: gap : Integer 5)
(define: score-panel-width : Integer 320)
(define: border-thickness : Integer 8)

(: pretty-value-hash (HashTable Tile-Value Symbol))
(define pretty-value-hash
  (make-hasheq
   '((#t . T)
     (#f . F)
     (* . ×)
     (!= . ≠)
     (<= . ≤)
     (>= . ≥)
     (not . ¬)
     (and . ∧)
     (or . ∨)
     (imp . →)
     (conv . ←)
     (iff . ↔))))

(: tile-value->string (Tile-Value -> String))
(define (tile-value->string v)
  (format "~a" (hash-ref pretty-value-hash v (λ () v))))
