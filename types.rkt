#lang typed/racket/base

(provide (all-defined-out))

;; Values that can be on a tile
(define-type Infix-Symbol (U '+ '- '* '/ '= '!= '< '<= '> '>= 'and 'or 'imp 'conv 'iff))
(define-type Prefix-Symbol (U 'not '-))
(define-type Postfix-Symbol (U '!))
(define-type Binary-Symbol (U Infix-Symbol))
(define-type Unary-Symbol (U Prefix-Symbol Postfix-Symbol))
(define-type Tile-Symbol (U Binary-Symbol Unary-Symbol))
(define-type Digit (U 0 1 2 3 4 5 6 7 8 9))
(define-type Name (U 'a 'b 'c))
(define-type Keyword (U 'let))
(define-type Tile-Value (U Null Tile-Symbol Digit Name Keyword Boolean))

(define-type Unparsed-Value (U Tile-Value 'lparen 'rparen 'space))

;; Interpreter return values
(define-type Bottom '⊥)  ; Rampant failure is expected; return this instead of raising errors
(define-type Expr-Value (U Exact-Rational Boolean Bottom))

(define-predicate infix-symbol? Infix-Symbol)
(define-predicate prefix-symbol? Prefix-Symbol)
(define-predicate postfix-symbol? Postfix-Symbol)
(define-predicate tile-symbol? Tile-Symbol)
(define-predicate tile-value? Tile-Value)
(define-predicate digit? Digit)
(define-predicate name? Name)
(define-predicate keyword? Keyword)

(define natural? exact-nonnegative-integer?)
(define-predicate exact-rational? Exact-Rational)

(struct: (A) span ([start : Integer] [end : Integer] [value : A]) #:transparent)
(struct: (A B) result ([span : (span A)] [value : B]) #:transparent)
