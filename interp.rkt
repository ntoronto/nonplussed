#lang typed/racket

(require math/number-theory
         "types.rkt"
         "defs.rkt")

(provide search-for-truth)

(: map* (All (A B) ((A -> (Listof B)) (Listof A) -> (Listof B))))
;; List monad's `bind' with arguments reversed, composed with remove-duplicates
(define (map* f x)
  (remove-duplicates (append* (map f x))))

;; Basically, to simulate nondeterminism, we're using (finite) lists to model the (finite) set
;; monad's computations

;; ===================================================================================================
;; Types

;; Type of lexer output
(define-type Lexeme (Listof Tile-Value))

;; Type of tokenizer output
(define-type Token (U Symbol Boolean Natural Bottom))

;; Type of expressions
(define-type Expr (U Natural Boolean Name expr))
(struct: expr ([type : Expr-Type]) #:transparent)
(struct: unary-expr expr ([symbol : Unary-Symbol] [arg : Expr]) #:transparent)
(struct: binary-expr expr ([symbol : Binary-Symbol] [lhs : Expr] [rhs : Expr]) #:transparent)
(struct: let-expr expr ([name : Name] [body : Expr]) #:transparent)

;; Simple type system allows failing somewhat earlier
(define-type Expr-Type (U 'N 'B unary-type binary-type '⊥))
(struct: unary-type ([arg : Expr-Type] [ret : Expr-Type]) #:transparent)
(struct: binary-type ([lhs : Expr-Type] [rhs : Expr-Type] [ret : Expr-Type]) #:transparent)

;; ===================================================================================================
;; Helpers

(define (true? v) (eq? v #t))

(: num-fact (Exact-Rational -> (U Bottom Exact-Rational)))
(define (num-fact x) (if (natural? x) (factorial x) '⊥))

(: num/ (Exact-Rational Exact-Rational -> (U Bottom Exact-Rational)))
(define (num/ x y) (if (= y 0) '⊥ (/ x y)))

(define-syntax-rule (implies x y) (or (not x) y))
(define-syntax-rule (converse x y) (implies y x))

(: != (Number Number -> Boolean))
(define (!= x y) (not (= x y)))

(define-syntax-rule (make-number-unary f)
  (λ: ([x : Expr-Value])
    (if (exact-rational? x) (f x) '⊥)))

(define-syntax-rule (make-number-binary f)
  (λ: ([x : Expr-Value] [y : Expr-Value])
    (if (and (exact-rational? x) (exact-rational? y)) (f x y) '⊥)))

(define-syntax-rule (make-boolean-unary f)
  (λ: ([x : Expr-Value])
    (if (boolean? x) (f x) '⊥)))

(define-syntax-rule (make-boolean-binary f)
  (λ: ([x : Expr-Value] [y : Expr-Value])
    (if (and (boolean? x) (boolean? y)) (f x y) '⊥)))

;; ===================================================================================================
;; First-order environment

(: unary-env (HashTable Unary-Symbol (Pair (Expr-Value -> Expr-Value) unary-type)))
(define unary-env
  (make-hasheq
   (list (cons 'not  (cons (make-boolean-unary not)      (unary-type 'B 'B)))
         (cons '!    (cons (make-number-unary num-fact)  (unary-type 'N 'N)))
         (cons '-    (cons (make-number-unary -)         (unary-type 'N 'N))))))

(define get-unary-function (λ: ([sym : Unary-Symbol]) (car (hash-ref unary-env sym))))
(define get-unary-type     (λ: ([sym : Unary-Symbol]) (cdr (hash-ref unary-env sym))))

(: binary-env (HashTable Binary-Symbol (Pair (Expr-Value Expr-Value -> Expr-Value) binary-type)))
(define binary-env
  (make-hasheq
   (list (cons '+     (cons (make-number-binary +)          (binary-type 'N 'N 'N)))
         (cons '-     (cons (make-number-binary -)          (binary-type 'N 'N 'N)))
         (cons '*     (cons (make-number-binary *)          (binary-type 'N 'N 'N)))
         (cons '/     (cons (make-number-binary num/)       (binary-type 'N 'N 'N)))
         (cons '=     (cons (make-number-binary =)          (binary-type 'N 'N 'B)))
         (cons '!=    (cons (make-number-binary !=)         (binary-type 'N 'N 'B)))
         (cons '<     (cons (make-number-binary <)          (binary-type 'N 'N 'B)))
         (cons '<=    (cons (make-number-binary <=)         (binary-type 'N 'N 'B)))
         (cons '>     (cons (make-number-binary >)          (binary-type 'N 'N 'B)))
         (cons '>=    (cons (make-number-binary >=)         (binary-type 'N 'N 'B)))
         (cons 'and   (cons (make-boolean-binary and)       (binary-type 'B 'B 'B)))
         (cons 'or    (cons (make-boolean-binary or)        (binary-type 'B 'B 'B)))
         (cons 'imp   (cons (make-boolean-binary implies)   (binary-type 'B 'B 'B)))
         (cons 'conv  (cons (make-boolean-binary converse)  (binary-type 'B 'B 'B)))
         (cons 'iff   (cons (make-boolean-binary equal?)    (binary-type 'B 'B 'B))))))

(define get-binary-function (λ: ([sym : Binary-Symbol]) (car (hash-ref binary-env sym))))
(define get-binary-type     (λ: ([sym : Binary-Symbol]) (cdr (hash-ref binary-env sym))))

;; ===================================================================================================
;; Lexer

(: prefix-of? (Tile-Value Lexeme -> Boolean))
(define (prefix-of? v vs)
  (cond [(empty? vs)  #f]
        [(and (digit? v) (digit? (first vs)))  #t]
        [else  #f]))

(: cons-if-prefix (Tile-Value (Listof Lexeme) -> (Listof Lexeme)))
(define (cons-if-prefix x xss)
  (if (prefix-of? x (first xss))
      (cons (cons x (first xss)) (rest xss))
      (cons (list x) xss)))

(: legal-groupings ((span (Listof Tile-Value)) -> (Listof (span (Listof Lexeme)))))
(define (legal-groupings sp)
  (match-define (span start end lst) sp)
  (define ress
    (let: loop : (Listof (Listof Lexeme)) ([lst : (Listof Tile-Value) lst])
      (cond [(empty? lst)  empty]
            [(empty? (rest lst))  (list (list lst))]
            [else  (map (λ: ([rst : (Listof Lexeme)])
                          (cons-if-prefix (first lst) rst))
                        (loop (rest lst)))])))
  (map (λ: ([res : (Listof Lexeme)]) (span start end res))
       ress))

(: sublists (All (A) ((Listof A) -> (Listof (span (Listof A))))))
(define (sublists lst)
  (define vec (list->vector lst))
  ;; todo: change to for*/list: when for*/list: is fixed
  (let: len-loop : (Listof (span (Listof A))) ([len : Integer (vector-length vec)])
    (cond [(len . <= . 2)  empty]
          [else  (let: start-loop : (Listof (span (Listof A))) ([start : Integer 0])
                   (cond [(start . >= . (+ 1 (- (vector-length vec) len)))
                          (len-loop (- len 1))]
                         [else  (define end (+ start len))
                                (cons (span start end (vector->list (vector-copy vec start end)))
                                      (start-loop (+ start 1)))]))])))

(: lex* ((Listof Tile-Value) -> (Listof (span (Listof Lexeme)))))
(define (lex* lst)
  (map* legal-groupings (sublists lst)))

;; ===================================================================================================
;; Tokenizer

(: digits->natural ((Listof Digit) -> Natural))
(define (digits->natural ds)
  (let loop ([ds ds] [#{acc : Natural} 0])
    (cond [(empty? ds)  acc]
          [else  (loop (rest ds) (+ (first ds) (* 10 acc)))])))

(: char->digit (Char -> Digit))
(define (char->digit c)
  (define d (- (char->integer c) (char->integer #\0)))
  (if (digit? d) d (raise-type-error 'char->digit "digit character" c)))

(: natural->digits (Natural -> (Listof Digit)))
(define (natural->digits n)
  (map char->digit (string->list (number->string n))))

(: lexeme->token (Lexeme -> Token))
(define (lexeme->token lst)
  (match lst
    [(list (? tile-symbol? x))  x]
    [(list (? boolean? b))  b]
    [(list (? digit? #{ds : (Listof Digit)}) ...)  (digits->natural ds)]
    [(list (? name? x))  x]
    [(list (? keyword? w))  w]
    [_  '⊥]))

(: tokenize ((span (Listof Lexeme)) -> (span (Listof Token))))
(define (tokenize sp)
  (match-define (span start end lst) sp)
  (span start end (map lexeme->token lst)))

(: tokenize* ((Listof Tile-Value) -> (Listof (span (Listof Token)))))
(define (tokenize* lsts)
  (map tokenize (lex* lsts)))

;; ===================================================================================================
;; Parser

(: get-expr-type (Expr -> Expr-Type))
(define (get-expr-type e)
  (cond [(natural? e)  'N]
        [(boolean? e)  'B]
        [(symbol? e)   'N]
        [else  (expr-type e)]))

(: maybe-unary-expr (Unary-Symbol Expr -> (Listof unary-expr)))
(define (maybe-unary-expr sym arg)
  (match-define (unary-type arg-type ret-type) (get-unary-type sym))
  (if (equal? arg-type (get-expr-type arg))
      (list (unary-expr ret-type sym arg))
      empty))

(: maybe-binary-expr (Binary-Symbol Expr Expr -> (Listof binary-expr)))
(define (maybe-binary-expr sym lhs rhs)
  (match-define (binary-type lhs-type rhs-type ret-type) (get-binary-type sym))
  (if (and (equal? lhs-type (get-expr-type lhs))
           (equal? rhs-type (get-expr-type rhs)))
      (list (binary-expr ret-type sym lhs rhs))
      empty))

(: parse-tokens ((Listof Token) -> (Listof Expr)))
(define (parse-tokens toks)
  (cond
    [(empty? toks)  empty]
    [else
     (append
      (match toks
        [(list (? natural? n))  (list n)]
        [(list (? boolean? b))  (list b)]
        [(list (? name? x))     (list x)]
        [_  empty])
      (cond [(and ((length toks) . >= . 4)
                  (eq? (first toks) 'let)
                  (eq? (third toks) '=))
             (define x (second toks))
             (cond [(name? x)  (map* (λ: ([body : Expr]) (list (let-expr '⊥ x body)))
                                     (parse-tokens (rest (rest (rest toks)))))]
                   [else  empty])]
            [else  empty])
      (cond [((length toks) . >= . 2)
             (define fst (first toks))
             (cond [(prefix-symbol? fst)
                    (map* (λ: ([rhs : Expr]) (maybe-unary-expr fst rhs))
                          (parse-tokens (rest toks)))]
                   [else  empty])]
            [else  empty])
      (cond [((length toks) . >= . 2)
             (define lst (last toks))
             (cond [(postfix-symbol? lst)
                    (map* (λ: ([lhs : Expr]) (maybe-unary-expr lst lhs))
                          (parse-tokens (reverse (rest (reverse toks)))))]
                   [else  empty])]
            [else  empty])
      (append*
       (let: loop : (Listof (Listof expr)) ([lhs : (Listof Token)  empty]
                                            [mid : Token  (first toks)]
                                            [rhs : (Listof Token)  (rest toks)])
         (cond [(empty? rhs)  (list empty)]
               [(infix-symbol? mid)
                (append (map* (λ: ([lhs : Expr])
                                (map* (λ: ([rhs : Expr])
                                        (list (maybe-binary-expr mid lhs rhs)))
                                      (parse-tokens rhs)))
                              (parse-tokens (reverse lhs)))
                        (loop (cons mid lhs) (first rhs) (rest rhs)))]
               [else  (loop (cons mid lhs) (first rhs) (rest rhs))]))))]))

(: parse ((span (Listof Token)) -> (Listof (span Expr))))
(define (parse sp)
  (match-define (span start end toks) sp)
  (map (λ: ([res : Expr]) (span start end res))
       (filter (λ: ([e : Expr]) (eq? 'B (get-expr-type e)))
               (parse-tokens toks))))

(: parse* ((Listof Tile-Value) -> (Listof (span Expr))))
(define (parse* lst)
  (map* parse (tokenize* lst)))

;; ===================================================================================================
;; Interpreter

(: interp (Expr (HashTable Name Exact-Rational) -> Expr-Value))
(define (interp e defs)
  (let interp ([e e])
    (match e
      [(? natural? n)  n]
      [(? boolean? b)  b]
      [(? name? x)  (hash-ref defs x (λ () '⊥))]
      [(unary-expr _ sym arg)  ((get-unary-function sym) (interp arg))]
      [(binary-expr _ sym lhs rhs)  ((get-binary-function sym) (interp lhs) (interp rhs))])))

(: interp* ((Listof Tile-Value) (HashTable Name Exact-Rational)
                                -> (Listof (result Expr Expr-Value))))
(define (interp* lst defs)
  (define sps (map (λ: ([sp : (span Expr)])
                     (match-define (span start end e) sp)
                     (result (span start end e) (interp e defs)))
                   (parse* lst)))
  (filter (λ: ([r : (result Expr Expr-Value)]) (true? (result-value r)))
          sps))

;; ===================================================================================================
;; Unparser

(: unary-prec (HashTable Unary-Symbol Exact-Rational))
(define unary-prec
  (make-hash
   (list (cons '! 250)
         (cons '- 225)
         (cons 'not 125))))

(: binary-prec (HashTable Binary-Symbol Exact-Rational))
(define binary-prec
  (make-hasheq
   (list (cons '* 200)
         (cons '/ 200)
         (cons '+ 150)
         (cons '- 150)
         (cons '= 100)
         (cons '!= 100)
         (cons '< 100)
         (cons '<= 100)
         (cons '> 100)
         (cons '>= 100)
         (cons 'and 90)
         (cons 'or 80)
         (cons 'imp 70)
         (cons 'conv 70)
         (cons 'iff 60))))

(: right-assoc (Setof (Pair Tile-Symbol Tile-Symbol)))
(define right-assoc
  (set (cons '* '*)
       (cons '+ '+)
       (cons 'and 'and)
       (cons 'or 'or)
       (cons '* '/)
       (cons '+ '-)
       (cons 'imp 'imp)))

(: left-assoc (Setof (Pair Tile-Symbol Tile-Symbol)))
(define left-assoc
  (set (cons '* '*)
       (cons '+ '+)
       (cons 'and 'and)
       (cons 'or 'or)
       (cons '* '/)
       (cons '+ '-)))

(: right-associative? (Tile-Symbol Tile-Symbol -> Boolean))
(define (right-associative? op1 op2)
  (set-member? right-assoc (cons op1 op2)))

(: left-associative? (Tile-Symbol Tile-Symbol -> Boolean))
(define (left-associative? op1 op2)
  (set-member? left-assoc (cons op1 op2)))

(: parenthesize-child? (Exact-Rational Tile-Symbol Boolean Expr -> Boolean))
(define (parenthesize-child? prec sym lhs? child)
  (match child
    [(unary-expr _ child-sym _)
     (<= (hash-ref unary-prec child-sym) prec)]
    [(binary-expr _ child-sym _ _)
     (define child-prec (hash-ref binary-prec child-sym))
     (or (< child-prec prec)
         (and (= child-prec prec)
              (not (cond [lhs?  (left-associative? child-sym sym)]
                         [else  (right-associative? sym child-sym)]))))]
    [_  #f]))

(: maybe-parenthesize (Exact-Rational Tile-Symbol Boolean Expr (Listof Unparsed-Value)
                                      -> (Listof Unparsed-Value)))
(define (maybe-parenthesize prec sym lhs? child-e child)
  (if (parenthesize-child? prec sym lhs? child-e)
      (append '(lparen) child '(rparen))
      child))

(: unparse (Expr -> (Listof Unparsed-Value)))
(define (unparse e)
  (match e
    [(? natural? n)  (natural->digits n)]
    [(? boolean? b)  (list b)]
    [(unary-expr _ sym arg)
     (define prec (hash-ref unary-prec sym))
     (cond [(postfix-symbol? sym)
            (append (maybe-parenthesize prec sym #t arg (unparse arg))
                    (list sym))]
           [else
            (append (list sym)
                    (maybe-parenthesize prec sym #f arg (unparse arg)))])]
    [(binary-expr _ sym lhs rhs)
     (define prec (hash-ref binary-prec sym))
     (cond [(infix-symbol? sym)
            (append (maybe-parenthesize prec sym #t lhs (unparse lhs))
                    (list 'space sym 'space)
                    (maybe-parenthesize prec sym #f rhs (unparse rhs)))]
           [else
            (append (list sym 'space)
                    (maybe-parenthesize prec sym #f lhs (unparse lhs))
                    '(space)
                    (maybe-parenthesize prec sym #f rhs (unparse rhs)))])]))

(: unparse-result ((result Expr Expr-Value) -> (result (Listof Unparsed-Value) Expr-Value)))
(define (unparse-result res)
  (match-define (result (span start end e) value) res)
  (result (span start end (unparse e)) value))

;; ===================================================================================================
;; Public API

(: search-for-truth ((Listof Tile-Value) (HashTable Name Exact-Rational)
                                         -> (Listof (result (Listof Unparsed-Value) Expr-Value))))
(define (search-for-truth vs defs)
  (remove-duplicates (map unparse-result (interp* vs defs))))
