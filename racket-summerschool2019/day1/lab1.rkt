#lang racket/base

(require (for-syntax racket/base)
         racket/contract/base
         racket/list
         syntax/parse/define)

(module+ test
  (require rackunit))

;; Exercise 1. Write a macro that encapsulates the
;;             define-and-provide-contract-out pattern. Consider the
;;             idiomatic code:
;;
;; (define (bigger-string x)
;;   (number->string (add1 x)))
;; (provide
;;  (contract-out
;;   [bigger-string
;;    (-> number? string?)]))
;;
;; How could you write these conveniently with a macro?

(define-simple-macro (define! (name args ...) contract body ...)
  (begin
    (define (name args ...) body ...)
    (provide (contract-out
              [name contract]))))

(define! (bigger-string x) (-> number? string?)
  (number->string (add1 x)))

(define-simple-macro (define*! (name [arg type] ...) out-type body ...)
  (begin
    (define (name arg ...) body ...)
    (provide (contract-out
              [name (-> type ... out-type)]))))

(define*! (bigger-string* [x number?] -> string?)
  (number->string (add1 x)))

(module+ test
  (check-equal? (bigger-string 42) "43"))

;; Exercise 2. Write a macro that encapsulates the sequential function
;;             composition pattern. For example, (number->string (add1
;;             x)) could be written as (fseq2 x add1 number->string).
;;             In your first version, only support two functions.

(define-simple-macro (fseq2 v f1 f2)
  ((compose f2 f1) v))

(module+ test
  (check-equal? (fseq2 42 add1 number->string) "43"))

;; Exercise 3. Extend your fseq2 macro to support an arbitrary number
;;             of function steps. For example, (fseq x add1
;;             number->string string-length) should work. Experiment
;;             with how you want to represent a computation like
;;             (number->string (foldr + 0 (map add1 (list 1 2)))).

(define-simple-macro (fseq* v f ...)
  ((apply compose1 (reverse (list f ...))) v)) ; FIX: ugly!

(module+ test
  (check-equal? (fseq* 42 add1 number->string string-length) 2))

;; Exercise 4. Write a version of our simple-for/list macro that
;;             accumulates the results using and rather than using
;;             cons. For example, it should be convenient to encode
;;             the computation (and (even? 2) (and (even? 4) (and
;;             (even? 6) #t))) as (simple-for/and ([x (list 2 4 6)])
;;             (even? x)).

(define-simple-macro (simple-for/and ([x xs]) expr)
  (andmap (lambda (x) expr) xs))

(module+ test
  (check-equal? (simple-for/and ([x (list 2 4 6)]) (even? x))
                #t))

;; Exercise 5. Generalize simple-for/and into simple-for/fold where
;;             the macro application controls the accumulation inside
;;             the body of the macro. For example, we should be able
;;             to write (simple-for/fold ([acc #t]) ([x (list 2 4 6)])
;;             (and (even? x) acc)) to get the same behavior as our
;;             simple-for/and macro.

(define-simple-macro (simple-for/fold ([acc init]) ([x xs]) expr)
  (foldl (lambda (x acc) expr) init xs))

(module+ test
  (check-equal? (simple-for/fold ([acc #t])
                                 ([x (list 2 4 6)])
                                 (and (even? x) acc))
                #t))

;; Exercise 6. In Racket, the cond form requires its body to be
;;             entirely made of question and answer pairs:
;;
;; (cond [question answer]
;;       ...)
;;
;; This has the unfortunate consequence of requiring nested conds when
;; an early questions allows the remaining questions to go deeper into
;; a structure. For example, consider the definition of filter:
;;
;; (define (filter ? l)
;;   (cond [(empty? l) empty]
;;         [else
;;          (define f (first l))
;;          (define fr (filter ? (rest l)))
;;          (cond [(? f) (cons f fr)]
;;                [else fr])]))
;;
;; Define a new macro, condd, that allows this program to be written
;; as:
;;
;; (define (filter ? l)
;;   (condd [(empty? l) empty]
;;          [#:def f (first l)]
;;          [#:def fr (filter ? (rest l))]
;;          [(? f) (cons f fr)]
;;          [else fr]))

(define-syntax (condd stx)
  (syntax-parse stx
    #:literals (else)
    [(_ (else expr))
     ;; =>
     #'expr]

    [(_ [#:def var val] rest ...)
     ;; =>
     #'(let ([var val])
         (condd rest ...))]

    [(_ (test expr) rest ...)
     ;; =>
     #'(if test
           expr
           (condd rest ...))]
    ))

(module+ test
  (check-equal? (let ([? even?]
                      [l '(1 2 3 4)])
                  (condd [(empty? l) empty]
                         [#:def f (first l)]
                         [#:def fr (filter ? (rest l))]
                         [(? f) (cons f fr)]
                         [else fr]))
                '(2 4)))

;; Exercise 7. Some computations are like burritos that sequence
;;             operations, like the following:
;;
;;   (define ((sum-both return bind) mx my)
;;     (bind mx (λ (x)
;;       (bind my (λ (y)
;;         (return (+ x y)))))))
;;
;; These burrito-like computations are abstracted over what it means
;; to sequence operations. For example, we can use sum-both in two
;; very different ways:
;;
;;   (map (sum-both (λ (x) x)
;;                  (λ (ma f) (and ma (f ma))))
;;        (list 3 #f 5)
;;        (list #f 4 6))
;;   ; => (list #f #f 11)
;;
;;   ((sum-both (λ (x) (list x))
;;              (λ (ma f) (append-map f ma)))
;;    (list 1 2 3)
;;    (list 3 4 5))
;;   ; => (list 4 5 6 5 6 7 6 7 8)
;;
;; Define a macro for patterns like this, so sum-both can be written
;; as:
;;
;;   (define ((sum-both return bind) mx my)
;;     (monad-do return bind
;;               [x #:<- mx]
;;               [y #:<- my]
;;               [#:ret (+ x y)]))

(define-syntax (monad-do stx)
  (syntax-parse stx
    [(_ return bind [var #:<- expr] rest ...)
     #'(bind expr (λ (var) (monad-do return bind rest ...)))]

    [(_ return bind [#:ret expr] rest ...)
     #'(return expr)]))

(define ((sum-both return bind) mx my)
  (bind mx (λ (x)
             (bind my (λ (y)
                        (return (+ x y)))))))

(define ((sum-both/do return bind) mx my)
  (monad-do return bind
            [x #:<- mx]
            [y #:<- my]
            [#:ret (+ x y)]))

;;;;;;;;;;

(define-syntax (syntax-fold-do stx)
  (syntax-parse stx
    [(_ return bind curr)
     #'curr]

    [(_ return bind curr [#:ret expr]    rest ...)
     #'(syntax-fold-do return bind (return expr)              rest ...)]

    [(_ return bind curr [var #:<- mvar] rest ...)
     #'(syntax-fold-do return bind (bind mvar (λ (var) curr)) rest ...)]))

(define-syntax (monad-do2 stx)
  (syntax-parse stx
    [(_ return bind rest ...)
     (define rlist (reverse (syntax-e #'(rest ...))))
     #`(syntax-fold-do return bind identity #,@rlist)]))

(define ((sum-both/do2 return bind) mx my)
  (monad-do2 return bind
            [x #:<- mx]
            [y #:<- my]
            [#:ret (+ x y)]))

;;;;;;;;;;

(module+ test
  (for ([fn (list sum-both sum-both/do)])
    (check-equal? (map (fn (λ (x) x)
                         (λ (ma f) (and ma (f ma))))
                       (list 3 #f 5)
                       (list #f 4 6))
                  '(#f #f 11))

    (check-equal? ((fn list
                     (λ (ma f) (append-map f ma)))
                   (list 1 2 3)
                   (list 3 4 5))
                  '(4 5 6 5 6 7 6 7 8))))

;; Exercise 8. Write a macro to encode a DFA that accepts lists of
;;             Racket objects. For example, consider the following
;;             program:
;;
;; (define (any? x) #t)
;;
;; (define-dfa even-length
;;   #:start even
;;   (#:state even #:accepting [any? odd])
;;   (#:state odd  #:rejecting [any? even]))
;;
;; (even-length '())      ; => #t
;; (even-length '(0))     ; => #f
;; (even-length '(0 0))   ; => #t
;; (even-length '(1 1 1)) ; => #f
;;
;; (define ((is? x) y) (equal? x y))
;;
;; (define-dfa represents-odd
;;   #:start not-odd
;;   (#:state not-odd #:rejecting [(is? 0) not-odd] [(is? 1) odd])
;;   (#:state odd     #:accepting [(is? 0) not-odd] [(is? 1) odd]))
;;
;; (represents-odd '()) ; => #f
;; (represents-odd '(0)) ; => #f
;; (represents-odd '(0 1)) ; => #t
;; (represents-odd '(1 0 1)) ; => #t

;; Exercise 9. Modify your DFA macro so the states can accumulate a
;;             value from the start of the execution, and base the
;;             return value on it. For example, consider the following
;;             program:
;;
;; (define-dfa-like length-of-odd
;;   #:start even
;;   #:accumulator len 0
;;   (#:state even #:value #f  [any? (odd  (add1 len))])
;;   (#:state odd  #:value len [any? (even (add1 len))]))
;;
;; (length-of-odd '())      ; => #f
;; (length-of-odd '(0))     ; => 1
;; (length-of-odd '(0 1))   ; => #f
;; (length-of-odd '(1 0 0)) ; => 3

(module+ test
  (displayln 'done))
