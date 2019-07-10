#lang racket/base

(require (for-syntax racket/base
                     racket/contract
                     syntax/parse)
         racket/contract
         racket/list
         racket/function
         syntax/parse/define)

(module+ test
  (require rackunit
           syntax/macro-testing))

;; Exercise 10. Add syntax class annotations to your define&provide
;; macro. Write test cases to demonstrate the bad behavior you are
;; protecting against.

(define-simple-macro (define* (name:id [arg:id type] ...
                                       #:-> out-type)
                       body:expr ...)
  #:declare type     (expr/c #'contract?)
  #:declare out-type (expr/c #'contract?)
  (define/contract (name arg ...) (-> type.c ... out-type.c) body ...))

(define* (bigger-string* [x number?] #:-> string?)
  (number->string (add1 x)))

(module+ test
  (check-equal? (bigger-string* 42) "43")

  (check-exn #rx"object%: unbound identifier"
             (lambda ()
              (convert-compile-time-error
               (let ()
                (define* (bigger-string*/bad/1 [x object%] #:-> string?)
                  (number->string (add1 x)))
                42)))))

;; Exercise 11. Add syntax class annotations to your simple-for/fold
;; macro. Write test cases to demonstrate the bad behavior you are
;; protecting against.

(define-simple-macro (simple-for/fold ([acc:id init:expr]) ([x:id xs])
                                      expr:expr)
  #:declare xs (expr/c #'list?)
  (foldl (lambda (x acc) expr) init xs.c))

(module+ test
  (check-equal? (simple-for/fold ([acc #t])
                                 ([x (list 2 4 6)])
                                 (and (even? x) acc))
                #t)

  (check-exn #rx"simple-for/fold: expected identifier.*at: 42"
             (lambda ()
              (convert-compile-time-error
               (simple-for/fold ([42 #t])
                                ([x (list 2 4 6)])
                                (and (even? x) acc)))))
  (check-exn #rx"simple-for/fold: expected identifier.*at: 42"
             (lambda ()
              (convert-compile-time-error
               (simple-for/fold ([acc #t])
                                ([42 (list 2 4 6)])
                                (and (even? x) acc)))))
  (check-exn #rx"simple-for/fold: contract violation.*expected: list?.*given: 42"
             (lambda ()
              (convert-compile-time-error
               (simple-for/fold ([acc #t])
                                ([x 42])
                                (and (even? x) acc)))))
  )

;; Exercise 12. Add syntax class annotations to your condd macro.
;; Write test cases to demonstrate the bad behavior you are protecting
;; against.

(define-syntax (condd stx)
  (syntax-parse stx
    #:literals (else)
    [(_ (else expr))                 #'expr]
    [(_ [#:def var:id val] rest ...) #'(let ([var val]) (condd rest ...))]
    [(_ (test expr) rest ...)        #'(if test expr    (condd rest ...))]
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

;; Exercise 13. Define a syntax class that identifies valid formals
;; for a lambda. You can test it with the macro
;;
;; (define-simple-macro (my-lambda fs:my-formals body:expr ...+)
;;   (lambda fs body ...))
;;
;; But, all invalid forms should be an error in my-lambda, not an
;; error in lambda.
;;
;; Start by handling only normal arguments, then support rest
;; arguments, then add default arguments, finally add keyword
;; arguments. If this was too easy, extend your class to properly
;; error when the same keyword is used multiple times, as well as
;; erroring if the same identifier is used multiple times.

(begin-for-syntax
  (define-syntax-class my-formals
    #:attributes ([x 1])
    (pattern (x:id ...)
             #:fail-when
             (check-duplicate-identifier (syntax->list #'(x ...)))
             "Duplicate ids"
             )))

(define-simple-macro (my-lambda fs:my-formals body:expr ...+)
  (lambda fs body ...))

(module+ test
  (check-equal? ((my-lambda (x) (+ x 1)) 42) 43)

  (check-exn #rx"my-lambda: Duplicate ids.*at: x"
             (lambda ()
              (convert-compile-time-error
               ((my-lambda (x x) (+ x 1)) 42)))))

;; Exercise 14. Add support for the [question => answer-function] form
;; of cond to your condd macro by designing a syntax class and using
;; it. We recommend defining separate condd-test-clause and
;; condd-define-clause syntax classes, rather than trying to normalize
;; these two different kinds of clauses.

(begin-for-syntax
  (define-syntax-class condd-test-expr
    #:attributes ([t 0] [e 0])
    (pattern (t:expr e:expr)))

  (define-syntax-class condd-test-lambda
    #:attributes ([t 0] [l 0])
    (pattern (t:expr #:=> l)
             #:declare l (expr/c #'procedure?))))

(define-syntax (condd* stx)
  (syntax-parse stx
    #:literals (else)
    [(_ (else expr))                  #'expr]
    [(_ [#:def var:id val] rest ...)  #'(let ([var val]) (condd* rest ...))]
    [(_ c:condd-test-expr  rest ...)  #'(if c.t c.e    (condd* rest ...))]
    [(_ c:condd-test-lambda rest ...) #'(let ([test-val c.t])
                                          (if test-val (c.l test-val)
                                              (condd* rest ...)))]
    ))

(module+ test
  (check-equal? (let ([? even?]
                      [l '(1 2 3 4)])
                  (condd* [(empty? l) empty]
                          [#:def f (first l)]
                          [#:def fr (filter ? (rest l))]
                          [(? f) (cons f fr)]
                          [else fr]))
                '(2 4))
  (check-equal? (let ([l '(1 2 3 4)])
                  (condd* [(list? l) #:=> identity]
                          [else #f]))
                #t)
  )

;; Exercise 15. Write a syntax class called match-pattern that allows
;; this macro:
;;
;; (define-simple-macro
;;   (simple-match-define mp:match-pattern scrutinee:expr)
;;   (define-values (mp.x ...)
;;     (mp.matcher
;;      scrutinee
;;      (λ () (error 'simple-match-define "Match does not apply!")))))
;;
;; (define (user s)
;;   (simple-match-define
;;    (#:cons (#:or (#:number x) (#:boolean x))
;;     (#:cons (#:number y)
;;      (#:or (#:null z) (#:boolean z))))
;;    s)
;;   (if (number? x) (+ x y)
;;       (if x y z)))
;;
;; (user (cons 1 (cons 2 empty)))
;; ; => 3
;;
;; (user (cons #t (cons 2 empty)))
;; ; => 2
;;
;; (user (cons #t (cons 2 #f)))
;; ; => 2
;;
;; (user (cons #t (cons 2 #t)))
;; ; => 2
;;
;; (user (cons #f (cons 2 #t)))
;; ; => #t
;;
;; (user (cons #f (cons 2 #f)))
;; ; => #t
;;
;; Your match-pattern class will support five different variants:
;; #:cons, #:or, #:number, #:boolean, and #:null. The #:cons and #:or
;; variants will be recursive.
;;
;; The matcher attribute should be the syntax of a function that
;; accepts a value and a function to call if it does not match. The
;; return values should be the matched values.

(module+ test
  (displayln 'done))
