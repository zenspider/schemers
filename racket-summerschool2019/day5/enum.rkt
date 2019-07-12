#lang racket

(require syntax/parse/define)

#;
(define-simple-macro (define-enum class:id name:id ...)
  ;; FIX: figure out how to introduce more hygene?
  #:with cleaned-names #'(list (format-id "~a/~a" #'class #'name) ...)
  (for ([clean-name (in-list cleaned-names)])
     (define clean-name 'clean-name)))

(define-simple-macro (define-enum class:id x:id ...)
  (begin
    (define-syntax class (list 'x ...))
    (define x 'x) ...))

(define-syntax (enum-match stx)
  (syntax-parse stx
    [(_ class:id e:expr [id:id val:expr] ...)
     (define enum (syntax-local-value #'class))
     (define used (syntax->list #'(id ...)))

     (for ([id (in-list used)])
       (unless (member (syntax-e id) enum)
         (raise-syntax-error 'enum-match "bad enum" stx id)))

     #'(case e [(id) val] ...)]))

(define-enum animal
  anteater
  dumbo
  snake)

#;
(define-enum collision
  anteater
  dumbo
  snake)

(define (food-of a)
  (enum-match animal a
              [anteater 'ants]
              [dumbo 'peanuts]
              [snake 'rats]))

(food-of 'anteater)
(food-of anteater)
