#lang racket

(require syntax/parse/define
         (for-syntax racket/match))

(provide (rename-out [define-five define]))

(define-syntax (define-five stx)
  #;
  (begin
    (define e (syntax->list stx))
    (define k (list-ref e 1))
    (define v (list-ref e 2))
    (printf "Assuming that ~s produces 5~n" (syntax->datum v))
    #`(define #,k 5))

  #;
  (match (syntax->list stx)
    [`(,_ ,k ,v)
     (printf "Assuming that ~s produces 5~n" (syntax->datum v))
     #`(define #,k 5)])

  (syntax-parse stx
    [(_ k v)
     (printf "Assuming that ~s produces 5~n" (syntax->datum #'v))
     #`(define k 5)])
  )

(define-five x 42)
(define-five y (+ 1 2))

(+ x y)
