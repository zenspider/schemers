#lang racket/base

(require (for-syntax racket/base)
         "time.rkt")

(define-syntax (time-it stx)
  (define es (syntax->list stx))
  (define body (list-ref es 1))
  #`(thunk-time-it (lambda () #,body)))

(time-it (+ 4 1))
