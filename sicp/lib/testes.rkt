#lang racket

(provide assert assert-equal done)

(define (assert test)
  (if test (display ".")
      (error "failed")))

(define (assert-equal a b)
  (if (equal? a b) (display ".")
      (error 'assert-equal "(equal? ~s ~s)~n" a b)))

(define (done)
  (display "done!")
  (newline))
