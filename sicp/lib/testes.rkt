#lang racket

(provide assert assert-equal done)

(define (assert test)
  (if test (display ".")
      (error "failed")))

(define (assert-equal a b)
  (assert (equal? a b)))

(define (done)
  (display "done!")
  (newline))
