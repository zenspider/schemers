#lang racket/base

(require (except-in "../lib/streams.scm" ones integers))
(require "../lib/test.rkt")

;;; Exercise 3.58

;; Give an interpretation of the stream computed by
;; the following procedure:

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; (`Quotient' is a primitive that returns the integer quotient of two
;; integers.)  What are the successive elements produced by `(expand
;; 1 7 10)'?  What is produced by `(expand 3 8 10)'?

(test-group "3.58"
            (test '(1 4 2 8 5 7) (stream-head (expand 1 7 10) 6))
            (test '(3 7 5)       (stream-head (expand 3 8 10) 3)))

(define q quotient)
(define r remainder)

(list (q 10 7) ; (r 10 7) 3
      (q 30 7) ; (r 30 7) 2
      (q 20 7) ; (r 20 7) 6
      (q 60 7) ; (r 60 7) 4
      (q 40 7) ; (r 40 7) 5
      (q 50 7) ; (r 50 7) 1
      )

(list (q 30 8) ; (r 30 8) 6
      (q 60 8) ; (r 60 8) 4
      (q 40 8) ; (r 40 8) 0
      )
