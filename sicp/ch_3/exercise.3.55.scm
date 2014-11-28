#lang racket/base

(require (except-in "../lib/streams.scm" ones integers))
(require "../lib/test.rkt")

;;; Exercise 3.55

;; Define a procedure `partial-sums' that takes as
;; argument a stream S and returns the stream whose elements are S_0,
;; S_0 + S_1, S_0 + S_1 + S_2, ....  For example, `(partial-sums
;; integers)' should be the stream 1, 3, 6, 10, 15, ....

(define ones     (stream-cons 1 ones))
(define integers (stream-cons 1 (stream-add ones integers)))

(define (old-partial-sums s)
  (define sums
    (stream-cons (stream-car s)
                 (stream-add sums
                             (stream-cdr s))))
  sums)

(define (partial-sums s)
  (stream-cons (stream-car s)
               (stream-add (partial-sums s)
                           (stream-cdr s))))

(test-group "3.55"
            (let ((numbers (partial-sums integers)))
              (test '(1 3 6 10 15 21 28 36 45 55)
                    (stream-head numbers 10))))
