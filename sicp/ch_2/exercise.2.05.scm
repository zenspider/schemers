#lang racket/base

;;; Exercise 2.5:

;; Show that we can represent pairs of nonnegative integers using only
;; numbers and arithmetic operations if we represent the pair a and b
;; as the integer that is the product 2^a 3^b. Give the corresponding
;; definitions of the procedures `cons', `car', and `cdr'.

(define (xcount-factors b n count)
  (if (> (remainder n b) 0) count
      (xcount-factors b (/ n b) (+ count 1))))

(define (xcons a b)
  (* (expt 2 a) (expt 3 b)))

(define (xcar p)
  (xcount-factors 2 p 0))

(define (xcdr p)
  (xcount-factors 3 p 0))

(xcar (xcons 2 3))
(xcdr (xcons 2 3))
