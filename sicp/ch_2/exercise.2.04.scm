#lang racket/base

;;; Exercise 2.4:

;; Here is an alternative procedural representation of pairs. For this
;; representation, verify that `(car (cons x y))' yields `x' for any
;; objects `x' and `y'.

(define (xcons x y)
  (lambda (m) (m x y)))

(define (xcar z)
  (z (lambda (p q) p)))

(xcar (xcons (xcons 'a 'b) 'c))

;; What is the corresponding definition of `cdr'? (Hint: To verify
;; that this works, make use of the substitution model of section
;; *Note 1-1-5::.)

(define (xcdr z)
  (z (lambda (p q) q)))

(xcdr (xcons (xcons 'a 'b) 'c))
