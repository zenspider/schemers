#lang racket/base

(require "../lib/test.rkt")
(require "../lib/myutils.scm")

;; Exercise 2.34

;; Evaluating a polynomial in x at a given value of x can be
;; formulated as an accumulation. We evaluate the polynomial
;;
;;      a[n] r^n | a[(n-1)] r^(n-1) + ... + a[1] r + a[0]
;;
;; using a well-known algorithm called "Horner's rule", which
;; structures the computation as
;;
;;      (... (a[n] r + a[(n-1)]) r + ... + a[1]) r + a[0]
;;
;; In other words, we start with a[n], multiply by x, add a[(n-1)],
;; multiply by x, and so on, until we reach a[0].(3)
;;
;; Fill in the following template to produce a procedure that
;; evaluates a polynomial using Horner's rule.  Assume that the
;; coefficients of the polynomial are arranged in a sequence, from
;; a[0] through a[n].
;;
;;      (define (horner-eval x coefficient-sequence)
;;        (accumulate (lambda (this-coeff higher-terms) <??>)
;;                    0
;;                    coefficient-sequence))
;;
;; For example, to compute 1 + 3x + 5x^3 + x^5 at x = 2 you would
;; evaluate
;;
;;      (horner-eval 2 (list 1 3 0 5 0 1))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(assert-equal 10 (horner-eval  0 '(10)))
(assert-equal 13 (horner-eval 10 '(3 1)))
(assert-equal 79 (horner-eval  2 '(1 3 0 5 0 1)))
(done)
