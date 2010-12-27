#lang racket

(require "../lib/testes.rkt")

;;; Exercise 1.16:

;; Design a procedure that evolves an iterative exponentiation process
;; that uses successive squaring and uses a logarithmic number of
;; steps, as does `fast-expt'.
;;
;; (Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep,
;; along with the exponent n and the base b, an additional state
;; variable a, and define the state transformation in such a way that
;; the product a b^n is unchanged from state to state. At the
;; beginning of the process a is taken to be 1, and the answer is
;; given by the value of a at the end of the process. In general, the
;; technique of defining an "invariant quantity" that remains
;; unchanged from state to state is a powerful way to think about the
;; design of iterative algorithms.)

(define (square n) (* n n))

(define (expt b n)
  (define (iterate b n product)
    (cond
     ((= n 0)   product)
     ((even? n) (iterate (square b) (/ n 2)    product))
     (else      (iterate         b  (- n 1) (* product b)))))
  (iterate b n 1))

(expt 2 2)                              ; 4
(expt 3 3)                              ; 27
(expt 2 7)                              ; 128
(expt 2 8)                              ; 256
(expt 2 10)                             ; 1024

(assert-equal    4 (expt 2 2))
(assert-equal   27 (expt 3 3))
(assert-equal  128 (expt 2 7))
(assert-equal  256 (expt 2 8))
(assert-equal 1024 (expt 2 10))
