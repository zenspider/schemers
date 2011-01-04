#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 2.94

;; Using `div-terms', implement the procedure
;; `remainder-terms' and use this to define `gcd-terms' as above.
;; Now write a procedure `gcd-poly' that computes the polynomial GCD
;; of two polys.  (The procedure should signal an error if the two
;; polys are not in the same variable.)  Install in the system a
;; generic operation `greatest-common-divisor' that reduces to
;; `gcd-poly' for polynomials and to ordinary `gcd' for ordinary
;; numbers.  As a test, try
;;
;;      (define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;;      (define p2 (make-polynomial 'x '((3 1) (1 -1))))
;;      (greatest-common-divisor p1 p2)
;;
;; and check your result by hand.

;; (assert-equal x y)
(done)
