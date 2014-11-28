#lang racket/base

;;; *Exercise 1.6:*

;; prelude:

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)                                ;    3.00009155413138
(sqrt (+ 100 37))                       ;   11.704699917758145
(sqrt (+ (sqrt 2) (sqrt 3)))            ;    1.7739279023207892
(square (sqrt 1000))                    ; 1000.000369924366

;; Alyssa P. Hacker doesn't see why `if' needs to be provided as a
;; special form. "Why can't I just define it as an ordinary procedure
;; in terms of `cond'?" she asks. Alyssa's friend Eva Lu Ator claims
;; this can indeed be done, and she defines a new version of `if':

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)                    ; 5
(new-if (= 1 1) 0 5)                    ; 0

;; Delighted, Alyssa uses `new-if' to rewrite the square-root program:

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                     x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

;; What happens when Alyssa attempts to use this to compute square
;; roots?  Explain.

(sqrt 9)                                ; infinite loop...

;; this is because new-if as used by sqrt-iter is not a special form
;; and even using applicative order still tries to expand the
;; arguments. By expanding the alternative, you recurse into sqrt-iter
;; and repeat the recursion w/o a guard.
