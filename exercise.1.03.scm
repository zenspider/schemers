#lang r5rs

;;; *Exercise 1.3:*

;; Define a procedure that takes three numbers as arguments and
;; returns the sum of the squares of the two larger numbers.

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-squares-of-biggers a b c)
  (cond ((>= a b c) (sum-of-squares a b))
        ((>= a c b) (sum-of-squares a c))
        ((>= b a c) (sum-of-squares b a))
        ((>= b c a) (sum-of-squares b c))
        ((>= c a b) (sum-of-squares c a))
        ((>= c b a) (sum-of-squares c b))))

(sum-of-squares-of-biggers 1 2 3)       ; 13

