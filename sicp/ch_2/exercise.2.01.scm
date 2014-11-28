#lang racket/base

;;; Exercise 2.1:

;; Define a better version of `make-rat' that handles both positive
;; and negative arguments. `Make-rat' should normalize the sign so
;; that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; n d
;; + + no change
;; - + no change
;; - - both postiive (swap both)
;; + - swap          (swap both)

(define (make-rat n d)
  (if (< d 0)
      (make-rat (- n) (- d))
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g)))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(define neg-one-half  (make-rat -1  2))
(define neg-one-third (make-rat  1 -3))
(define one-fourth    (make-rat -1 -4))

neg-one-half
neg-one-third
one-fourth
