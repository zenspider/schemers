#lang racket/base

;;; Exercise 1.38:

;; In 1737, the Swiss mathematician Leonhard Euler published a memoir
;; `De Fractionibus Continuiss', which included a continued fraction
;; expansion for e - 2, where e is the base of the natural logarithms.
;; In this fraction, the n[i] are all 1, and the D[i] are successively
;; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses
;; your `cont-frac' procedure from *Note Exercise 1-37:: to
;; approximate e, based on Euler's expansion.

(define (cont-frac n d i)
  (define (iterate i fraction)
    (if (< i 1) fraction
        (iterate (- i 1)
                 (/ (n i)
                    (+ (d i) fraction)))))
  (iterate i 0))

(define (e-approx k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i) (if (= (remainder (+ i 1) 3) 0)
                                  (* (/ (+ i 1) 3) 2)
                                  1))
                  k)))

;; Math::E = 2.71828182845905

(e-approx 1)                            ; 3.0
(e-approx 5)                            ; 2.71875
(e-approx 10)                           ; 2.7182817182817183
(e-approx 15)                           ; 2.718281828470584
(e-approx 20)                           ; 2.718281828459045
