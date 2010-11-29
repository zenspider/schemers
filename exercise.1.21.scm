#lang r5rs

;;; Exercise 1.21:

;; Use the `smallest-divisor' procedure to find the smallest divisor
;; of each of the following numbers: 199, 1999, 19999.

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (square n) (* n n))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(smallest-divisor 199)                  ; 199
(smallest-divisor 1999)                 ; 1999
(smallest-divisor 19999)                ; 7
