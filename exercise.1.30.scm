#lang racket

;;; Exercise 1.30:

;; The `sum' procedure above generates a linear recursion. The
;; procedure can be rewritten so that the sum is performed
;; iteratively. Show how to do this by filling in the missing
;; expressions in the following definition:

(define (sum f a b n)
  (if (> a b) 0
      (+ (f a)
         (sum f (n a) b n))))

(define (sum f a b n)
  (define (iterate a result)
    (if (> a b) result
        (iterate (n a) (+ result (f a)))))
  (iterate a 0))

(define (identity n) n)
(sum identity 1 10 inc)

(= (/ (+ (* 10 10) 10) 2) (sum identity 1 10 inc))