#lang racket

;;; Exercise 2.12:

;; Define a constructor `make-center-percent' that takes a center and
;; a percentage tolerance and produces the desired interval. You must
;; also define a selector `percent' that produces the percentage
;; tolerance for a given interval. The `center' selector is the same
;; as the one shown above.

(define make-interval cons)
(define upper-bound cdr)
(define lower-bound car)

(define (make-center-percent c p)
  (make-interval (* c (- 1.0 p)) (* c (+ 1.0 p))))

(define r1 (make-interval (* 0.90 6.8) (* 1.10 6.8)))
(define r2 (make-interval (* 0.95 4.7) (* 1.05 4.7)))

(define r3 (make-center-percent 6.8 1/10))
(define r4 (make-center-percent 4.7 1/20))

(define (i= i j)
  (and (= (upper-bound i) (upper-bound j))
       (= (lower-bound i) (lower-bound j))))

(i= r1 r3)
(i= r2 r4)
