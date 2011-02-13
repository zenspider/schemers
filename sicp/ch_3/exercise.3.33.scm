
(use test)
(require 'constraints)
(import constraints)

;;; Exercise 3.33

;; Using primitive multiplier, adder, and constant
;; constraints, define a procedure `averager' that takes three
;; connectors `a', `b', and `c' as inputs and establishes the
;; constraint that the value of `c' is the average of the values of
;; `a' and `b'.

(define (averager m n avg)
  (let ((sum (make-connector))
        (two (make-connector)))
    (adder m n sum)
    (multiplier avg two sum)
    (constant 2 two)
    'ok))

(test-group "3.33"
            (define M (make-connector))
            (define N (make-connector))
            (define A (make-connector))

            (define (test-average m n a)
              (forget-value! M 'user)
              (forget-value! N 'user)
              (forget-value! A 'user)
              (set-value! M m 'user)
              (set-value! N n 'user)

              (test a (get-value A)))

            (averager M N A)
            (test-average 0 0 0)
            (test-average 2 0 1.0)
            (test-average 2 4 3.0))
