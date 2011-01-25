
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.1

;; An "accumulator" is a procedure that is called
;; repeatedly with a single numeric argument and accumulates its
;; arguments into a sum.  Each time it is called, it returns the
;; currently accumulated sum.  Write a procedure `make-accumulator'
;; that generates accumulators, each maintaining an independent sum.
;; The input to `make-accumulator' should specify the initial value
;; of the sum; for example
;;
;;      (define A (make-accumulator 5))
;;
;;      (A 10)
;;      15
;;
;;      (A 10)
;;      25

(define (make-accumulator value)
  (lambda (n)
    (set! value (+ value n))
    value))

(define A (make-accumulator 5))

(assert-equal 15 (A 10))
(assert-equal 25 (A 10))
(done)
