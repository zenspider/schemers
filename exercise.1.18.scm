#lang r5rs

;;; Exercise 1.18:

;; Using the results of *Note Exercise 1-16:: and *Note Exercise
;; 1-17::, devise a procedure that generates an iterative process for
;; multiplying two integers in terms of adding, doubling, and halving
;; and uses a logarithmic number of steps.

;; um... I did that in 1.17

(define (mult a b)
  (define (iterate a b p)
    (cond ((= b 0) p)
          ((even? b) (iterate (double a) (halve b) p))
          (else      (iterate a (- b 1) (+ p a)))))
  (iterate a b 0))

;; ah... ok. I messed up by doing half tail-recursive and half not. By
;; adding an extra variable p (product) we can reorder my solution
;; cleanly to be fully tail recursive.

(mult 1 3)                              ; 3
(mult 3 1)                              ; 3
(mult 31 31)                            ; 961
