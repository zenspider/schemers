#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 4.68

;; Define rules to implement the `reverse' operation
;; of *Note Exercise 2-18::, which returns a list containing the same
;; elements as a given list in reverse order.  (Hint: Use
;; `append-to-form'.)  Can your rules answer both `(reverse (1 2 3)
;; ?x)' and `(reverse ?x (1 2 3))' ?
