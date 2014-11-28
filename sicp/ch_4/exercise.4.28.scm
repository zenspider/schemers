#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 4.28

;; `Eval' uses `actual-value' rather than `eval' to
;; evaluate the operator before passing it to `apply', in order to
;; force the value of the operator.  Give an example that
;; demonstrates the need for this forcing.

;; wouldn't something as simple as this count?

(define call (lambda (f x) (f x)))
(define add1 (lambda (n) (+ n 1)))
(call add1 2)
