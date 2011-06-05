#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.31

;; In evaluating a procedure application, the
;; explicit-control evaluator always saves and restores the `env'
;; register around the evaluation of the operator, saves and restores
;; `env' around the evaluation of each operand (except the final
;; one), saves and restores `argl' around the evaluation of each
;; operand, and saves and restores `proc' around the evaluation of
;; the operand sequence.  For each of the following combinations, say
;; which of these `save' and `restore' operations are superfluous and
;; thus could be eliminated by the compiler's `preserving' mechanism:
;;
;;   1. (f 'x 'y)
;;   2. ((f) 'x 'y)
;;   3. (f (g 'x) y)
;;   4. (f (g 'x) 'y)

;; A: I believe the answer is 1 & 2 since the lookup of f in both
;; cases doesn't require writing to env. In the case of 3 & 4, the
;; second argument is evaluated in applicative order and need to
;; create their own env layer for g's argument. That env should be
;; popped before evaluating the rest of the args of f and then apply f
;; itself.
