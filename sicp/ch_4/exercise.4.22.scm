#!/usr/bin/env csi -s

(use test)
(require-library eval2)
(import eval2)

;;; Exercise 4.22

;; Extend the evaluator in this section to support
;; the special form `let'.  (See *Note Exercise 4-6::.)

(define env (setup-environment))

(test 42 (eval 42 env))
(test 'x (eval '(quote x) env))

(test 'ok (eval '(define y 42) env))
(test 42 (eval 'y env))

(test 'ok (eval '(set y 24) env))
(test 24 (eval 'y env))

(test 42 (eval '(if true  42 24) env))
(test 24 (eval '(if false 42 24) env))
