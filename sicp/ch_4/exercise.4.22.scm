#!/usr/bin/env csi -s

(require rackunit)
(require-library eval2)
(import eval2)

;;; Exercise 4.22

;; Extend the evaluator in this section to support
;; the special form `let'.  (See *Note Exercise 4-6::.)

(define env (setup-environment))

(test-group "eval2 sanity check"
  (test 42  (eval 42                         env))
  (test 'x  (eval '(quote x)                 env))
  (test 'ok (eval '(define y 42)             env))
  (test 42  (eval 'y                         env))
  (test 'ok (eval '(set y 24)                env))
  (test 24  (eval 'y                         env))
  (test 42  (eval '(if true  42 24)          env))
  (test 24  (eval '(if false 42 24)          env))
  (test 42  (eval '((lambda (n) (* 2 n)) 21) env)))

;; See eval2.scm's analyze:
;;    ((let? exp) (analyze (let->combination exp)))

(test-group "4.22"
  (define mylet '(let ((a 1)
                       (b 2)
                       (c 3))
                   (+ a b c)))

  (test 6 (eval mylet env)))
