#!/usr/bin/env csi -s

(use test)
(require-library lazy-eval)
(import lazy-eval)

;;; Exercise 4.29

;; Exhibit a program that you would expect to run much more slowly
;; without memoization than with memoization. Also, consider the
;; following interaction, where the `id' procedure is defined as in
;; *Note Exercise 4-27:: and `count' starts at 0:
;;
;;      (define (square x)
;;        (* x x))
;;
;;      ;;; L-Eval input:
;;      (square (id 10))
;;      ;;; L-Eval value:
;;      <RESPONSE>
;;
;;      ;;; L-Eval input:
;;      count
;;      ;;; L-Eval value:
;;      <RESPONSE>
;;
;; Give the responses both when the evaluator memoizes and when it
;; does not.

(define env (setup-environment))
(test 'ok (eval '(define count 0) env))
(test 'ok (eval '(define (id x) (set count (+ count 1)) x) env))
(test 'ok (eval '(define (square x) (* x x)) env))
(test 'ok (eval '(define s10 (square (id 10))) env))
(test   1 (eval 'count            env))
(test 100 (eval 's10              env))
(test   1 (eval 'count            env))
(test 100 (eval 's10              env))
(test   1 (eval 'count            env))
(test 100 (eval '(square (id 10)) env))
(test   2 (eval 'count            env))
(test 100 (eval '(square (id 10)) env))
(test   3 (eval 'count            env))

;; idgi