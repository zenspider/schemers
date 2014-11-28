#!/usr/bin/env csi -s

(require rackunit)
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

;; since aja was lazy, so am I: fib

(define env (setup-environment))

(define-syntax test-eval
  (syntax-rules ()
    ((_ exp ast) (test exp (eval ast env)))))

(test-group "4.29"
  (test-eval 'ok '(define count 0))
  (test-eval 'ok '(define (id x) (set count (+ count 1)) x))
  (test-eval 'ok '(define (square x) (* x x)))
  (test-eval 'ok '(define s10 (square (id 10))))
  (test-eval   1 'count)
  (test-eval 100 's10)
  (test-eval   1 'count)
  (test-eval 100 's10)
  (test-eval   1 'count)
  (test-eval 100 '(square (id 10)))
  (test-eval   2 'count)
  (test-eval 100 '(square (id 10)))
  (test-eval   3 'count))

;; idgi