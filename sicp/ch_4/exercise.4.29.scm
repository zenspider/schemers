#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.29

;; Exhibit a program that you would expect to run
;; much more slowly without memoization than with memoization.  Also,
;; consider the following interaction, where the `id' procedure is
;; defined as in *Note Exercise 4-27:: and `count' starts at 0:
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
