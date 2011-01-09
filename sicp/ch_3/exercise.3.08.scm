#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.8

;; When we defined the evaluation model in section
;; *Note 1-1-3::, we said that the first step in evaluating an
;; expression is to evaluate its subexpressions.  But we never
;; specified the order in which the subexpressions should be
;; evaluated (e.g., left to right or right to left).  When we
;; introduce assignment, the order in which the arguments to a
;; procedure are evaluated can make a difference to the result.
;; Define a simple procedure `f' such that evaluating `(+ (f 0) (f
;; 1))' will return 0 if the arguments to `+' are evaluated from left
;; to right but will return 1 if the arguments are evaluated from
;; right to left.

;; (assert-equal x y)
(done)
