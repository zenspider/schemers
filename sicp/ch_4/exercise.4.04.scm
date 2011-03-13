#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.4

;; Recall the definitions of the special forms `and'
;; and `or' from *Note Chapter 1:::
;;
;;    * `and': The expressions are evaluated from left to right.  If
;;      any expression evaluates to false, false is returned; any
;;      remaining expressions are not evaluated.  If all the
;;      expressions evaluate to true values, the value of the last
;;      expression is returned.  If there are no expressions then
;;      true is returned.
;;
;;    * `or': The expressions are evaluated from left to right.  If
;;      any expression evaluates to a true value, that value is
;;      returned; any remaining expressions are not evaluated.  If
;;      all expressions evaluate to false, or if there are no
;;      expressions, then false is returned.
;;
;;
;; Install `and' and `or' as new special forms for the evaluator by
;; defining appropriate syntax procedures and evaluation procedures
;; `eval-and' and `eval-or'.  Alternatively, show how to implement
;; `and' and `or' as derived expressions.

(require-library eval)
(import eval)

;; add the following to eval:

;; ((and? exp) (eval-and (cdr exp) env))
;; ((or?  exp) (eval-or  (cdr exp) env))

;; or run the following:

;; (def-set 'and (lambda (exp env) (eval-and exp env)))
;; (def-set 'or  (lambda (exp env) (eval-or  exp env)))

;; only necessary for non-data-driven eval:

(define (and? exp) (tagged-list? 'and exp))
(define (or? exp) (tagged-list? 'or exp))

;; needed in both cases:

(define (eval-and exp env)
  (if (null? exp) #t
      (and (eval     (car exp) env)
           (eval-and (cdr exp) env))))

(define (eval-or exp env)
  (if (null? exp) #f
      (or (eval    (car exp) env)
          (eval-or (cdr exp) env))))
