#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 4.3

;; Rewrite `eval' so that the dispatch is done in data-directed style.
;; Compare this with the data-directed differentiation procedure of
;; *Note Exercise 2-73::. (You may use the `car' of a compound
;; expression as the type of the expression, as is appropriate for the
;; syntax implemented in this section.)

(require-library eval)
(import eval)

(define defines '())

(define (def-set name lambda)
  (set! defines (append defines (list (list name lambda)))))

(define (def-get name)
  (cdr (or (assoc name defines) (assoc 'apply defines))))

(define (dd-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable?        exp) (lookup-variable-value exp env))
        (else
         ((def-get (operator exp)) (operands exp) env))))

(def-set 'quote  (lambda (exp env) (text-of-quotation exp)))
(def-set 'set!   (lambda (exp env) (eval-assignment exp env)))
(def-set 'define (lambda (exp env) (eval-definition exp env)))
(def-set 'if     (lambda (exp env) (eval-if exp env)))
(def-set 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                   (lambda-body exp) env)))
(def-set 'begin  (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(def-set 'cond   (lambda (exp env) (eval (cond->if exp) env)))
(def-set 'apply  (lambda (exp env) (apply (eval (operator exp) env)
                                          (list-of-values (operands exp) env))))
