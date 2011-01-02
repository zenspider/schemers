#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 2.58:

;; Suppose we want to modify the differentiation program so that it
;; works with ordinary mathematical notation, in which `+' and `*' are
;; infix rather than prefix operators. Since the differentiation
;; program is defined in terms of abstract data, we can modify it to
;; work with different representations of expressions solely by
;; changing the predicates, selectors, and constructors that define
;; the representation of the algebraic expressions on which the
;; differentiator is to operate.
;;
;;   a. Show how to do this in order to differentiate algebraic
;;      expressions presented in infix form, such as `(x + (3 * (x +
;;      (y + 2))))'.  To simplify the task, assume that `+' and `*'
;;      always take two arguments and that expressions are fully
;;      parenthesized.
;;
;;   b. The problem becomes substantially harder if we allow standard
;;      algebraic notation, such as `(x + 3 * (x + y + 2))', which
;;      drops unnecessary parentheses and assumes that multiplication
;;      is done before addition.  Can you design appropriate
;;      predicates, selectors, and constructors for this notation
;;      such that our derivative program still works?

;; TODO

;; (assert-equal x y)
(done)
