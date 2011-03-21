#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.17

;; Draw diagrams of the environment in effect when
;; evaluating the expression <E3> in the procedure in the text,
;; comparing how this will be structured when definitions are
;; interpreted sequentially with how it will be structured if
;; definitions are scanned out as described.  Why is there an extra
;; frame in the transformed program?  Explain why this difference in
;; environment structure can never make a difference in the behavior
;; of a correct program.  Design a way to make the interpreter
;; implement the "simultaneous" scope rule for internal definitions
;; without constructing the extra frame.

;; A: there's an extra frame because of the extend-environment in
;; apply on compound-procedure.
;;
;; This won't make a difference in a correct program because the
;; transformation is algebraically equivalent to the pre-transformed
;; code. Adding any number of frames won't make a difference as long
;; as the union of the frames is always the same.
;;
;; One way to get rid of the extra scope is to get rid of the
;; let->lambda phase. Expose define-variable so that set! will create
;; the var in the current scope, then you can skip the let phase
;; entirely:

;; (lambda (...)
;;   (letrec ((a (lambda () ...))
;;            (b (lambda () ...)))
;;     (f a b)))

;; becomes:

;; (lambda (...)
;;   (define-var 'a 'b)
;;   (set! a (lambda () ...))
;;   (set! b (lambda () ...))
;;   (f a b))

;; instead of:

;; (lambda (...)
;;   ((lambda (a b)
;;      (set! a (lambda () ...))
;;      (set! b (lambda () ...))
;;      (f a b)) 42 42))
