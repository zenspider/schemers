#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.13

;; Scheme allows us to create new bindings for variables by means of
;; `define', but provides no way to get rid of bindings. Implement for
;; the evaluator a special form `make-unbound!' that removes the
;; binding of a given symbol from the environment in which the
;; `make-unbound!' expression is evaluated. This problem is not
;; completely specified. For example, should we remove only the
;; binding in the first frame of the environment? Complete the
;; specification and justify any choices you make.

(use srfi-1)                            ; cheating! alist-delete! rules!

(define (make-unbound! env var)
  (alist-delete! var (first-frame env)))

(define (identity x) x)

(test-group "4.13"
    (let ((env '(((a 1) (b 2) (c 3)))))
      (make-unbound! env 'b)

      (test '(((a 1) (c 3))) (identity env))))