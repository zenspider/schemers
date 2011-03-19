#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.11

;; Instead of representing a frame as a pair of
;; lists, we can represent a frame as a list of bindings, where each
;; binding is a name-value pair.  Rewrite the environment operations
;; to use this alternative representation.

(test-group "4.11"
    (let ((env (extend-environment '() '() the-empty-environment)))

      (test `(proc (a b c) 42 ,env) (make-procedure '(a b c) 42 env))

      (define-variable! 'a 42 env)
      (test 42 (lookup-variable-value 'a env))

      (set-variable-value! 'a 24 env)
      (test 24 (lookup-variable-value 'a env))
      ))