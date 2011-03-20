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
    (test #f #t))