#!/usr/bin/env csi -s

(require rackunit)
(require-library eval)
(import eval)

;;; Exercise 4.12

;; The procedures set-variable-value!, define-variable!, and
;; lookup-variable-value can be expressed in terms of more abstract
;; procedures for traversing the environment structure. Define
;; abstractions that capture the common patterns and redefine the
;; three procedures in terms of these abstractions.

;; STUPID. see 4.11