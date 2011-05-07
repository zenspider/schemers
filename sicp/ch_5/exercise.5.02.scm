#!/usr/bin/env csi -s

;;; Exercise 5.2

;; Use the register-machine language to describe the iterative
;; factorial machine of *Note Exercise 5-1::.

;; (controller
;;  factorial
;;  (assign product (const 1))
;;  (assign counter (const 1))
;;  factorial-top
;;  (test (op >) (reg counter) (reg n))
;;  (branch (label factorial-done))
;;  (assign t (op *) (reg counter) (reg product))
;;  (assign product (reg t))
;;  (assign t (op +) (reg counter) (contst 1))
;;  (assign counter (reg t))
;;  (goto (label factorial-top))
;;  factorial-done)
