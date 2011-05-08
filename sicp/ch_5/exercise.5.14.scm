#!/usr/bin/env csi -s

(use test)
(use extras)
(require-library machine)
(import machine)

;;; Exercise 5.14

;; Measure the number of pushes and the maximum stack depth required
;; to compute n! for various small values of n using the factorial
;; machine shown in *Note Figure 5-11::. From your data determine
;; formulas in terms of n for the total number of push operations and
;; the maximum stack depth used in computing n! for any n > 1. Note
;; that each of these is a linear function of n and is thus determined
;; by two constants. In order to get the statistics printed, you will
;; have to augment the factorial machine with instructions to
;; initialize the stack and print the statistics. You may want to also
;; modify the machine so that it repeatedly reads a value for n,
;; computes the factorial, and prints the result (as we did for the
;; GCD machine in *Note Figure 5-4::), so that you will not have to
;; repeatedly invoke `get-register-contents',
;; `set-register-contents!', and `start'.

(define factorial
  (make-machine
   '(n continue product)
   (list (list '= =) (list '- -) (list '* *))
   '(controller
       (assign continue (label fact-done))
     fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
     after-fact
       (restore n)
       (restore continue)
       (assign product (op *) (reg n) (reg product))
       (goto (reg continue))
     base-case
       (assign product (const 1))
       (goto (reg continue))
     fact-done)))

(assert-machine factorial '((n 1)) 'product 1)
(assert-machine factorial '((n 2)) 'product 2)
(assert-machine factorial '((n 3)) 'product 6)
(assert-machine factorial '((n 4)) 'product 24)
(assert-machine factorial '((n 5)) 'product 120)

;; 1 = ((max-depth 0) (number-pushes 0))
;; 2 = ((max-depth 2) (number-pushes 2))
;; 3 = ((max-depth 4) (number-pushes 4))
;; 4 = ((max-depth 6) (number-pushes 6))
;; 5 = ((max-depth 8) (number-pushes 8))
;;
;; max-depth = 2*(n-1)
;; pushes    = 2*(n-1)
