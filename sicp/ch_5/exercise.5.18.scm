#!/usr/bin/env csi -s

(require rackunit)
(require-library machine)
(import machine)
(require-library ec-eval)
(import ec-eval)

;;; Exercise 5.18

;; Modify the `make-register' procedure of section *Note 5-2-1:: so
;; that registers can be traced. Registers should accept messages that
;; turn tracing on and off. When a register is traced, assigning a
;; value to the register should print the name of the register, the
;; old contents of the register, and the new contents being assigned.
;; Extend the interface to the machine model to permit you to turn
;; tracing on and off for designated machine registers.

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
(trace-register-on factorial 'product)
(assert-machine factorial '((n 5)) 'product 120)
(trace-register-off factorial 'product)
