#!/usr/bin/env csi -s

(use test)
(require-library machine)
(import machine)
(require-library ec-eval)
(import ec-eval)

;;; Exercise 5.16

;; Augment the simulator to provide for "instruction tracing". That
;; is, before each instruction is executed, the simulator should print
;; the text of the instruction. Make the machine model accept
;; `trace-on' and `trace-off' messages to turn tracing on and off.

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
(factorial 'trace-on)
(assert-machine factorial '((n 2)) 'product 2)
(factorial 'trace-off)
(assert-machine factorial '((n 3)) 'product 6)
(assert-machine factorial '((n 4)) 'product 24)
(assert-machine factorial '((n 5)) 'product 120)
