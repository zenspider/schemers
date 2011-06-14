#!/usr/bin/env csi -s

(use test)
(require-library machine)
(import machine)
(require-library ec-eval)
(import ec-eval)

;;; Exercise 5.15

;; Add counting "instruction counting" to the register machine
;; simulation. That is, have the machine model keep track of the
;; number of instructions executed. Extend the machine model's
;; interface to accept a new message that prints the value of the
;; instruction count and resets the count to zero.

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

;; ic = 11*n + 6

(test-group "5.15"
  (define (test-count machine count)
    (test (list 'instruction-count count)
          (assoc 'instruction-count (machine 'statistics))))

  (assert-machine factorial '((n 1)) 'product 1)
  (test-count factorial 6)
  (assert-machine factorial '((n 2)) 'product 2)
  (test-count factorial 17)
  (assert-machine factorial '((n 3)) 'product 6)
  (test-count factorial 28)
  (assert-machine factorial '((n 4)) 'product 24)
  (test-count factorial 39)
  (assert-machine factorial '((n 5)) 'product 120)
  (test-count factorial 50))
