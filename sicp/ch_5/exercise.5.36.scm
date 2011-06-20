#!/usr/bin/env csi -s

(use test)

;;; Exercise 5.36

;; What order of evaluation does our compiler produce for operands of
;; a combination? Is it left-to-right, right-to-left, or some other
;; order? Where in the compiler is this order determined? Modify the
;; compiler so that it produces some other order of evaluation. (See
;; the discussion of order of evaluation for the explicit-control
;; evaluator in section *Note 5-4-1::.) How does changing the order of
;; operand evaluation affect the efficiency of the code that
;; constructs the argument list?

(require-library compiler)
(import compiler)

;; A: right to left, as defined by the reversal of the args when
;; building up argl for an apply. You can see it in the generated code
;; below where it starts with calling y, not x.

;; (statements (compile '(+ (x) (y)) 'val 'return))
;; =>
;; ((assign proc (op lookup-variable-value) (const +) (reg env))
;;   (save continue)
;;   (save proc)
;;   (save env)
;;   (assign proc (op lookup-variable-value) (const y) (reg env))
;;   (assign argl (const ()))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch4))
;;
;;   compiled-branch5
;;
;;   (assign continue (label after-call6))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch4
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;
;;   after-call6
;;
;;   (assign argl (op list) (reg val))
;;   (restore env)
;;   (save argl)
;;   (assign proc (op lookup-variable-value) (const x) (reg env))
;;   (assign argl (const ()))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch1))
;;
;;   compiled-branch2
;;
;;   (assign continue (label after-call3))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch1
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;
;;   after-call3
;;
;;   (restore argl)
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (restore proc)
;;   (restore continue)
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch7))
;;
;;   compiled-branch8
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch7
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (goto (reg continue))
;;
;;   after-call9)