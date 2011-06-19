#!/usr/bin/env csi -s

(use test)
(require-library compiler)
(import compiler)

(assert-compile 11 '(+ 1 (* 2 3) 4))

(assert-assemble
 11
 '((assign proc (op lookup-variable-value) (const +) (reg env))
   (save proc)
   (assign val (const 4))
   (assign argl (op list) (reg val))
   (save argl)                                  ; NOTE: added to fix bug
   (assign proc (op lookup-variable-value) (const *) (reg env))
   (assign val (const 3))
   (assign argl (op list) (reg val))            ; FIX: BUG! drops (4)
   (assign val (const 2))
   (assign argl (op cons) (reg val) (reg argl))
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch1))

   compiled-branch2

   (assign continue (label after-call3))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))

   primitive-branch1

   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

   after-call3

   (restore argl)                               ; NOTE: added to fix bug
   (assign argl (op cons) (reg val) (reg argl)) ; NOTE: switched to cons
   (assign val (const 1))
   (assign argl (op cons) (reg val) (reg argl))
   (restore proc)
   (test (op primitive-procedure?) (reg proc))
   (branch (label primitive-branch4))

   compiled-branch5

   (assign continue (label after-call6))
   (assign val (op compiled-procedure-entry) (reg proc))
   (goto (reg val))

   primitive-branch4

   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

   after-call6))
