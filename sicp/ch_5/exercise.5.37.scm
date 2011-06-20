#!/usr/bin/env csi -s

(use test)
(require-library compiler)
(import compiler)

;;; Exercise 5.37

;; One way to understand the compiler's `preserving' mechanism for
;; optimizing stack usage is to see what extra operations would be
;; generated if we did not use this idea. Modify `preserving' so that
;; it always generates the `save' and `restore' operations. Compile
;; some simple expressions and identify the unnecessary stack
;; operations that are generated. Compare the code to that generated
;; with the `preserving' mechanism intact.

;; no

(statements (compile '(+ (x) (y)) 'val 'return))

;; Lines commented out are from the inefficient preserving function:

'(;; (save continue)
  ;; (save env)
  ;; (save continue)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  ;; (restore continue)
  ;; (restore env)
  ;; (restore continue)
  (save continue)
  (save proc)
  (save env)
  ;; (save continue)
  ;; (save env)
  ;; (save continue)
  (assign proc (op lookup-variable-value) (const y) (reg env))
  ;; (restore continue)
  ;; (restore env)
  ;; (restore continue)
  ;; (save continue)
  ;; (save proc)
  (assign argl (const ()))
  ;; (restore proc)
  ;; (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch4))

  compiled-branch5

  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch4

  ;; (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  ;; (restore continue)

  after-call6

  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  ;; (save continue)
  ;; (save env)
  ;; (save continue)
  (assign proc (op lookup-variable-value) (const x) (reg env))
  ;; (restore continue)
  ;; (restore env)
  ;; (restore continue)
  ;; (save continue)
  ;; (save proc)
  (assign argl (const ()))
  ;; (restore proc)
  ;; (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch1))

  compiled-branch2

  (assign continue (label after-call3))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch1

  ;; (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  ;; (restore continue)

  after-call3

  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch7))

  compiled-branch8

  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch7

  ;; (save continue)
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  ;; (restore continue)
  (goto (reg continue))

  after-call9)
