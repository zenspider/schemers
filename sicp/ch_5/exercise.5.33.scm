#!/usr/bin/env csi -s

(use test)

(require-library compiler)
(import compiler)

;;; Exercise 5.33

;; Consider the following definition of a factorial procedure, which
;; is slightly different from the one given above:

(define (factorial-alt n)
  (if (= n 1)
      1
      (* n (factorial-alt (- n 1)))))

;; Compile this procedure and compare the resulting code with that
;; produced for `factorial'. Explain any differences you find. Does
;; either program execute more efficiently than the other?

;; ((env)                                  ; needs
;;  (val x)                                ; modifies
;;  ;; statements:
;;   ((assign val (op make-compiled-procedure) (label entry1) (reg env))
;;    (goto (label after-lambda2))
;;    entry1
;;    (assign env (op compiled-procedure-env) (reg proc))
;;    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;    (save continue)
;;    (save env)
;;    (assign proc (op lookup-variable-value) (const =) (reg env))
;;    (assign val (const 1))
;;    (assign argl (op list (reg val)))
;;    (assign val (op lookup-variable-value) (const n) (reg env))
;;    (assign argl (op cons) (reg val) (reg argl))
;;    (test (op primitive-procedure?) (reg proc))
;;    (branch (label primitive-branch6))
;;    compiled-branch7
;;    (assign continue (label after-call8))
;;    (assign val (op copmiled-procedure-entry) (reg proc))
;;    (goto (reg val))
;;    primitive-branch6
;;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;    after-call8
;;    (restore env)
;;    (restore continue)
;;    (test (op false?) (reg val))
;;    (branch (label false-branch4))
;;    true-branch3
;;    (assign val (const 1))
;;    (goto (reg continue))
;;    false-branch4
;;    (assign proc (op lookup-variable-value) (const *) (reg env))
;;    (save continue)
;;    (save proc)
;;
;;
;;    ;; factorial
;;    (assign val (op lookup-variable-value) (const n) (reg env))
;;    (assign argl (op list (reg val)))
;;    (save argl)
;;    (assign proc (op lookup-variable-value) (const factorial) (reg env))
;;
;;    ;; factorial-alt
;;    (save env)
;;    (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
;;
;;
;;    (save proc)
;;    (assign proc (op lookup-variable-value) (const -) (reg env))
;;    (assign val (const 1))
;;    (assign argl (op list (reg val)))
;;    (assign val (op lookup-variable-value) (const n) (reg env))
;;    (assign argl (op cons) (reg val) (reg argl))
;;    (test (op primitive-procedure?) (reg proc))
;;    (branch (label primitive-branch9))
;;    compiled-branch10
;;    (assign continue (label after-call11))
;;    (assign val (op copmiled-procedure-entry) (reg proc))
;;    (goto (reg val))
;;    primitive-branch9
;;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;    after-call11
;;    (assign argl (op list (reg val)))
;;    (restore proc)
;;    (test (op primitive-procedure?) (reg proc))
;;    (branch (label primitive-branch12))
;;    compiled-branch13
;;    (assign continue (label after-call14))
;;    (assign val (op copmiled-procedure-entry) (reg proc))
;;    (goto (reg val))
;;    primitive-branch12
;;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;    after-call14
;;
;;
;;    ;; factorial
;;    (restore argl)
;;
;;    ;; factorial-alt
;;    (assign argl (op list (reg val)))
;;    (restore env)
;;    (assign val (op lookup-variable-value) (const n) (reg env))
;;
;;
;;    (assign argl (op cons) (reg val) (reg argl))
;;    (restore proc)
;;    (restore continue)
;;    (test (op primitive-procedure?) (reg proc))
;;    (branch (label primitive-branch15))
;;    compiled-branch16
;;    (assign val (op compiled-procedure-entry) (reg proc))
;;    (goto (reg val))
;;    primitive-branch15
;;    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;    (goto (reg continue))
;;    after-call17
;;    after-if5
;;    after-lambda2
;;
;;
;;    ;; factorial
;;    (perform (op define-variable!) (const factorial) (reg val) (reg env))
;;
;;    ;; factorial-alt
;;    (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
;;
;;
;;    (assign x (const ok))))

(test-group "compiler functionality"

  (test-group "compile-linkage"
    (test '(()         () ())                      (compile-linkage 'next))
    (test '((continue) () ((goto (reg continue)))) (compile-linkage 'return))
    (test '(()         () ((goto (label blah))))   (compile-linkage 'blah)))

  (test-group "compile-self-evaluating"
    (define (test-compile-lit code link expected)
      (let ((regs (if (eq? link 'return) '(continue) '())))
        (let ((expect `(,regs (x) ,expected)))
          (test expect (compile code 'x link)))))

    (test-compile-lit '42 'next   '((assign x (const 42))))
    (test-compile-lit '42 'return '((assign x (const 42))
                                    (goto (reg continue))))
    (test-compile-lit '42 'xxxx   '((assign x (const 42))
                                    (goto (label xxxx)))))

  (test-group "compile-variable"
    (define (test-compile-var code link expected)
      (let ((regs (if (eq? link 'return) '(env continue) '(env))))
        (let ((expect `(,regs (x) ,expected)))
          (test expect (compile code 'x link)))))

    (test-compile-var 'var 'next   '((assign x
                                             (op lookup-variable-value)
                                             (const var) (reg env))))
    (test-compile-var 'var 'return '((assign x
                                             (op lookup-variable-value)
                                             (const var) (reg env))
                                     (goto (reg continue))))
    (test-compile-var 'var 'xxxx   '((assign x
                                             (op lookup-variable-value)
                                             (const var) (reg env))
                                     (goto (label xxxx)))))

  (test-group "assert-compile"
    (assert-compile 'x 3
                    '(define x (+ 1 2)))

    (assert-compile 3
                    '(begin
                       (define x (+ 1 2))
                       x))


    (assert-compile 120
                    '(begin
                       (define (factorial n)
                                (if (= n 1)
                                    1
                                    (* (factorial (- n 1)) n)))
                       (factorial 5)))))
