#!/usr/bin/env csi -s

(use test)
(require-library compiler)
(import compiler)

;;; Exercise 5.34

;; Compile the iterative factorial procedure
;;
;;      (define (factorial n)
;;        (define (iter product counter)
;;          (if (> counter n)
;;              product
;;              (iter (* counter product)
;;                    (+ counter 1))))
;;        (iter 1 1))
;;
;; Annotate the resulting code, showing the essential difference
;; between the code for iterative and recursive versions of
;; `factorial' that makes one process build up stack space and the
;; other run in constant stack space.

(assert-compile 120 '(begin
                       (define (factorial-r n)
                         (if (= n 1)
                             1
                             (* (factorial-r (- n 1)) n)))
                       (factorial-r 5)))

(assert-compile 120 '(begin
                       (define (factorial-i n)
                         (define (iter product counter)
                           (if (> counter n)
                               product
                               (iter (* counter product)
                                     (+ counter 1))))
                         (iter 1 1))
                       (factorial-i 5)))

;; '((assign val (op make-compiled-procedure) (label entry1) (reg env))
;;   (goto (label after-lambda2))
;;   entry1
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;
;;
;;   ;; iterative: make inner function (iter product counter)
;;   (assign val (op make-compiled-procedure) (label entry3) (reg env))
;;   (goto (label after-lambda4))
;;   entry3
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment)
;;           (const (product counter)) (reg argl) (reg env))
;;   ;; recursive: nothing extra
;;   ;; end
;;
;;
;;   (save continue)                        ; i=x:[x]; r=x:[x]
;;   (save env)
;;
;;
;;   ;; iterative: (> <counter> n)
;;   (assign proc (op lookup-variable-value) (const >) (reg env))
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   ;; recursive: (= <n> 1)
;;   (assign proc (op lookup-variable-value) (const =) (reg env))
;;   (assign val (const 1))
;;   ;; end
;;
;;
;;   (assign argl (op list) (reg val))
;;
;;
;;   ;; iterative: <counter>
;;   (assign val (op lookup-variable-value) (const counter) (reg env))
;;   ;; recursive: <n>
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   ;; end
;;
;;
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;
;;
;;   ;; iterative: boring counter differences
;;   (branch (label primitive-branch8))
;;   compiled-branch9
;;   (assign continue (label after-call10))
;;   ;; recursive
;;   (branch (label primitive-branch6))
;;   compiled-branch7
;;   (assign continue (label after-call8))
;;   ;; end
;;
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;
;;   ;; iterative: boring counter differences
;;   primitive-branch8
;;   ;; recursive
;;   primitive-branch6
;;   ;; end
;;
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;
;;
;;   ;; iterative: boring counter differences
;;   after-call10
;;   ;; recursive
;;   after-call8
;;   ;; end
;;
;;
;;   (restore env)
;;   (restore continue)
;;   (test (op false?) (reg val))
;;
;;
;;   ;; iterative: if terminated return product
;;   (branch (label false-branch6))
;;   true-branch5
;;   (assign val (op lookup-variable-value) (const product) (reg env))
;;   ;; recursive: if terminated return 1
;;   (branch (label false-branch4))
;;   true-branch3
;;   (assign val (const 1))
;;   ;; end
;;
;;
;;   (goto (reg continue))                  ; and exit
;;
;;
;;   ;; iterative: otherwise, prepare to run iter again
;;   false-branch6
;;   (assign proc (op lookup-variable-value) (const iter) (reg env))
;;   ;; recursive: otherwise prepare to run * again
;;   false-branch4
;;   (assign proc (op lookup-variable-value) (const *) (reg env))
;;   ;; end
;;
;;
;;   (save continue)
;;   (save proc)                            ; i=iter:[iter]; r=*:[*]
;;
;;
;;   ;; iterative: (+ <counter> <1>)
;;   (save env)
;;   (assign proc (op lookup-variable-value) (const +) (reg env))
;;   ;; recursive: (* (factorial <(- n 1)> n)
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   (assign argl (op list) (reg val))
;;   (save argl)
;;   (assign proc (op lookup-variable-value) (const factorial) (reg env))
;;   (save proc)
;;   (assign proc (op lookup-variable-value) (const -) (reg env))
;;   ;; end                                 ; i=+:[iter]; r=-:[factorial, *]
;;
;;
;;   (assign val (const 1))
;;   (assign argl (op list) (reg val))
;;
;;
;;   ;; iterative: <counter>
;;   (assign val (op lookup-variable-value) (const counter) (reg env))
;;   ;; recursive: <n> (inner to (- n 1))
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   ;; end
;;
;;
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;
;;
;;   ;; iterative: boring counter differences
;;   (branch (label primitive-branch14))
;;   compiled-branch15
;;   (assign continue (label after-call16))
;;   ;; recursive
;;   (branch (label primitive-branch9))
;;   compiled-branch10
;;   (assign continue (label after-call11))
;;   ;; end
;;
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;
;;   ;; iterative: boring counter differences
;;   primitive-branch14
;;   ;; recursive
;;   primitive-branch9
;;   ;; end
;;
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;
;;
;;   ;; iterative: boring counter differences
;;   after-call16
;;   ;; recursive
;;   after-call11
;;   ;; end
;;
;;
;;   (assign argl (op list) (reg val))
;;
;;
;;   ;; iterative: (* counter product)
;;   (restore env)
;;   (save argl)
;;   (assign proc (op lookup-variable-value) (const *) (reg env))
;;   (assign val (op lookup-variable-value) (const product) (reg env))
;;   (assign argl (op list) (reg val))
;;   (assign val (op lookup-variable-value) (const counter) (reg env))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   ;; recursive: back to factorial
;;   (restore proc)                            ; i=*:[iter]; r=factorial:[*]
;;   ;; end
;;
;;
;;   (test (op primitive-procedure?) (reg proc))
;;
;;
;;   ;; iterative: boring counter differences
;;   (branch (label primitive-branch11))
;;   compiled-branch12
;;   (assign continue (label after-call13))
;;   ;; recursive
;;   (branch (label primitive-branch12))
;;   compiled-branch13
;;   (assign continue (label after-call14))
;;   ;; end
;;
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;
;;   ;; iterative: boring counter differences
;;   primitive-branch11
;;   ;; recursive
;;   primitive-branch12
;;   ;; end
;;
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;
;;
;;   ;; iterative: boring counter differences
;;   after-call13
;;   ;; recursive
;;   after-call14
;;   ;; end
;;
;;
;;   (restore argl)
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (restore proc)                            ; i=iter:[]; r=*:[]
;;   (restore continue)
;;   (test (op primitive-procedure?) (reg proc))
;;
;;
;;   ;; iterative
;;   (branch (label primitive-branch17))
;;   compiled-branch18
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))                       ; TADA! tail-recursive cycle!
;;   primitive-branch17
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (goto (reg continue))                  ; here too
;;   after-call19
;;   after-if7
;;   after-lambda4
;;   ;; define inner-function: (iter product counter)
;;   (perform (op define-variable!) (const iter) (reg val) (reg env))
;;   (assign val (const ok))
;;   (goto (reg continue))
;;   ;; call inner-function: (iter 1 1)
;;   (assign proc (op lookup-variable-value) (const iter) (reg env))
;;   (assign val (const 1))
;;   (assign argl (op list) (reg val))
;;   (assign val (const 1))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch20))
;;   compiled-branch21
;;   ;; recursive
;;   (branch (label primitive-branch15))
;;   compiled-branch16
;;   ;; end
;;
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;
;;   ;; iterative: boring counter differences
;;   primitive-branch20
;;   ;; recursive
;;   primitive-branch15
;;   ;; end
;;
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (goto (reg continue))
;;
;;
;;   ;; iterative: boring counter differences
;;   after-call22
;;   ;; recursive
;;   after-call17
;;   after-if5
;;   ;; end
;;
;;
;;   after-lambda2
;;   (perform (op define-variable!) (const factorial) (reg val) (reg env))
;;   (assign val (const ok)))
