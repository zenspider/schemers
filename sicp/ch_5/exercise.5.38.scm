#!/usr/bin/env csi -s

(use test)
(require-library compiler)
(import compiler)

;;; Exercise 5.38

;; Our compiler is clever about avoiding unnecessary stack operations,
;; but it is not clever at all when it comes to compiling calls to the
;; primitive procedures of the language in terms of the primitive
;; operations supplied by the machine. For example, consider how much
;; code is compiled to compute `(+ a 1)': The code sets up an argument
;; list in `argl', puts the primitive addition procedure (which it
;; finds by looking up the symbol `+' in the environment) into `proc',
;; and tests whether the procedure is primitive or compound. The
;; compiler always generates code to perform the test, as well as code
;; for primitive and compound branches (only one of which will be
;; executed). We have not shown the part of the controller that
;; implements primitives, but we presume that these instructions make
;; use of primitive arithmetic operations in the machine's data paths.
;; Consider how much less code would be generated if the compiler
;; could "open-code" primitives--that is, if it could generate code to
;; directly use these primitive machine operations. The expression `(+
;; a 1)' might be compiled into something as simple as (1)
;;
;;      (assign val (op lookup-variable-value) (const a) (reg env))
;;      (assign val (op +) (reg val) (const 1))
;;
;; In this exercise we will extend our compiler to support open coding
;; of selected primitives. Special-purpose code will be generated for
;; calls to these primitive procedures instead of the general
;; procedure-application code. In order to support this, we will
;; augment our machine with special argument registers `arg1' and
;; `arg2'. The primitive arithmetic operations of the machine will
;; take their inputs from `arg1' and `arg2'. The results may be put
;; into `val', `arg1', or `arg2'.
;;
;; The compiler must be able to recognize the application of an
;; open-coded primitive in the source program. We will augment the
;; dispatch in the `compile' procedure to recognize the names of these
;; primitives in addition to the reserved words (the special forms) it
;; currently recognizes.(2) For each special form our compiler has a
;; code generator. In this exercise we will construct a family of code
;; generators for the open-coded primitives.
;;
;;   a. The open-coded primitives, unlike the special forms, all need
;;      their operands evaluated. Write a code generator
;;      `spread-arguments' for use by all the open-coding code
;;      generators. `Spread-arguments' should take an operand list and
;;      compile the given operands targeted to successive argument
;;      registers. Note that an operand may contain a call to an
;;      open-coded primitive, so argument registers will have to be
;;      preserved during operand evaluation.

;; FIX: they can't be asking for this... too easy. no preserving?
;; (define (spread-arguments a b)
;;   (list (compile a 'arg1 'next)
;;         (compile b 'arg2 'next)))

;;   b. For each of the primitive procedures `=', `*', `-', and `+',
;;      write a code generator that takes a combination with that
;;      operator, together with a target and a linkage descriptor, and
;;      produces code to spread the arguments into the registers and
;;      then perform the operation targeted to the given target with
;;      the given linkage. You need only handle expressions with two
;;      operands. Make `compile' dispatch to these code generators.

;; (let ((expected '((assign arg1 (const 1))
;;                   (save arg1)
;;                   (assign arg1 (const 2))
;;                   (assign arg2 (const 3))
;;                   (assign arg2 (op =) (reg arg1) (reg arg2))
;;                   (restore arg1)
;;                   (assign val (op =) (reg arg1) (reg arg2)))))
;;   (test expected (statements (compile '(= 1 (= 2 3)) 'val 'next)))
;;
;;   (assert-compile 7 '(+ 1 (* 2 3))))

;;   c. Try your new compiler on the `factorial' example. Compare the
;;      resulting code with the result produced without open coding.

;; before: 109 lines long, 225 operations executed in trace.
;; after: 56 lines long, 132 operations executed in trace.
;; ~51% the original size and ~59% the number of operations executed.

(assert-compile 120 '(begin
                       (define (factorial1 n)
                         (if (= n 1)
                             1
                             (* (factorial1 (- n 1)) n)))
                       (factorial1 5)))

(assert-compile 120 '(begin
                       (define (factorial2 n)
                         (define (iter product counter)
                           (if (> counter n)
                               product
                               (iter (* counter product)
                                     (+ counter 1))))
                         (iter 1 1))
                       (factorial2 5)))

;; '(;; start compiling factorial2
;;   (assign val (op make-compiled-procedure) (label entry1) (reg env))
;;   (goto (label after-lambda2))
;;
;;   entry1
;;
;;   ;; start compiling iter
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;;   (assign val (op make-compiled-procedure) (label entry3) (reg env))
;;   (goto (label after-lambda4))
;;
;;   entry3
;;
;;   (assign env (op compiled-procedure-env) (reg proc))
;;   (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;;   (save continue)
;;   (save env)
;;
;;   ;;
;;   (assign proc (op lookup-variable-value) (const >) (reg env))
;;   (assign val (op lookup-variable-value) (const n) (reg env))
;;   (assign argl (op list) (reg val))
;;   (assign val (op lookup-variable-value) (const counter) (reg env))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch8))
;;
;;   compiled-branch9
;;
;;   (assign continue (label after-call10))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch8
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;
;;   after-call10
;;
;;   (restore env)
;;   (restore continue)
;;   (test (op false?) (reg val))
;;   (branch (label false-branch6))
;;
;;   true-branch5
;;
;;   (assign val (op lookup-variable-value) (const product) (reg env))
;;   (goto (reg continue))
;;
;;   false-branch6
;;
;;   (assign proc (op lookup-variable-value) (const iter) (reg env))
;;   (assign arg1 (op lookup-variable-value) (const counter) (reg env))
;;   (assign arg2 (const 1))
;;   (assign val (op +) (reg arg1) (reg arg2))
;;   (assign argl (op list) (reg val))
;;   (assign arg1 (op lookup-variable-value) (const counter) (reg env))
;;   (assign arg2 (op lookup-variable-value) (const product) (reg env))
;;   (assign val (op *) (reg arg1) (reg arg2))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch11))
;;
;;   compiled-branch12
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch11
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (goto (reg continue))
;;
;;   after-call13
;;   after-if7
;;   after-lambda4                         ; done defining iter
;;
;;   (perform (op define-variable!) (const iter) (reg val) (reg env))
;;   (assign val (const ok))
;;   (goto (reg continue))
;;   (assign proc (op lookup-variable-value) (const iter) (reg env))
;;   (assign val (const 1))
;;   (assign argl (op list) (reg val))
;;   (assign val (const 1))
;;   (assign argl (op cons) (reg val) (reg argl))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch14))
;;
;;   compiled-branch15
;;
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch14
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   (goto (reg continue))
;;
;;   after-call16
;;   after-lambda2                         ; done defining factorial
;;
;;   (perform (op define-variable!) (const factorial2) (reg val) (reg env))
;;   (assign val (const ok))
;;
;;   ;; (factorial2 5)
;;   (assign proc (op lookup-variable-value) (const factorial2) (reg env))
;;   (assign val (const 5))
;;   (assign argl (op list) (reg val))
;;   (test (op primitive-procedure?) (reg proc))
;;   (branch (label primitive-branch17))
;;
;;   compiled-branch18
;;
;;   (assign continue (label after-call19))
;;   (assign val (op compiled-procedure-entry) (reg proc))
;;   (goto (reg val))
;;
;;   primitive-branch17
;;
;;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;   after-call19)

;; [1] (branch (label external-entry))
;; [2] (perform (op initialize-stack))
;; [3] (assign env (op get-global-environment))
;; [4] (test (op unassigned?) (reg exp))
;; [5] (branch (label execute-external-entry))
;; [6] (assign continue (label print-result))
;; [7] (goto (reg val))
;; [8] (assign val (op make-compiled-procedure) (label entry1) (reg env))
;; [9] (goto (label after-lambda2))
;; [10] (perform (op define-variable!) (const factorial2) (reg val) (reg env))
;; [11] (assign val (const ok))
;; [12] (assign proc (op lookup-variable-value) (const factorial2) (reg env))
;; [13] (assign val (const 5))
;; [14] (assign argl (op list) (reg val))
;; [15] (test (op primitive-procedure?) (reg proc))
;; [16] (branch (label primitive-branch17))
;; [17] (assign continue (label after-call19))
;; [18] (assign val (op compiled-procedure-entry) (reg proc))
;; [19] (goto (reg val))
;; [20] (assign env (op compiled-procedure-env) (reg proc))
;; [21] (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; [22] (assign val (op make-compiled-procedure) (label entry3) (reg env))
;; [23] (goto (label after-lambda4))
;; [24] (perform (op define-variable!) (const iter) (reg val) (reg env))
;; [25] (assign val (const ok))
;; [26] (goto (reg continue))


;;   d. Extend your code generators for `+' and `*' so that they can
;;      handle expressions with arbitrary numbers of operands. An
;;      expression with more than two operands will have to be
;;      compiled into a sequence of operations, each with only two
;;      inputs.
