#!/usr/bin/env csi -s

(require rackunit)
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

;; (define (spread-arguments a b operation)
;;   (preserving '(env)
;;               (compile a 'arg1 'next)
;;               (preserving '(arg1)
;;                           (compile b 'arg2 'next)
;;                           operation)))

;;   b. For each of the primitive procedures `=', `*', `-', and `+',
;;      write a code generator that takes a combination with that
;;      operator, together with a target and a linkage descriptor, and
;;      produces code to spread the arguments into the registers and
;;      then perform the operation targeted to the given target with
;;      the given linkage. You need only handle expressions with two
;;      operands. Make `compile' dispatch to these code generators.

(let ((expected '((assign arg1 (const 1))
                  (save arg1)
                  (assign arg1 (const 2))
                  (assign arg2 (const 3))
                  (assign arg2 (op =) (reg arg1) (reg arg2))
                  (restore arg1)
                  (assign val (op =) (reg arg1) (reg arg2)))))
  (test expected (statements (compile '(= 1 (= 2 3)) 'val 'next)))

  (assert-compile 7 '(+ 1 (* 2 3))))

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

;;   d. Extend your code generators for `+' and `*' so that they can
;;      handle expressions with arbitrary numbers of operands. An
;;      expression with more than two operands will have to be
;;      compiled into a sequence of operations, each with only two
;;      inputs.

(let ((expected '((assign arg1 (const 1))
                  (assign arg2 (const 2))
                  (assign arg1 (op *) (reg arg1) (reg arg2))
                  (assign arg2 (const 3))
                  (assign arg1 (op *) (reg arg1) (reg arg2))
                  (assign arg2 (const 4))
                  (assign arg1 (op *) (reg arg1) (reg arg2))
                  (assign arg2 (const 5))
                  (assign val  (op *) (reg arg1) (reg arg2)))))
  (test expected (statements (compile '(* 1 2 3 4 5) 'val 'next)))

  (assert-compile 120 '(* 1 2 3 4 5)))
