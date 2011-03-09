#!/usr/bin/env csi -s

(use test)

;;; Exercise 4.8

;; "Named `let'" is a variant of `let' that has the
;; form
;;
;;      (let <VAR> <BINDINGS> <BODY>)
;;
;; The <BINDINGS> and <BODY> are just as in ordinary `let', except
;; that <VAR> is bound within <BODY> to a procedure whose body is
;; <BODY> and whose parameters are the variables in the <BINDINGS>.
;; Thus, one can repeatedly execute the <BODY> by invoking the
;; procedure named <VAR>.  For example, the iterative Fibonacci
;; procedure (section *Note 1-2-2::) can be rewritten using named
;; `let' as follows:
;;
;;      (define (fib n)
;;        (let fib-iter ((a 1)
;;                       (b 0)
;;                       (count n))
;;          (if (= count 0)
;;              b
;;              (fib-iter (+ a b) a (- count 1)))))
;;
;; Modify `let->combination' of *Note Exercise 4-6:: to also support
;; named `let'.
