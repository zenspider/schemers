#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

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

(define let-body   cddr)
(define let-params cadr)

(define (let-args exp)
  (map car (cadr exp)))

(define (let-vals exp)
  (map cadr (cadr exp)))

(define (let->call args vals body)
  (append (list (append (list 'lambda args) body)) vals))

(define (make-define name args body)
  (append (cons 'define (list (cons name args))) body))

(define (let->defun exp)
  (let ((name (let-params exp))
        (args (map car  (caddr exp)))
        (vals (map cadr (caddr exp)))
        (body (cdddr exp)))
    (make-begin (list (make-define name args body) (cons name vals)))))

(define (let->combination exp)
  (if (symbol? (let-params exp))
      (let->defun exp)
      (let->call (let-args exp) (let-vals exp) (let-body exp))))

(test
 '(begin
    (define (fib-iter a b count)
      (if (= count 0)
          b
          (fib-iter (+ a b) a (- count 1))))
    (fib-iter 1 0 n))
 (let->combination  '(let fib-iter ((a 1)
                                    (b 0)
                                    (count n))
                       (if (= count 0)
                           b
                           (fib-iter (+ a b) a (- count 1))))))

