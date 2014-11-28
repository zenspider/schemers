(require rackunit)

;;; Exercise 4.1:

;; Notice that we cannot tell whether the metacircular evaluator
;; evaluates operands from left to right or from right to left. Its
;; evaluation order is inherited from the underlying Lisp: If the
;; arguments to `cons' in `list-of-values' are evaluated from left to
;; right, then `list-of-values' will evaluate operands from left to
;; right; and if the arguments to `cons' are evaluated from right to
;; left, then `list-of-values' will evaluate operands from right to
;; left.
;;
;; Write a version of `list-of-values' that evaluates operands from
;; left to right regardless of the order of evaluation in the
;; underlying Lisp. Also write a version of `list-of-values' that
;; evaluates operands from right to left.

(define null '())
(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (list-of-values-l2r exps env)
  (if (no-operands? exps) null
      (let ((l (eval (first-operand exps) env)))
        (cons l (list-of-values-l2r (rest-operands exps) env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps) null
      (let ((r (list-of-values-r2l (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) r))))

(test-group "4.01"
            (set! a 0)
            (define (succ-a) (set! a (+ a 1)) a)
            (test '(1 2 3) (list-of-values-l2r '((succ-a) (succ-a) (succ-a))
                                               (interaction-environment)))
            (set! a 0)
            (test '(3 2 1) (list-of-values-r2l '((succ-a) (succ-a) (succ-a))
                                               (interaction-environment))))
