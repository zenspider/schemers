#!/usr/bin/env csi -s

(require rackunit)

;;; Exercise 4.7

;; `Let*' is similar to `let', except that the
;; bindings of the `let' variables are performed sequentially from
;; left to right, and each binding is made in an environment in which
;; all of the preceding bindings are visible.  For example
;;
;;      (let* ((x 3)
;;             (y (+ x 2))
;;             (z (+ x y 5)))
;;        (* x z))
;;
;; returns 39.  Explain how a `let*' expression can be rewritten as a
;; set of nested `let' expressions, and write a procedure
;; `let*->nested-lets' that performs this transformation.  If we have
;; already implemented `let' (*Note Exercise 4-6::) and we want to
;; extend the evaluator to handle `let*', is it sufficient to add a
;; clause to `eval' whose action is
;;
;;      (eval (let*->nested-lets exp) env)
;;
;; or must we explicitly expand `let*' in terms of non-derived
;; expressions?

(define let-params cadr)
(define let-body   cddr)

(define (let*->nested-lets exp)
  (define (let*->let params body)
    (if (null? (cdr params))
        (append (list 'let (list (car params)))
                body)
        (list 'let (list (car params))
              (let*->let (cdr params) body))))
  (let*->let (let-params exp) (let-body exp)))

(test '(let ((x 3))
         (let ((y (+ x 2)))
           (let ((z (+ x y 5)))
             (* x z)
             42)))

      (let*->nested-lets '(let* ((x 3)
                                 (y (+ x 2))
                                 (z (+ x y 5)))
                            (* x z)
                            42)))
