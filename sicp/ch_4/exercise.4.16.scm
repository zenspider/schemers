#!/usr/bin/env csi -s

(use test)
(require-library eval)
(import eval)

;;; Exercise 4.16

;; In this exercise we implement the method just described for
;; interpreting internal definitions. We assume that the evaluator
;; supports `let' (see *Note Exercise 4-6::).
;;
;;   a. Change `lookup-variable-value' (section *Note 4-1-3::) to
;;      signal an error if the value it finds is the symbol
;;      `*unassigned*'.

(define (lookup-variable-value var env)
  (let ((pair (find-pair-in-env env var)))
    (if (null? pair)
        (error "Unbound variable" var)
        (let ((val (cadr pair)))
          (if (eq? '*unassigned* val)
              (error "Variable is undefined: " var)
              val)))))

;;   b. Write a procedure `scan-out-defines' that takes a procedure
;;      body and returns an equivalent one that has no internal
;;      definitions, by making the transformation described above.

(define (scan-out-defines exp)
  (define (define? x) (tagged-list? x 'define))
  (define (body->letrec body)
    (let-values (((defines code) (partition define? body)))
      (let* ((undef (list 'quote '*unassigned*))
             (decls (map (lambda (x) (list (cadr x) undef)) defines))
             (defns (map (lambda (x) (cons 'set! (cdr x))) defines)))
        (append (list 'let decls) (append defns code)))))
  (body->letrec exp))

;;   c. Install `scan-out-defines' in the interpreter, either in
;;      `make-procedure' or in `procedure-body' (see section *Note
;;      4-1-3::). Which place is better? Why?

;; A: make-procedure means the transform is only done once, not on every apply.

(test-group "4.16"
  (define input '(lambda (a b c)
                   (define u e1)
                   (define v e2)
                   e3))

  (define exp-let '(lambda (a b c)
                     (let ((u '*unassigned*)
                           (v '*unassigned*))
                       (set! u e1)
                       (set! v e2)
                       e3)))

  (test (caddr exp-let) (scan-out-defines (cddr input))))